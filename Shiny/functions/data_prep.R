# ===================================
# functions: data_prep.R
# ===================================
# -----------------------------------
# Calculate Respiration Rate
# -----------------------------------
calculate_resp_rate <- function(abp_data) {
  # Perform Fast Fourier Transform (FFT) to estimate respiration frequency
  fft_result <- Mod(fft(abp_data$ABP))
  freq_seq <- seq(from = 0, by = 125 / length(fft_result), length.out = length(fft_result))
  
  # Filter the frequency range typically associated with respiration (0.1-0.5 Hz)
  resp_data <- tibble(fft = fft_result, freq = freq_seq) %>%
    filter(freq > 0.1, freq < 0.5)
  
  # Find the most prominent frequency and convert it to respiratory rate (in breaths per minute)
  resp_rate <- 60 * resp_data$freq[which.max(resp_data$fft)]
  
  return(round(resp_rate))
}

# -----------------------------------
# Generate Inspiration Start Times
# -----------------------------------
create_insp_start_times <- function(abp_data) {
  # Calculate respiration rate
  resp_rate <- calculate_resp_rate(abp_data)
  
  # Generate inspiration start times based on the calculated respiration rate
  start_time <- min(abp_data$time)
  end_time <- max(abp_data$time)
  insp_period <- 60 / resp_rate
  insp_times <- seq(from = start_time, to = end_time, by = insp_period)
  
  # Create a data frame with inspiration start times
  insp_df <- data.frame(time = insp_times, label = "insp start")
  
  return(insp_df)
}

# -----------------------------------
# Process RDS Data Files
# -----------------------------------
process_rds_data <- function(data) {
  # Convert ABP, ECG, and CVP data to tibble format
  abp_df <- tibble(time = data$ABP$seconds, ABP = data$ABP$ABP)
  ecg_df <- tibble(time = data$ECG$seconds, ECG_II = data$ECG$ECG_II)
  cvp_df <- tibble(time = data$CVP$seconds, CVP = data$CVP$CVP)
  
  # Calculate inspiration start times
  insp_df <- create_insp_start_times(abp_df)
  
  # Detect R-peaks from ECG data
  rpeaks <- detect_rpeaks(ecg_df$ECG_II, sRate = 500)
  qrs_df <- data.frame(time = rpeaks, label = "R peak")
  
  # Return processed data as a list
  sample_pp <- list(
    abp = abp_df, 
    insp_start = insp_df
  )
  
  sample_cvp <- list(
    cvp = cvp_df,
    ecg = ecg_df,
    insp_start = insp_df,
    qrs = qrs_df,
    fluid_start = NA,
    fluid_end = NA
  )
  
  return(list(sample_pp = sample_pp, sample_cvp = sample_cvp))
}

# -----------------------------------
# Process EDF Data Files
# -----------------------------------
process_edf_data <- function(edf_file_path) {
  pt_header <- readEdfHeader(edf_file_path)
  
  # Read the signals with correct names
  pt_art <- readEdfSignals(pt_header, signals = "ART")
  pt_cvp <- tryCatch({
    readEdfSignals(pt_header, signals = "CVP-1")
  }, error = function(e1) {
    warning("CVP-1 not available, trying CVP: ", e1$message)
    tryCatch({
      readEdfSignals(pt_header, signals = "CVP")
    }, error = function(e2) {
      stop("Failed to load CVP-1 and CVP: ", e2$message)
    })
  })
  pt_ecg <- tryCatch({
    readEdfSignals(pt_header, signals = "ECG_II")
  }, error = function(e1) {
    warning("ECG_II not available, trying ECG_III: ", e1$message)
    tryCatch({
      readEdfSignals(pt_header, signals = "ECG_III")
    }, error = function(e2) {
      stop("Failed to load ECG_II and ECG_III: ", e2$message)
    })
  })
  
  
  # Convert ABP data to tibble format
  ABP <- tibble(
    time = seq(pt_art$startTime, by = 1/pt_art$sRate, along.with = pt_art$signal),
    ABP = as.numeric(pt_art$signal)) %>% 
    mutate(time = as.numeric(difftime(time, min(time), units = "secs"))) %>%
    select(time, ABP)
  
  # Convert CVP data to tibble format
  CVP <- tibble(
    time = seq(pt_cvp$startTime, by = 1 / pt_cvp$sRate, along.with = pt_cvp$signal),
    CVP = pt_cvp$signal) %>%
    mutate(time = as.numeric(difftime(time, min(time), units = "secs"))) %>%
    select(time, CVP) 
  
  # Convert ECG data to tibble format
  ECG <- tibble(
    time = seq(pt_ecg$startTime, by = 1 / pt_ecg$sRate, along.with = pt_ecg$signal),
    ECG_II = pt_ecg$signal) %>%
    mutate(time = as.numeric(difftime(time, min(time), units = "secs"))) %>%
    select(time, ECG_II) 
  
  # Calculate inspiration start times
  insp_df <- create_insp_start_times(ABP)
  
  # Detect R-peaks from ECG data
  rpeaks <- detect_rpeaks(ECG$ECG_II, sRate = 500)
  qrs_df <- tibble(time = rpeaks, label = "R peak")
  
  # Return processed data as a list
  sample_pp <- list(
    abp = ABP,
    insp_start = insp_df
  )
  
  sample_cvp <- list(
    cvp = CVP,
    ecg = ECG,
    insp_start = insp_df,
    qrs = qrs_df,
    fluid_start = NA,
    fluid_end = NA
  )
  
  return(list(sample_pp = sample_pp, sample_cvp = sample_cvp))
}

# -----------------------------------
# Process VitalDB Data Files
# -----------------------------------
process_vitaldb_data <- function(caseid, fluid = NA) {
  on.exit(closeAllConnections())
  
  # Load raw data from VitalDB for specific caseid
  data_art <- VitalDBR::load_case(tname = 'SNUADC/ART', caseid = caseid)
  data_cvp <- VitalDBR::load_case(tname = 'SNUADC/CVP', caseid = caseid)
  data_ecg <- VitalDBR::load_case(tname = 'SNUADC/ECG_II', caseid = caseid)
  
  # Handle fluid data
  if (!is.na(fluid)) {
    tname <- paste0('Orchestra/', fluid, '_RATE')
    
    data_fluid <- tryCatch({
      df <- VitalDBR::load_case(tname = tname, caseid = caseid)
      fluid_col <- names(df)[2]
      tibble(time = df$Time, fluid_value = df[[fluid_col]]) 
    }, error = function(e) {
      warning("Error loading fluid data: ", e$message)
      return(NULL)
    })
    
    if (!is.null(data_fluid) && nrow(data_fluid) > 0) {
      fluid_start <- min(data_fluid$time[data_fluid$fluid_value != min(data_fluid$fluid_value)], na.rm = TRUE)
      fluid_end <- max(data_fluid$time[data_fluid$fluid_value != max(data_fluid$fluid_value)], na.rm = TRUE)
    } else {
      fluid_start <- NA
      fluid_end <- NA
    }
  } else {
    fluid_start <- NA
    fluid_end <- NA
  }
  
  # Convert ABP, CVP, and ECG data to tibble format
  abp_df <- tibble(time = data_art$Time, ABP = data_art$SNUADC.ART)
  cvp_df <- tibble(time = data_cvp$Time, CVP = data_cvp$SNUADC.CVP)
  ecg_df <- tibble(time = data_ecg$Time, ECG_II = data_ecg$SNUADC.ECG_II)
  
  # Calculate inspiration start times
  insp_df <- create_insp_start_times(abp_df)
  
  # Detect R-peaks from ECG data
  rpeaks <- detect_rpeaks(ecg_df$ECG_II, sRate = 500)
  qrs_df <- tibble(time = rpeaks, label = "R peak")
  
  # Return processed data
  sample_pp <- list(
    abp = abp_df, 
    insp_start = insp_df
    )
  
  sample_cvp <- list(
    cvp = cvp_df, 
    ecg = ecg_df, 
    insp_start = insp_df, 
    qrs = qrs_df, 
    fluid_start = fluid_start, 
    fluid_end = fluid_end
    )
  
  return(list(sample_pp = sample_pp, sample_cvp = sample_cvp))
}