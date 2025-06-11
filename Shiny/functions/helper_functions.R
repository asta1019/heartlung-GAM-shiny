# ===================================================
# Functions: helper_functions.R
# ===================================================
# ---------------------------------------------------
# Calculate Beats Pr. Minute
# ---------------------------------------------------
calculate_bpm <- function(beats) {
  # Calculate inter-beat intervals (IBI) in seconds, by finding time differences between systolic times
  ibi <- diff(beats$time_systole)
  
  # Calculate average IBI
  avg_ibi <- mean(ibi, na.rm = TRUE)
  
  # Calculate beats per minute (BPM) as 60 / average IBI
  bpm <- 60 / avg_ibi
  return(bpm)
}

# ---------------------------------------------------
# adjust R-peaks
# ---------------------------------------------------
correct_rpeaks <- function(data) {
  # Initial QRS sample identification
  data$sample_cvp$qrs$samples <- sapply(data$sample_cvp$qrs$time, function(qrs_time) {
    which(data$sample_cvp$ecg$time >= qrs_time)[1]
  })
  
  # Mark initial R-peaks
  data$sample_cvp$ecg$Rpeak <- NA
  data$sample_cvp$ecg$Rpeak[data$sample_cvp$qrs$samples] <- 
    data$sample_cvp$ecg$ECG_II[data$sample_cvp$qrs$samples]
  
  # Correct R-peak positions
  data$sample_cvp$qrs$correctedRpeak_time <- sapply(data$sample_cvp$qrs$samples, function(sample) {
    # Ensure window stays within bounds
    start <- max(1, sample - 10)
    end <- min(nrow(data$sample_cvp$ecg), sample + 20)
    qrs_segment <- data$sample_cvp$ecg$ECG_II[start:end]
    max_index <- which.max(qrs_segment)
    data$sample_cvp$ecg$time[start + max_index - 1]  # -1 because which.max returns 1-based index. Gives the Global correct position 
  })
  
  # Replace qrs$time with correctedRpeak_time
  data$sample_cvp$qrs$time <- data$sample_cvp$qrs$correctedRpeak_time
  
  # Remove the old qrs$time column (if necessary)
  data$sample_cvp$qrs$correctedRpeak_time <- NULL  # Optional: remove the column if you don't need it anymore
  
  # Mark corrected R-peaks
  data$sample_cvp$qrs$corrected_samples <- sapply(data$sample_cvp$qrs$time, 
                                                  function(correctedRpeak_time) {
                                                    which(data$sample_cvp$ecg$time >= correctedRpeak_time)[1]
                                                  })
  
  data$sample_cvp$ecg$corrected_Rpeak <- NA
  data$sample_cvp$ecg$corrected_Rpeak[data$sample_cvp$qrs$corrected_samples] <- 
    data$sample_cvp$ecg$ECG_II[data$sample_cvp$qrs$corrected_samples]
  
  # Return the modified data object
  return(data)
}


# ---------------------------------------------------
# Function: adjusted_insp_index.R for cvp module
# ---------------------------------------------------
adjust_cvp_insp_index <- function(cvp, qrs_times, insp_start_times, PQ_interval = 0.150) {
  # Add initial timing indexes based on cardiac and respiratory events
  cvp_with_index <- cvp %>%
    add_time_since_event(qrs_times - PQ_interval, prefix = "P_wave") %>%
    add_time_since_event(insp_start_times, prefix = "insp")
  
  # Find the time of minimum CVP during the first inspiration cycle
  new_start_time <- cvp_with_index %>%
    filter(insp_n == 1) %>%
    slice_min(CVP, with_ties = FALSE) %>%
    pull(time)
  
  # Adjust indexing relative to the new cycle reference point
  cvp_with_index %>%
    mutate(
      insp_index = (time - new_start_time) %% insp_cycle_len,        # tide siden aktuelle cyklus startede
      insp_rel_index = (time - new_start_time) / insp_cycle_len,    # Normalize time within respiratory cycle
      insp_rel_index = insp_rel_index %% 1,                         # Wrap around to keep index between 0 and 1
      insp_n = floor((time - new_start_time) / insp_cycle_len) + 1  # Recalculate inspiration cycle number
    )
}


# ---------------------------------------------------
# Function: adjusted_insp_index.R for abp module
# ---------------------------------------------------

adjust_abp_insp_index <- function(abp, cvp, qrs_times, insp_start_times, PQ_interval = 0.150) {
  # Add time since last P-wave and inspiration start to ABP data
  abp_with_index <- abp %>%
    add_time_since_event(qrs_times - PQ_interval, prefix = "P_wave") %>%
    add_time_since_event(insp_start_times, prefix = "insp")
  
  # Add the same indexing to CVP data for alignment/reference
  cvp_with_index <- cvp %>%
    add_time_since_event(qrs_times - PQ_interval, prefix = "P_wave") %>%
    add_time_since_event(insp_start_times, prefix = "insp")
  
  # Identify the time of the lowest CVP value within the first inspiration cycle
  new_start_time <- cvp_with_index %>%
    filter(insp_n == 1) %>%
    slice_min(CVP, with_ties = FALSE) %>%
    pull(time)
  
  # Adjust ABP data using the new reference time
  abp_with_index %>%
    mutate(
      insp_index = (time - new_start_time) %% insp_cycle_len,
      insp_rel_index = (time - new_start_time) / insp_cycle_len,    # Normalize time within respiratory cycle
      insp_rel_index = insp_rel_index %% 1,                         # Wrap around to keep index between 0 and 1
      insp_n = floor((time - new_start_time) / insp_cycle_len) + 1  # Recalculate inspiration cycle number
    )
}

