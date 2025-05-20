# =========================================
# Modules: DataUploadModule.R
# =========================================
# -----------------------------------------
# UI Function for Data Upload Module
# -----------------------------------------
DataUploadUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # Style to hide the file input progress bar
    tags$style(HTML("
      .shiny-file-input-progress {
        display: none !important;
      }
    ")),
    
    h2("Upload your Data"),
    tags$p(
      "Select your preferred data source below to begin. You can upload a local .RDS or .EDF file, fetch data directly from the VitalDB database, or load pre-cleaned data.", tags$br(), tags$br(),
      "If uploading an RDS or EDF file, you will be asked to manually specify the time of intervention. For VitalDB data, this time window is automatically set: from 30 seconds before to 5 minutes after the intervention."
    ),
    
    tags$p(
      tags$b("Please note:")
    ),
    tags$ul(
      tags$li("Large files may cause the application to crash. To ensure smooth performance, uploaded data should not exceed approximately 20 minutes of monitoring time."),
      tags$li("If you have already loaded a file and wish to upload a new one, please refresh the page before selecting the new file. This will reset the data and ensure that the subsequent results are accurate.")
    ),
    
   
    
    # Panel for selecting the data source
    wellPanel(
      # Dropdown to select the data source type
      selectInput(ns("data_source"), "Data Source:", 
                  choices = c("RDS File" = "rds", "VitalDB Data" = "vitaldb", "EDF File" = "edf", "Previously Cleaned Data" = "fd")),
      
      # Conditional panels based on selected data source
      # RDS input
      conditionalPanel(
        condition = sprintf("input['%s'] == 'rds'", ns("data_source")),
        fileInput(ns("rds_file"), "Upload RDS file", accept = c(".rds"))
      ),
      
      # VitalDB input
      conditionalPanel(
        condition = sprintf("input['%s'] == 'vitaldb'", ns("data_source")),
        uiOutput(ns("medication_ui")),
        uiOutput(ns("caseid_ui"))
      ),
      
      # EDF input
      conditionalPanel(
        condition = sprintf("input['%s'] == 'edf'", ns("data_source")),
        fileInput(ns("edf_file"), "Upload EDF file", accept = c(".edf"))
      ),
      
      # Filtered data input
      conditionalPanel(
        condition = sprintf("input['%s'] == 'fd'", ns("data_source")),
        fileInput(ns("fd_file"), "Upload Filtered RDS file", accept = c(".rds"))
      ),
      
      # Button to trigger data loading
      div(style = "margin-top: 15px;",
          actionButton(ns("load_data"), "Load Data", class = "btn-primary")
      )
    ),
    
    # Intervention time panel (shown after data is loaded for RDS and EDF)
    uiOutput(ns("intervention_note")),
    uiOutput(ns("intervention_time_ui")),
    
    # Output to show the structure of the loaded data
    verbatimTextOutput(ns("data_info"))
  )
}

# -----------------------------------------
# Server Function for Data Upload Module
# -----------------------------------------
DataUploadServer <- function(id, data_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive to fetch metadata for VitalDB source
    md <- reactive({
      req(input$data_source == "vitaldb")  # Only load if vitaldb is selected
      tryCatch({
        session$sendCustomMessage("disable-dismiss", "true")
        withProgress(message = 'Loading VitalDB metadata...', value = 0.5, {
          VitalDBR::load_VDB("https://api.vitaldb.net/trks")
        })
      }, error = function(e) {
        showNotification("Could not load VitalDB metadata", type = "error")
        NULL
      })
    })
    
    medication_mapping <- reactive({
      data.frame(
        code = c("NEPI", "EPI", "DOPA", "PHEN"),
        name = c("Norepinephrine", "Epinephrine", "Dopamine", "Phenylephrine"),
        stringsAsFactors = FALSE
      )
    })
    
    # Collecting orchestra names for medication selection
    orchestra_names <- reactive({
      req(md())
      # Extract unique orchestra names
      all_codes <- unique(sub(".*\\/([^\\/]+)_.*", "\\1", md()$tname[grep("Orchestra", md()$tname)]))
      
      # Filter to only include allowed medication types
      allowed_meds <- medication_mapping()
      valid_codes <- intersect(all_codes, allowed_meds$code)
      
      # Return named vector for UI selection (code = value, name = label)
      meds <- allowed_meds[allowed_meds$code %in% valid_codes, ]
      setNames(meds$code, meds$name)
    })
    
    # Render medication selection UI
    output$medication_ui <- renderUI({
      meds <- orchestra_names()
      if(length(meds) == 0) {
        return(helpText("No allowed medications found in data"))
      }
      selectInput(ns("medication"), "Select Medication", choices = meds)
    })
    
    # Render case ID selection UI
    output$caseid_ui <- renderUI({
      req(input$medication, md())
      
      session$sendCustomMessage("disable-dismiss", "true")
      withProgress(message = 'Filtering case IDs...', value = 0.3, {
        filtered_caseids <- tryCatch({
          md() %>%
            dplyr::filter(
              tname %in% c(
                "SNUADC/ART",
                "SNUADC/CVP",
                "SNUADC/ECG_II",
                paste0("Orchestra/", input$medication, "_RATE")
              )
            ) %>%
            dplyr::count(caseid) %>%    # Count entries per caseid
            dplyr::filter(n == 4) %>%   # Keep only cases with all 4 signals
            dplyr::pull(caseid)
        }, error = function(e) {
          showNotification("Error filtering cases", type = "error")
          NULL
        })
        
        if (is.null(filtered_caseids)) {
          return(helpText("Could not fetch cases"))
        }
        
        if (length(filtered_caseids) == 0) {
          return(helpText("No cases found for selected medication"))
        }
        
        selectInput(ns("caseid"), "Select Case ID", choices = filtered_caseids)
      })
    })
    
    # Intervention time UI (shown after data is loaded for RDS and EDF)
    output$intervention_time_ui <- renderUI({
      req(data_rv$data, input$data_source %in% c("rds", "edf"))
      
      intervention_start <- ifelse(!is.null(data_rv$data$data$sample_cvp$fluid_start) && 
                                     !is.na(data_rv$data$data$sample_cvp$fluid_start),
                                   data_rv$data$data$sample_cvp$fluid_start,
                                   NA)
      
      intervention_end <- ifelse(!is.null(data_rv$data$data$sample_cvp$fluid_end) && 
                                   !is.na(data_rv$data$data$sample_cvp$fluid_end),
                                 data_rv$data$data$sample_cvp$fluid_end,
                                 NA)
      
      tagList(
        wellPanel(
          fluidRow(
            column(6,
                   numericInput(ns("intervention_start"), "Intervention Start Time (seconds):", 
                                value = round(intervention_start))
            ),
            column(6,
                   numericInput(ns("intervention_end"), "Intervention End Time (seconds):", 
                                value = round(intervention_end))
            )
          ),
          actionButton(ns("save_intervention"), "Save Intervention Times", class = "btn-primary")
        )
      )
    })
    
    # Render intervention note only when the data is loaded and the source is either RDS or EDF
    output$intervention_note <- renderUI({
      req(data_rv$data, input$data_source %in% c("rds", "edf"))
      
      # If the data is loaded, show the note
      tagList(
        tags$p(
          tags$b("Please note:"), 
          "To ensure all animations work properly in the later tabs, both the intervention start and end times must be set."
        )
      )
    })
    
    # Output to show current intervention times
    output$current_intervention_times <- renderText({
      req(data_rv$data)
      
      intervention_start <- if(!is.null(data_rv$data$data$sample_cvp$fluid_start) && 
                               !is.na(data_rv$data$data$sample_cvp$fluid_start)) {
        round(data_rv$data$data$sample_cvp$fluid_start, 1)
      } else {
        "Not set"
      }
      
      intervention_end <- if(!is.null(data_rv$data$data$sample_cvp$fluid_end) && 
                             !is.na(data_rv$data$data$sample_cvp$fluid_end)) {
        round(data_rv$data$data$sample_cvp$fluid_end, 1)
      } else {
        "Not set"
      }
      
      paste("Current intervention times - Start:", intervention_start, "seconds, End:", intervention_end, "seconds")
    })
    
    # Observe for saving intervention times
    observeEvent(input$save_intervention, {
      req(input$data_source %in% c("rds", "edf"), 
          !is.na(input$intervention_start), !is.na(input$intervention_end),
          input$intervention_end >= input$intervention_start,
          data_rv$data)
      
      # Update the intervention times reactive data
      data_rv$data$data$sample_cvp$fluid_start <- input$intervention_start
      data_rv$data$data$sample_cvp$fluid_end <- input$intervention_end
      
      # Trigger update of data_info output
      data_rv$trigger <- Sys.time()
      
      showNotification("Intervention times saved successfully", type = "message")
    })
    
    # Data info output
    output$data_info <- renderText({
      req(data_rv$data)
      
      # Use reactive dependency to ensure update
      data_rv$trigger
      
      data <- data_rv$data$data
      beats <- data_rv$data$beats
      
      paste(
        "=== Data Point Overview ===\n",
        "ABP Data Points:", nrow(data$sample_pp$abp), "\n",
        "CVP Data Points:", ifelse(!is.null(data$sample_cvp$cvp), 
                                   nrow(data$sample_cvp$cvp), "0"), "\n",
        "ECG Data Points:", ifelse(!is.null(data$sample_cvp$ecg), 
                                   nrow(data$sample_cvp$ecg), "0"), "\n\n",
        "Total Time:", round(max(data$sample_pp$abp$time) - min(data$sample_pp$abp$time)), "seconds\n",
        "Intervention Start:", if(!is.null(data$sample_cvp$fluid_start) && 
                                  !is.na(data$sample_cvp$fluid_start)) {
          round(data$sample_cvp$fluid_start)
        } else {
          "Not set"
        }, "seconds\n",
        "Intervention End:", if(!is.null(data$sample_cvp$fluid_end) && 
                                !is.na(data$sample_cvp$fluid_end)) {
          round(data$sample_cvp$fluid_end)
        } else {
          "Not set"
        }, "seconds\n\n",
        "Heart Rate:", if(!is.null(beats)) round(calculate_bpm(beats), 1) else "N/A", "beats/min\n"
      )
    })
    
    # Main function for loading the data based on selected source
    observeEvent(input$load_data, {
      req(input$data_source)
      
      tryCatch({
        # Handle data loading for different sources (RDS, EDF, etc.)
        if (input$data_source == "rds") {
          req(input$rds_file)
          
          session$sendCustomMessage("disable-dismiss", "true")
          withProgress(message = 'Loading RDS file...', value = 0.2, {
            data <- readRDS(input$rds_file$datapath)
          })
          
          session$sendCustomMessage("disable-dismiss", "true")
          withProgress(message = 'Processing data...', value = 0.6, {
            data <- process_rds_data(data)
          })
          
        } else if (input$data_source == "edf") {
          req(input$edf_file)
          session$sendCustomMessage("disable-dismiss", "true")
          withProgress(message = 'Loading EDF file...', value = 0.2, {
            data <- process_edf_data(input$edf_file$datapath)
          })
          
        } else if (input$data_source == "fd") {
          req(input$fd_file)
          session$sendCustomMessage("disable-dismiss", "true")
          withProgress(message = 'Loading preprocessed data...', value = 0.2, {
            data <- readRDS(input$fd_file$datapath)
          })
          
        } else if (input$data_source == "vitaldb") {
          req(input$medication, input$caseid)
          session$sendCustomMessage("disable-dismiss", "true")
          withProgress(message = 'Fetching VitalDB data...', value = 0.2, {
            data <- process_vitaldb_data(caseid = input$caseid, fluid = input$medication)
            
            #Automatically slice VitalDB data to 30 sec before and 5 min after intervention start
            if (!is.null(data)) {
              intervention_start <- data$sample_cvp$fluid_start
              if (!is.null(intervention_start)) {
                start_time <- intervention_start - 30
                end_time <- intervention_start + 300

                # Filter various datasets by time window
                data$sample_pp$abp <- data$sample_pp$abp %>%
                  filter(time >= start_time & time <= end_time)

                data$sample_pp$insp_start <- data$sample_pp$insp_start %>%
                  filter(time >= start_time & time <= end_time)

                data$sample_cvp$cvp <- data$sample_cvp$cvp %>%
                  filter(time >= start_time & time <= end_time)

                data$sample_cvp$ecg <- data$sample_cvp$ecg %>%
                  filter(time >= start_time & time <= end_time)

                data$sample_cvp$insp_start <- data$sample_cvp$insp_start %>%
                  filter(time >= start_time & time <= end_time)

                data$sample_cvp$qrs <- data$sample_cvp$qrs %>%
                  filter(time >= start_time & time <= end_time)
              }
            }
          })
        }
        
        
        # Post-processing (if not filtered data)
        if (!is.null(data) && !input$data_source == "fd") {
          session$sendCustomMessage("disable-dismiss", "true")
          withProgress(message = 'Processing data...', value = 0.6, {
            # Correct R-peak positions
            if (!is.null(data$sample_cvp$qrs$time)) {
              data <- correct_rpeaks(data)
            }
            
            
            # Validate R-peaks
            # indexes must be between 1 and the amount af ECG datapoints
            # The R-peak index corresponds to the index of a data point in the ECG signal
            if (!is.null(data$sample_cvp$qrs$index)) {
              valid_indices <- data$sample_cvp$qrs$index >= 1 & 
                data$sample_cvp$qrs$index <= nrow(data$sample_cvp$ecg)
              # remove the R-peaks 
              data$sample_cvp$qrs <- data$sample_cvp$qrs[valid_indices, ]
            }
          })
        }
        
        # Save the data to the reactive container
        session$sendCustomMessage("disable-dismiss", "true")
        withProgress(message = 'Saving data...', value = 0.95, {
          if (!is.null(data$sample_pp$abp)) {
            beats <- find_abp_beats(data$sample_pp$abp)
            data_rv$data <- list(data = data, beats = beats)
          } else {
            data_rv$data <- list(data = data)
          }
        })
        
        showNotification("Data loaded successfully", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
      })
    })
  })
}