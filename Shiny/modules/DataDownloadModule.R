# =======================================================
# Modules: DataDownloadModule.R
# =======================================================
# -------------------------------------------------------
# UI Function for Download & Data Structure Module
# -------------------------------------------------------
DataDownloadUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("Download Cleaned Data"),
      tags$p("You can now save and export your fully processed dataset for future use.", 
             "The cleaned data is downloaded as an RDS file, preserving all processing steps and analysis results.", 
             "To continue your work later, simply reupload the file using the “Previously Cleaned Data” option in the Data Upload section.",
             "Your analysis will resume exactly where you left off, with all data changes intact."),
    
    downloadButton(ns("download_data"), "Download renset data (.rds)"),
    br(), br(),
    verbatimTextOutput(ns("data_structure"))
  )
}

# -------------------------------------------------------
# Server Function for Download & Data Structure Module
# -------------------------------------------------------
DataDownloadServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive to extract data
    data_reactive <- reactive({
      req(data_in())
      data_in()
    })
    
    # Prepare cleaned and structured data for download
    download_data <- reactive({
      req(data_reactive())
      
      if (!is.null(data_reactive()$data) && !is.null(data_reactive()$beats)) {
        return(list(
          sample_pp = data_reactive()$data$sample_pp,
          sample_cvp = data_reactive()$data$sample_cvp,
          beats = data_reactive()$beats
        ))
      }
      
      data_reactive()  # fallback
    })
    
    # Overview of data structure
    output$data_structure <- renderText({
      req(data_reactive())
      
      pp_str <- capture.output(str(data_reactive()$data$sample_pp))
      cvp_str <- capture.output(str(data_reactive()$data$sample_cvp))
      beats_str <- capture.output(str(data_reactive()$beats))
      
      bpm <- calculate_bpm(data_reactive()$beats)
      
      paste(
        "=== Data Point Overview ===\n",
        "ABP Data Points:", nrow(data_reactive()$data$sample_pp$abp), "\n",
        "CVP Data Points:", ifelse(!is.null(data_reactive()$data$sample_cvp$cvp), 
                                   nrow(data_reactive()$data$sample_cvp$cvp), "0"), "\n",
        "ECG Data Points:", ifelse(!is.null(data_reactive()$data$sample_cvp$ecg), 
                                   nrow(data_reactive()$data$sample_cvp$ecg), "0"), "\n\n",
        "Total Time:", round(max(data_reactive()$data$sample_pp$abp$time) - 
                               min(data_reactive()$data$sample_pp$abp$time)), "seconds\n",
        "Intervention Start:", if (!is.null(data_reactive()$data$sample_cvp$fluid_start) && 
                                   !is.na(data_reactive()$data$sample_cvp$fluid_start)) {
          round(data_reactive()$data$sample_cvp$fluid_start)
        } else {
          "Not set"
        }, "seconds\n",
        "Intervention End:", if (!is.null(data_reactive()$data$sample_cvp$fluid_end) && 
                                 !is.na(data_reactive()$data$sample_cvp$fluid_end)) {
          round(data_reactive()$data$sample_cvp$fluid_end)
        } else {
          "Not set"
        }, "seconds\n\n",
        "Heart Rate:", if (!is.null(data_reactive()$beats)) round(bpm, 1) else "N/A", "beats/min\n",
        "\n=== Structure of Pulse Pressure Data ===\n",
        paste(pp_str, collapse = "\n"), "\n\n",
        "=== Structure of Central Venous Pressure Data ===\n", 
        paste(cvp_str, collapse = "\n"), "\n\n",
        "=== Structure of Calculated Beats ===\n", 
        paste(beats_str, collapse = "\n"), "\n\n"
      )
    })
    
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("processed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        req(download_data())
        saveRDS(download_data(), file)
      }
    )
  })
}

