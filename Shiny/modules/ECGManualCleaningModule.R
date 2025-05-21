# ========================================
# Modules: ECGCleaningModule.R
# ========================================
# ----------------------------------------
# UI Function for ECG Cleaning Module
# ----------------------------------------
CleanECGModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("Electrocardiogram R-Peaks Cleaning - Manual"),
    tags$p("Here you can manually remove R-peaks from the ECG data."),
    
    fluidRow(
      column(
        width = 3,
        br(),
        sliderInput(ns("x_range"), "Time window (seconds)", 
                    min = 0, max = 10, value = c(0, 10), step = 1), 
        DTOutput(ns("datatable")),
        div(style = "display: flex; gap: 5px; padding-top: 5px;",
            actionButton(ns("remove_selected"), "Remove selected peaks", width = "100%"),
            actionButton(ns("undo_remove"), "Undo latest removal", width = "100%")
        )
      ),
      
      column(
        width = 9,
        tags$div(
          style = "margin-top: 28px; margin-left: 10px;",
          h4("ECG Signal with R-peak Markers"),
          plotlyOutput(ns("ecgPlot"), height = "300px"),
          h4("ECG R-R Interval"),
          plotlyOutput(ns("rrPlot"), height = "300px"))
      )
    )
  )
}

# ----------------------------------------
# Server Function for ECG Cleaning Module
# ----------------------------------------
CleanECGModuleServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive call for the hole dataset
    data_reactive <- reactive({
      req(data_in())
      data_in()
    })
    
    # Store original, current, and deleted R-peaks
    original_rpeaks <- reactiveVal(NULL)
    current_rpeaks <- reactiveVal(NULL)
    deleted_rpeaks <- reactiveVal(data.frame())
    undo_stack <- reactiveVal(list()) 
    slider_updated <- reactiveVal(FALSE)
    
    # Load data and initialize R-peaks
    observe({
      req(data_reactive()$data$sample_cvp)
      
      ecg_data <- data_reactive()$data$sample_cvp$ecg
      qrs_data <- data_reactive()$data$sample_cvp$qrs
      
      # Initialize slider based on ECG time range
      if (!slider_updated()) {
        updateSliderInput(session, "x_range",
                          min = floor(min(ecg_data$time, na.rm = TRUE)),
                          max = ceiling(max(ecg_data$time, na.rm = TRUE)),
                          value = c(min(ecg_data$time), ceiling(max(ecg_data$time, na.rm = TRUE))))
        slider_updated(TRUE)
      }
      
      if (is.null(original_rpeaks())) original_rpeaks(qrs_data)
      if (is.null(current_rpeaks())) current_rpeaks(qrs_data)
    })
    
    # Compute amplitude values for current R-peaks
    rpeaks_with_amplitude <- reactive({
      req(current_rpeaks(), data_reactive()$data$sample_cvp$ecg)
      
      ecg_data <- data_reactive()$data$sample_cvp$ecg
      peaks <- current_rpeaks()
      
      # Get ECG amplitude at each R-peak time
      peaks$amplitude <- sapply(peaks$time, function(t) {
        idx <- which.min(abs(ecg_data$time - t))
        ecg_data$ECG_II[idx]
      })
      
      peaks
    })
    
    # -------------------------------
    # DATATABLE
    # -------------------------------
    
    # DataTable with amplitude (filtered by time range)
    output$datatable <- renderDT({
      req(rpeaks_with_amplitude(), input$x_range)
      
      rpeaks_with_amplitude() %>%
        filter(time >= input$x_range[1], time <= input$x_range[2]) %>%
        select(time, amplitude) %>%
        datatable(
          selection = 'multiple',
          options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))),
          rownames = TRUE
        ) %>%
        formatRound(columns = c("time", "amplitude"), digits = 3)
    })
    
    # Remove selected R-peaks
    observeEvent(input$remove_selected, {
      req(current_rpeaks(), input$datatable_rows_selected)

      visible_peaks <- rpeaks_with_amplitude() %>%
        filter(time >= input$x_range[1], time <= input$x_range[2]) %>%
        select(amplitude, time)
      
      selected_times <- visible_peaks$time[input$datatable_rows_selected]
      
      undo_stack(c(isolate(undo_stack()), list(isolate(current_rpeaks()))))
      
      updated_peaks <- current_rpeaks() %>%
        filter(!time %in% selected_times)
      
      current_rpeaks(updated_peaks)
    })
    
    # Undo last deletion
    observeEvent(input$undo_remove, {
      if (length(undo_stack()) > 0) {
        last_state <- undo_stack()[[length(undo_stack())]]
        undo_stack(undo_stack()[-length(undo_stack())])
        current_rpeaks(last_state)
      }
    })
    
    # Filtered ECG 
    filtered_ecg <- reactive({
      req(data_reactive()$data$sample_cvp$ecg, input$x_range)
      ecg_data <- data_reactive()$data$sample_cvp$ecg
      ecg_data %>%
        filter(time >= input$x_range[1], time <= input$x_range[2])
    })
    
    # Filtered R-peaks
    filtered_rpeaks <- reactive({
      req(current_rpeaks(), input$x_range)
      ecg_data <- data_reactive()$data$sample_cvp$ecg
      peaks <- current_rpeaks() %>%
        filter(time >= input$x_range[1], time <= input$x_range[2])
      
      peaks$y_val <- sapply(peaks$time, function(t) {
        idx <- which.min(abs(ecg_data$time - t))
        ecg_data$ECG_II[idx]
      })
      
      peaks
    })
    
    # -------------------------------
    # PLOTS
    # -------------------------------
    
    # Plot ECG signal 
    output$ecgPlot <- renderPlotly({
      req(filtered_ecg(), filtered_rpeaks())
      
      ecg <- filtered_ecg()
      rpeaks <- filtered_rpeaks()
      
      p <- ggplot(ecg, aes(x = time, y = ECG_II)) +
        geom_line() +
        geom_point(data = rpeaks, aes(x = time, y = y_val, shape = "R-peak"), 
                   color = "red", size = 2) +
        scale_shape_manual(name = " ", 
                           values = c("R-peak" = 4),
                           breaks = c("R-peak")) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        labs(title = "ECG Signal with R-peaks",
             x = "Time [s]",
             y = "Amplitude [mV]")
      
      ggplotly(p) %>%
        layout(
          xaxis = list(title = list(text = "Time [s]", standoff = 5)),
          margin = list(b = 10),
          legend = list(
            orientation = "h",
            x = 0.5, xanchor = "center",
            y = -0.2, yanchor = "top",
            font = list(size = 10)
          )
        )
    })
    
    # Plot R-R Interval
    output$rrPlot <- renderPlotly({
      req(current_rpeaks(), input$x_range)
      
      if (nrow(current_rpeaks()) > 1) {
        rr_df <- data.frame(
          time = current_rpeaks()$time[-1],
          RR_interval = diff(current_rpeaks()$time)
        )
        
        y_range <- c(0, max(rr_df$RR_interval, na.rm = TRUE)) * 1.1
        
        shaded_rect <- data.frame(
          xmin = input$x_range[1],
          xmax = input$x_range[2],
          ymin = y_range[1],
          ymax = y_range[2]
        )
        
        p <- ggplot(rr_df, aes(x = time, y = RR_interval)) +
          geom_rect(data = shaded_rect, 
                    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                    inherit.aes = FALSE,
                    fill = "dodgerblue2", alpha = 0.2) +
          geom_line(color = "black") +
          geom_point(color = "black") +
          theme_minimal() +
          labs(title = "R-R Interval",
               x = "Time [s]",
               y = "R-R Interval [s]") +
          theme(legend.position = "none")
        
        ggplotly(p) %>%
          layout(
            xaxis = list(title = list(text = "Time [s]", standoff = 1)),
            margin = list(b = 40)
          )
        
      } else {
        plot_ly() %>%
          layout(title = "Not enough R-peaks for R-R Interval plot")
      }
    })
    
    # Return the updated data with cleaned R-peaks
    return(reactive({
      updated_data <- data_reactive()
      if (!is.null(current_rpeaks())) {
        updated_data$data$sample_cvp$qrs <- current_rpeaks()
      }
      updated_data
    }))
  })
}
