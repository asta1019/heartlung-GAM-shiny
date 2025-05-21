# ==================================================================
# Modules: PPManualCleaningModule.R
# ==================================================================
# ------------------------------------------------------------------
# UI Function for manual Pulse Pressure and ABP Cleaning Module
# ------------------------------------------------------------------
ManualCleanPPUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("Pulse Pressure Data Cleaning step - Manual"), 
    tags$p("Here you can manually remove additional data points, even after using the preselect step."),
    
    fluidRow(
      column(
        width = 3,
        br(),
        sliderInput(ns("time_window"), "Time window (seconds)", 
                    min = 0, max = 10, value = c(0, 10), step = 1),
        DTOutput(ns("beats_table")),
        div(style = "display: flex; gap: 5px; padding-top: 5px;",
            actionButton(ns("remove_selected"), "Remove selected beats", 
                         width = "100%"),
            actionButton(ns("undo_remove"), "Undo latest removal", 
                         width = "100%")
        )
      ),
      column(
        width = 9, 
        tags$div(
          style = "margin-top: 28px; margin-left: 10px;",
          tags$h4("Pulse Pressure Over Time with Highlighted Data Points to Excluded"),
          plotlyOutput(ns("PP_plot"), height = "300px"),
          tags$h4("ABP with Beat Markers"),
          plotlyOutput(ns("ABP_plot"), height = "300px"))
      )
    )
  )
}

# ------------------------------------------------------------------
# Server Function for manual Pulse Pressure and ABP Cleaning Module
# ------------------------------------------------------------------
ManualCleanPPServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive call for the hole dataset 
    data_reactive <- reactive({
      req(data_in())
      data_in()
    })
    
    # Reactive values for beats handeling 
    manual_rv <- reactiveValues(
      beats = NULL,     # current working copy of beats
      undo_beats = NULL # copy for undo
    )
    
    # Initializing beats on change in data_in
    observe({
      req(data_reactive()$beats)
      manual_rv$beats <- data_reactive()$beats
    })
    
    # Dynamic updating sliders based on beat timestamp
    observe({
      req(data_reactive()$beats)
      min_time <- floor(min(data_reactive()$beats$time, na.rm = TRUE))
      max_time <- ceiling(max(data_reactive()$beats$time, na.rm = TRUE))
      
      updateSliderInput(
        session, "time_window",
        min = min_time,
        max = max_time,
        value = c(min_time, max_time)
      )
    })
    
    # -------------------------------
    # DATATABLE
    # -------------------------------
    
    # Render datatable
    output$beats_table <- renderDT({
      req(manual_rv$beats)
      datatable(round(manual_rv$beats[, c("time", "PP")], 2), 
                selection = "multiple", 
                options = list(pageLength = 10))
    })
    
    # Remove selected beats from the table (with undo support)
    observeEvent(input$remove_selected, {
      req(input$beats_table_rows_selected)
      selected <- input$beats_table_rows_selected
      
      # Copi for undo
      manual_rv$undo_beats <- manual_rv$beats
      
      # Remove chosen rows
      manual_rv$beats <- manual_rv$beats[-selected, ]
    })
    
    # Undo the last removal of beats
    observeEvent(input$undo_remove, {
      req(manual_rv$undo_beats)
      manual_rv$beats <- manual_rv$undo_beats
      manual_rv$undo_beats <- NULL
    })
    
    # -------------------------------
    # PLOTS
    # -------------------------------
    
    # Plot Pulse Pressure (PP) over time with a highlighted time window and selected points
    output$PP_plot <- renderPlotly({
      req(manual_rv$beats, input$time_window)
      df <- manual_rv$beats
      
      # Blue rectangle showing time window
      pp_range <- range(df$PP, na.rm = TRUE)
      time_rect <- data.frame(
        xmin = input$time_window[1],
        xmax = input$time_window[2],
        ymin = pp_range[1],
        ymax = pp_range[2]
      )
      
      # Mark selected points for exclusion
      selected_rows <- input$beats_table_rows_selected
      df$selected <- "Exclude datapoints: False"
      if (!is.null(selected_rows)) {
        df$selected[selected_rows] <- "Exclude datapoints: True"
      }
      df$selected <- factor(df$selected, levels = c("Exclude datapoints: True", "Exclude datapoints: False"))
      
      p1 <- ggplot(df, aes(x = time, y = PP)) +
        geom_rect(data = time_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = "dodgerblue2", alpha = 0.2, inherit.aes = FALSE) +
        geom_line(color = "grey") +
        geom_point(aes(color = selected), size = 2) +
        scale_color_manual(
          values = c("Exclude datapoints: True" = "red", "Exclude datapoints: False" = "black"),
          name = NULL
        ) +
        theme_minimal() +
        labs(title = "Pulse Pressure (PP = \u25B3 Systole - \u25CB Diastole)", x = "Time [s]", y = "PP [mmHg]")
      
      # Convert to interactive plotly with custom layout
      ggplotly(p1) %>%
        layout(
          xaxis = list(
            title = list(text = "Time [s]", standoff = 5),
            automargin = FALSE
          ),
          margin = list(b = 10),
          legend = list(
            orientation = "h",
            x = 0.5, 
            xanchor = "center",
            y = -0.2, 
            yanchor = "top",
            font = list(size = 10)
          )
        )
    })
    
    
    # Plot ABP waveform within selected time window including systolic, diastolic, and inspiration markers
    output$ABP_plot <- renderPlotly({
      req(data_reactive()$data$sample_pp$abp, manual_rv$beats, input$time_window)
      abp <- data_reactive()$data$sample_pp$abp
      insp <- data_reactive()$data$sample_pp$insp_start
      beats <- manual_rv$beats
      
      p2 <- ggplot() +
        geom_line(data = abp, aes(x = time, y = ABP)) +
        xlim(input$time_window[1], input$time_window[2]) +
        ylim(min(abp$ABP, na.rm = TRUE) - 4, max(abp$ABP, na.rm = TRUE)) +
        geom_point(data = insp, aes(x = time, y = min(abp$ABP, na.rm = TRUE) - 3, shape = "Inspiration start"), size = 2, color = "black") +
        geom_point(data = beats, aes(x = time_systole, y = sys, shape = "Systolic pressure"), color = "black", size = 2, fill = NA) +
        geom_point(data = beats, aes(x = time, y = dia, shape = "Diastolic pressure"), color = "black", size = 2, fill = NA) +
        scale_shape_manual(name = " ", values = c("Systolic pressure" = 2, "Diastolic pressure" = 1, "Inspiration start" = 17)) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        labs(title = "Arterial Blood Pressure (ABP)") + 
        xlab("Time [s]")+
        ylab("ABP [mmHg]")
      
      ggplotly(p2) %>%
        layout(
          xaxis = list(
            title = list(text = "Time [s]", standoff = 5),
            automargin = FALSE
          ),
          margin = list(b = 10),
          legend = list(
            orientation = "h",
            x = 0.5, 
            xanchor = "center",
            y = -0.2, 
            yanchor = "top",
            font = list(size = 10)
          )
        )
    })
    
    # Return the updated dataset with cleaned beats
    return(reactive({
      updated_data <- data_reactive()
      updated_data$beats <- manual_rv$beats
      updated_data
    }))
  })
}
