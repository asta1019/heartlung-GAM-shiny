# =============================================================
# Modules: PPCleaningModule.R
# =============================================================
# -------------------------------------------------------------
# UI Function for Pulse Pressure and ABP Cleaning Module
# -------------------------------------------------------------
CleanPPModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("Pulse Pressure Data Cleaning - Preselected"),
    tags$p("Pulse pressure data points can be removed using the preselect button. Potential outliers are automatically flagged and colored based on their deviation from the local data pattern."),
    
    fluidRow(
      column(12,
             div(class = "card shadow-sm p-3 mb-3",
                 # time slider and outlier checkbox
                 fluidRow(
                   column(3,
                          sliderInput(ns("time_window"),
                                      "Time window (seconds)",
                                      min = 0,
                                      max = 10,
                                      value = c(0, 10),
                                      step = 1,
                                      width = "100%")
                   ),
                   column(3, offset = 6,
                          div(style = "text-align: right; position: relative; top: 100px;",
                              tags$style(HTML("
                              .checkbox input[type='checkbox'] {
                                width: 15px;
                                height: 15px;
                              }
                            ")),
                              checkboxInput(ns("filter_pp"),
                                            HTML("<b style='font-size:16px;'>Remove preselcted outliers</b>"),
                                            value = FALSE
                              )
                          )
                   )
                   
                   )
                 ),
                 
                 tags$h4("Pulse Pressure Over Time with Highlighted Excluded Data Points"),
                 plotlyOutput(ns("PP_plot"), height = "300px"),
                 
                 tags$h4("ABP with Beat Markers"),
                 plotlyOutput(ns("ABP_plot"), height = "300px")
             )  
      )  
    )
}

                 


# -------------------------------------------------------------
# Server Function for Pulse Pressure and ABP Cleaning Module
# -------------------------------------------------------------
CleanPPModuleServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    data_reactive <- reactive({
      req(data_in$data)
      data_in$data
    })
    
    # ReactiveValues container for beats
    beats_rv <- reactiveValues(
      cleaned_beats = NULL,
      filtered_beats = NULL,
      selected_beats = NULL
    )
    
    # Preselection for PP-outliers
    observe({
      req(data_reactive()$beats)
      
      df <- data_reactive()$beats
      req(df)
      
      df$is_outlier <- NA
      
      n <- nrow(df)
      
      for (i in seq_len(n)) {
        # Define moving window (max 10 points, centered around current)
        if (i < 5) {
          window_indices <- 1:10
        } else if (i > (n - 5)) {
          window_indices <- (n - 9):n
        } else {
          window_indices <- (i - 4):(i + 5)
        }
        
        # compute local median and flag if deviation > 7 mmHg
        median_PP <- median(df$PP[window_indices], na.rm = TRUE)
        df$is_outlier[i] <- abs(median_PP - df$PP[i]) > 7
      }
      
      beats_rv$cleaned_beats <- df
      beats_rv$filtered_beats <- df[!df$is_outlier, ]
    })
    
    # filtered and unfiltered beats based on checkbox
    observe({
      req(beats_rv$cleaned_beats, beats_rv$filtered_beats)
      
      beats_rv$selected_beats <- if (isTRUE(input$filter_pp)) {
        beats_rv$filtered_beats
      } else {
        beats_rv$cleaned_beats
      }
    })
    
    # Dynamic update on sliders 
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
    # PLOTS
    # -------------------------------
    
    #Pulse Pressure over time with highlighted outliers
    output$PP_plot <- renderPlotly({
      req(beats_rv$selected_beats, beats_rv$cleaned_beats, input$time_window)
      
      df <- beats_rv$selected_beats
      cleaned_df <- beats_rv$cleaned_beats
      
      df$highlight_outlier <- cleaned_df$is_outlier[match(df$time, cleaned_df$time)]
      df$highlight_outlier[is.na(df$highlight_outlier)] <- FALSE
      
      df$excluded_label <- factor(
        ifelse(df$highlight_outlier, "Exclude datapoints: True", "Exclude datapoints: False"),
        levels = c("Exclude datapoints: True", "Exclude datapoints: False")
      )
      
      pp_range <- range(df$PP, na.rm = TRUE)
      
      time_rect <- data.frame(
        xmin = input$time_window[1],
        xmax = input$time_window[2],
        ymin = pp_range[1],
        ymax = pp_range[2]
      )
      
      p1 <- ggplot(df, aes(x = time, y = PP)) +
        geom_rect(data = time_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = "dodgerblue2", alpha = 0.2, inherit.aes = FALSE) +
        geom_line(color = "grey") +
        geom_point(aes(color = excluded_label)) +
        scale_color_manual(
          values = c("Exclude datapoints: True" = "red", "Exclude datapoints: False" = "black"),
          name = NULL
        ) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.key = element_blank()) +
        labs(title = "Pulse Pressure (PP = \u25B3 Systole - \u25CB Diastole)", color = "Excluded datapoints") +
        xlab(" Time [s]") +
        ylab("PP [mmHg]")
        
      
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
    
    
    # ABP waveform with systolic/diastolic beat markers and inspiration points
    output$ABP_plot <- renderPlotly({
      req(data_reactive()$data$sample_pp$abp, input$time_window)
      
      abp <- data_reactive()$data$sample_pp$abp
      beats <- beats_rv$selected_beats
      insp <- data_reactive()$data$sample_pp$insp_start
      
      req(abp, beats, insp)
      
      p2 <- ggplot() +
        geom_line(data = abp, aes(x = time, y = ABP)) +
        xlim(input$time_window[1], input$time_window[2]) +
        ylim(min(abp$ABP, na.rm = TRUE) - 4, max(abp$ABP, na.rm = TRUE)) +
        geom_point(data = insp, aes(x = time, y = min(abp$ABP, na.rm = TRUE) - 3, shape = "Inspiration start"), size = 2, color = "black") +
        geom_point(data = beats, aes(x = time_systole, y = sys, shape = "Systolic pressure"), color = "black", size = 2, fill = NA) +
        geom_point(data = beats, aes(x = time, y = dia, shape = "Diastolic pressure"), color = "black", size = 2, fill = NA) +
        scale_shape_manual(name = " ", values = c("Systolic pressure" = 2, "Diastolic pressure" = 1, "Inspiration start" = 17)) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.key = element_blank()) +
        labs(title = "Arterial Blood Pressure (ABP)") +
        xlab("Time [s]") +
        ylab("ABP [mmHg]")
      
      # Convert to interactive plot
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
    
    # Return the updated dataset (with selected beats) back to caller
    return(reactive({
      req(beats_rv$cleaned_beats)
      
      updated_data <- data_reactive()  # Start with the original input data
      
      # Update the 'beats' part of the data with the cleaned/filtered beats
      updated_data$beats <- beats_rv$selected_beats  
      updated_data
    }))
  })
}

