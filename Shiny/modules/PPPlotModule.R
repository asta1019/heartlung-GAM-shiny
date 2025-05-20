# ================================================
# Modules: PPPlotModule.R
# ================================================
# ------------------------------------------------
# UI Function for Pulse Pressure Plot Module
# ------------------------------------------------
ppGAMPlotModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    h2("Pulse Pressure Plots and GAM Models"),
    
    tags$p(
      "This module allows modeling the heart-lung interaction effect on pulse pressure using Generalized Additive Models (GAM).", tags$br(), tags$br(),
      "The four plots below represent:"
    ),
    tags$ul(
      tags$li("The two upper plots show the raw arterial waveform data and the derived pulse pressures from each heartbeat. Both plots also indicate the respiratory cycles with triangles at the bottom of each of the two figures."),
      tags$li("Bottom left: GAM-derived relationship between pulse pressure and respiratory phase."),
      tags$li("Bottom right: Time trend of pulse pressure from the GAM model.")
    ),
    tags$p(
      tags$b("Note:"), " If no plots appear, try increasing the maximum slider value — there may be too few data points."
    ),
    
    fluidRow(
      column(3,
             sliderInput(ns("time_range"),
                         "Time window (seconds)",
                         min = 0, max = 10, value = c(0, 10), step = 1, width = "100%"
             )
      ),
      column(9,
             div(style = "text-align: right; position: relative; top: 100px;",
                 downloadButton(ns("download_all_plots"), "Download all plots")
             )
      ),
    ),
    # Plot 1: ABP with systole/diastole/inspiration markers
    fluidRow(
      column(12,
             div(class = "card shadow-sm p-3 mb-3",
                 tags$h4("ABP with Beat Markers"),
                 plotOutput(ns("Plot1"), height = "250px")
             )
      )
    ),
    
    # Plot 2: Pulse pressure per beat + respiration markers
    fluidRow(
      column(12,
             div(class = "card shadow-sm p-3 mb-3",
                 tags$h4("Pulse Pressure and Respiratory Cycles"),
                 plotOutput(ns("Plot2"), height = "250px")
             )
      )
    ),
    
    # GAM formula explanation
    fluidRow(
      column(12,
             tags$h4("GAM model", style = "margin-bottom: 10px;"),
             div(
               style = "background-color: white; padding: 10px 10px 10px 10px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-bottom: 15px;",
               withMathJax(
                 div(style = "text-align: center; font-size: 14px;",
                     '\\( PP \\sim s(insp\\_rel\\_index) + s(time) + \\varepsilon \\)'
                 ),
                 div(style = "font-size: 11px; margin-top: 5px;",
                     '\\( \\boldsymbol{insp\\_rel\\_index} \\): The relative position in the respiratory cycle'
                 )
               )
             )
      )
    ),
    
    # Show/hide model code button and box
    fluidRow(
      column(12,
             actionButton(ns("toggle_code"), "Show R-code for the GAM-modelling"),
             hidden(
               div(id = ns("code_box"),
                   div(style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-top: 10px;",
                       tags$pre(HTML(
                         "gam(PP ~ s(insp_rel_index, k = 30, bs = 'cc') + s(time, bs = 'cr'),
                          knots = list(insp_rel_index = c(0, 1)), 
                          method = 'REML',
                          data = PP_data())"
                       ))
                   )
               )
             )
      )
    ),
    
    # PPV explanation and output and GAM plots
    fluidRow(
      column(3,
             div(class = "white-box", style = "background-color: #ffffff; padding: 15px; width: 100%; height: 250px; margin-top: 40px;",
                 withMathJax(
                   tags$div(style = "font-size: 9px;",
                            tags$b(tags$span(style = "font-size: 11px;", "PPV formula:")),
                            helpText("$$PPV = \\frac{\\max(s(\\text{insp_rel_index})) - \\min(s(\\text{insp_rel_index}))}{\\alpha}$$"),
                            tags$ul(
                              tags$li('\\( \\boldsymbol{insp\\_rel\\_index} \\): Beat position in respiratory cycle'),
                              tags$li('\\( \\boldsymbol{\\alpha} \\): Model intercept')
                            ),
                            tags$b(tags$span(style = "font-size: 11px;", "Calculated Pulse Pressure Variation:")),
                            tags$div(style = "margin-top: 10px; font-size: 11px;", textOutput(ns("ppvEstimate")))
                   )
                 )
             )
      ),
      column(9,
             fluidRow(
               column(6,
                      div(class = "card shadow-sm p-2 mb-3",
                          tags$h4("Position in Respiratory Cycle"),
                          plotOutput(ns("gamPlot3"), height = "250px")
                      )
               ),
               column(6,
                      div(class = "card shadow-sm p-2 mb-3",
                          tags$h4("Trend over Time"),
                          plotOutput(ns("gamPlot4"), height = "250px")
                      )
               )
             )
      )
    )
  )
}



# ------------------------------------------------
# Server Function for Pulse Pressure Plot Module
# ------------------------------------------------
ppGAMPlotModuleServer <- function(id, updated_data) {
  moduleServer(id, function(input, output, session) {
    
    #GAM model – R code (expand/collapse)
    observeEvent(input$toggle_code, {
      shinyjs::toggle("code_box")
    })
    
    # Reactive accessors for input data
    beats <- reactive({ updated_data()$beats })
    abp <- reactive({ updated_data()$data$sample_pp$abp })
    insp <- reactive({ updated_data()$data$sample_pp$insp_start })
    
    # Add relative inspiration phase and grouping for cycles
    beats_indexed <- reactive({
      req(beats(), insp())
      add_time_since_event(beats(), time_events = insp()$time, prefix = "insp") %>%
        dplyr::mutate(
          color = "black",
          cycle_group = cumsum(insp_n != dplyr::lag(insp_n, default = first(insp_n)))
        )
    })
    
    # Update time slider based on data
    observeEvent(updated_data(), {
      req(updated_data())
      
      abp_data <- updated_data()$data$sample_pp$abp
      time_min <- floor(min(abp_data$time, na.rm = TRUE))
      time_max <- ceiling(max(abp_data$time, na.rm = TRUE))
      
      updateSliderInput(session, "time_range",
                        min = time_min, max = time_max,
                        value = c(time_min, time_max))
    })
    
    # Filtered data used for modeling
    PP_data <- reactive({
      req(beats_indexed(), input$time_range)
      beats_indexed() %>%
        filter(time >= input$time_range[1], time <= input$time_range[2]) %>%
        select(PP, time, insp_rel_index, insp_n, color)
    })
    
    # Fit a GAM model to PP as a function of respiration cycle and time
    PP_gam <- reactive({
      req(PP_data())
      gam(
        PP ~ s(insp_rel_index, k = 30, bs = "cc") + s(time, bs = "cr"),
        knots = list(insp_rel_index = c(0, 1)),
        method = "REML",
        data = PP_data()
      )
    })
    
    # PPV estimate based on fitted GAM
    ppv_value <- reactive({
      req(PP_gam())
      
      smooth_df <- smooth_estimates(PP_gam(), select = "s(insp_rel_index)", n = 100)
      
      min_PP <- min(smooth_df$.estimate, na.rm = TRUE)
      max_PP <- max(smooth_df$.estimate, na.rm = TRUE)
      intercept_PP <- coef(PP_gam())[1]
      
      if (intercept_PP == 0) return(NA)
      (max_PP - min_PP) / intercept_PP
    })
    
    # Display PPV value as percentage in UI
    output$ppvEstimate <- renderText({
      req(ppv_value())
      sprintf("PPV = %.3f%%", ppv_value() * 100)
    })
    
    # -------------------------------
    # PLOTS
    # -------------------------------
    
    # Plot 1: Arterial blood pressure waveform with event markers
    output$Plot1 <- renderPlot({
      req(abp(), beats(), PP_gam(), input$time_range)
      
      abp_data <- abp() %>%
        filter(time >= input$time_range[1], time <= input$time_range[2])
      insp_data <- insp() %>%
        filter(time >= input$time_range[1], time <= input$time_range[2])
      
      ggplot(abp_data, aes(time, ABP)) +
        geom_line() +
        ylim(min(abp_data$ABP) - 4, max(abp_data$ABP)) +
        geom_point(aes(x = time, y = min(abp_data$ABP) - 3, shape = "Inspiration start"),
                   size = 2, color = "black", data = insp_data) +
        geom_point(aes(x = time_systole, y = sys, shape = "Systole"),
                   color = "black", size = 2, data = beats()) +
        geom_point(aes(x = time, y = dia, shape = "Diastole"),
                   color = "black", size = 2, data = beats()) +
        scale_shape_manual(name = " ",
                           values = c("Diastole" = 1, "Systole" = 2, "Inspiration start" = 17),
                           breaks = c("Diastole", "Systole", "Inspiration start")) +
        geom_hline(yintercept = min(abp_data$ABP) - 4, color = "black") +
        geom_vline(xintercept = 0, color = "black") +
        xlab("Time [S]") +
        ylab("ABP [mmHg]") +
        labs(title = "Arterial Blood Pressure (ABP)") +
        xlim(input$time_range[1], input$time_range[2]) +
        theme(legend.position = "bottom", legend.key = element_blank())
    })
    
    # Plot 2: Pulse Pressure over time
    output$Plot2 <- renderPlot({
      req(beats_indexed(), input$time_range)
      
      filtered_beats <- beats_indexed() %>%
        filter(time >= input$time_range[1], time <= input$time_range[2])
      
      insp_data <- insp() %>%
        filter(time >= input$time_range[1], time <= input$time_range[2])
      
      pp_min <- min(filtered_beats$PP, na.rm = TRUE)
      pp_max <- max(filtered_beats$PP, na.rm = TRUE)
      
      ggplot(filtered_beats, aes(time, PP)) +
        geom_line(aes(group = 1), color = "black", size = 0.5) +
        geom_line(aes(group = cycle_group, color = "black"), size = 1.5) +
        geom_point(aes(color = color), size = 3, show.legend = FALSE) +
        geom_point(aes(x = time, y = pp_min - 0.5),
                   shape = 17, size = 2, color = "black", data = insp_data) +
        labs(title = "Pulse Pressure (PP = \u25B3 Systole - \u25CB Diastole)") +
        scale_color_identity() +
        xlim(input$time_range[1], input$time_range[2]) +
        ylim(pp_min - 1, pp_max + 1)
    })
    
    # Plot 3: GAM smooth over respiration phase
    output$gamPlot3 <- renderPlot({
      req(PP_gam())
      draw(PP_gam(), select = 1, residuals = TRUE, rug = FALSE)
    })
    
    # Plot 4: GAM smooth over time
    output$gamPlot4 <- renderPlot({
      req(PP_gam())
      draw(PP_gam(), select = 2, residuals = TRUE, rug = FALSE)
    })
    
    # -------------------------------
    # DOWNLOAD HANDLERS
    # -------------------------------
    
    output$download_all_plots <- downloadHandler(
      filename = function() {
        paste0("pulse_pressure_plots_", Sys.Date(), ".zip")
      },
      content = function(file) {
        temp_dir <- tempdir()
        
        # Re-build all plots using the same data as in the renderPlot functions
        plot1 <- {
          abp_data <- abp() %>%
            filter(time >= input$time_range[1], time <= input$time_range[2])
          insp_data <- insp() %>%
            filter(time >= input$time_range[1], time <= input$time_range[2])
          
          ggplot(abp_data, aes(time, ABP)) +
            geom_line() +
            ylim(min(abp_data$ABP) - 4, max(abp_data$ABP)) +
            geom_point(aes(x = time, y = min(abp_data$ABP) - 3, shape = "Inspiration start"),
                       size = 2, color = "black", data = insp_data) +
            geom_point(aes(x = time_systole, y = sys, shape = "Systole"),
                       color = "black", size = 2, data = beats()) +
            geom_point(aes(x = time, y = dia, shape = "Diastole"),
                       color = "black", size = 2, data = beats()) +
            scale_shape_manual(name = " ",
                               values = c("Diastole" = 1, "Systole" = 2, "Inspiration start" = 17),
                               breaks = c("Diastole", "Systole", "Inspiration start")) +
            geom_hline(yintercept = min(abp_data$ABP) - 4, color = "black") +
            geom_vline(xintercept = 0, color = "black") +
            xlab("Time [S]") +
            ylab("ABP [mmHg]") +
            labs(title = "Arterial Blood Pressure (ABP)") +
            xlim(input$time_range[1], input$time_range[2]) +
            theme(legend.position = "bottom", legend.key = element_blank())
        }
        
        plot2 <- {
          filtered_beats <- beats_indexed() %>%
            filter(time >= input$time_range[1], time <= input$time_range[2])
          insp_data <- insp() %>%
            filter(time >= input$time_range[1], time <= input$time_range[2])
          
          pp_min <- min(filtered_beats$PP, na.rm = TRUE)
          pp_max <- max(filtered_beats$PP, na.rm = TRUE)
          
          ggplot(filtered_beats, aes(time, PP)) +
            geom_line(aes(group = 1), color = "black", size = 0.5) +
            geom_line(aes(group = cycle_group, color = "black"), size = 1.5) +
            geom_point(aes(color = color), size = 3, show.legend = FALSE) +
            geom_point(aes(x = time, y = pp_min - 0.5),
                       shape = 17, size = 2, color = "black", data = insp_data) +
            labs(title = "Pulse Pressure") +
            scale_color_identity() +
            xlim(input$time_range[1], input$time_range[2]) +
            ylim(pp_min - 1, pp_max + 1)
        }
        
        plot3 <- draw(PP_gam(), select = 1, residuals = TRUE)
        plot4 <- draw(PP_gam(), select = 2, residuals = TRUE)
        
        plot_files <- c("ArterialBloodPressure.png", "PulsePressure.png", "GAM_PP~respiratory phase.png", "GAM_PP~time.png")
        
        # Save PNG-files
        ggsave(file.path(temp_dir, plot_files[1]), plot = plot1, width = 12, height = 2.604, units = "in", dpi = 300)
        ggsave(file.path(temp_dir, plot_files[2]), plot = plot2, width = 12, height = 2.604, units = "in", dpi = 300)
        ggsave(file.path(temp_dir, plot_files[3]), plot = plot3, width = 6, height = 2.604, units = "in", dpi = 300)
        ggsave(file.path(temp_dir, plot_files[4]), plot = plot4, width = 6, height = 2.604, units = "in", dpi = 300)
      
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        
        zip::zip(zipfile = file, files = plot_files)
      }
    )
  })
}