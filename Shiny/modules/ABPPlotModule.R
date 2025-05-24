# ====================================
# Modules: ABPPlotModule.R
# ====================================
# ------------------------------------
# UI Function for ABP Plot Module
# ------------------------------------
abpGAMPlotModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    h2("Arterial Blood Pressure Plots and GAM Models"),
    
    tags$p(
      "Here, the GAM of a full ABP waveform can be calculated and visualised. This modelling takes some time (in the range of minutes).", tags$br(),tags$br(),
      "In the upper figure, the raw ABP waveform is shown and below the four figures plotted show (after finished modelling) the components that the GAM has decomposed the ABP waveform into.",tags$br(),
      "Of these four GAM figures:"
    ),
    tags$ul(
      tags$li("The upper left shows the ABP for the average heartbeat."),
      tags$li("The upper right shows how ABP is generally affected by the respiratory phase."),
      tags$li("The lower left shows the interaction between the cardiac cycle and the respiratory cycle — the average heartbeat depends somewhat on its position in the respiratory cycle."),
      tags$li("The lower right illustrates the slight overall changes in the ABP throughout the selected time frame.")
    ),
    tags$p(
      tags$b("Please note:"), "If no plots appear, try increasing the maximum slider value — there may be too few data points to fit the model properly."
    ),
    
    fluidRow(
      column(12,
             div(class = "card shadow-sm p-3 mb-3", style = "margin-bottom: 10px;",
                 
                 # slider and download butten
                 fluidRow(
                   column(3,
                          sliderInput(ns("plots_time_range"),
                                      "Time window corresponding to all plots (seconds)",
                                      min = 0,
                                      max = 10,
                                      value = c(0, 10),
                                      step = 1,
                                      width = "100%")
                   ),
                   column(9,
                          div(style = "text-align: right; position: relative; top: 120px;",
                              downloadButton(ns("download_abp_plot"), "Download ABP plot")
                          )
                   )
                 ),
                 # plot
                 tags$h4("ABP Signal with Event Markers"),
                 plotOutput(ns("abpPlot"), height = "250px")
             )
      )
    ),
    
    # White box with GAM model
    fluidRow(
      column(
        12,
        tags$h4("GAM model", style = "margin-bottom: 10px;"),
        div(
          style = "background-color: white; padding: 10px 10px 10px 10px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-bottom: 15px;",
          withMathJax(
            tagList(
              div(style = "text-align: center; font-size: 14px;",
                  '\\( ABP \\sim s(P\\_wave\\_index) + s(insp\\_rel\\_index) + ti(P\\_wave\\_index, insp\\_rel\\_index) + s(time) + \\varepsilon \\)'
              ),
              div(style = "font-size: 11px; margin-top: 5px;",
                  '\\( \\boldsymbol{P\\_wave\\_index} \\): The position in the cardiac cycle', tags$br(),
                  '\\( \\boldsymbol{insp\\_rel\\_index} \\): The position in the respiratory cycle'
              )
            )
          )
        )
      )
    ),
    
    
    # GAM model – R code (expand/collapse) 
    fluidRow(
      column(
        12,
        actionButton(ns("toggle_code"), "Show R-code for the GAM-modelling"),
        hidden(
          div(
            id = ns("code_box"),
            div(
              style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-top: 10px;",
              tags$pre(HTML(
                "model <- mgcv::bam(ABP ~ s(P_wave_index, bs = 'cr', k = 40) +
                s(insp_rel_index, bs = 'cc', k = 30) +
                ti(P_wave_index, insp_rel_index, bs = c('cr', 'cc'), k = c(40, 30)) +
                s(time, bs = 'cr'),
                knots = list(insp_rel_index = c(0, 1)),
                gamma = 5,
                data = abp_data(),
                discrete = FALSE,
                nthreads = 6)"
              ))
            )
          )
        )
      )
    ),
    
    # Trigger for fitting the GAM model
    fluidRow(
      column(12, 
             br(),
             tags$p(
               tags$b("Please note:"), "The GAM animation may take a few minutes to generate. Please be patient. Once started, the process cannot be interrupted or stopped."
             ),
             
             actionButton(ns("fitGAMButton"), "Fit GAM Model"),
             downloadButton(ns("download_gam_plots"), "Download GAM plots", style = "margin-left: 5px;"))
    ),
    
    
    
    # GAM plot
    fluidRow(
      column(6,
             div(class = "card shadow-sm p-2 mb-3",
                 tags$h4("Position in cardiac cycle"),
                 plotOutput(ns("gamPlot1"), height = "250px"))
      ),
      column(6,
             div(class = "card shadow-sm p-2 mb-3",
                 tags$h4("Position in respiratory cycle"),
                 plotOutput(ns("gamPlot2"), height = "250px"))
      ),
      column(6,
             div(class = "card shadow-sm p-2 mb-3",
                 tags$h4("Interaction between cardiac and respiratory cycle"),
                 plotOutput(ns("gamPlot3"), height = "250px"))
      ),
      column(6,
             div(class = "card shadow-sm p-2 mb-3",
                 tags$h4("Trend over time"),
                 plotOutput(ns("gamPlot4"), height = "250px"))
      )
    )
  )
}

# ------------------------------------
# Server Function for ABP Plot Module
# ------------------------------------
abpGAMPlotModuleServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    
    # GAM model – R code (expand/collapse)
    observeEvent(input$toggle_code, {
      shinyjs::toggle("code_box")
    })
    
    # Download button for GAM plots
    shinyjs::disable("download_gam_plots")
    
    # Reactive expression to extract ABP time series from input data
    time <- reactive({
      req(data_in())
      data_in()$data$sample_pp$abp
    })
    
    # Update the time range slider whenever new data is loaded
    observeEvent(data_in(), {
      abp <- data_in()$data$sample_pp$abp
      updateSliderInput(session, "plots_time_range", 
                        min = round(min(abp$time, na.rm = TRUE)), 
                        max = round(max(abp$time, na.rm = TRUE)), 
                        value = round(range(abp$time, na.rm = TRUE)))
    })
    
    # Reactive expression to adjust ABP data using inspiration and QRS markers
    abp_data_unfiltered <- reactive({
      req(data_in())
      adjust_abp_insp_index(
        abp = data_in()$data$sample_pp$abp,
        cvp = data_in()$data$sample_cvp$cvp,
        qrs_times = data_in()$data$sample_cvp$qrs$time,
        insp_start_times = data_in()$data$sample_cvp$insp_start$time
      )
    })
    
    # Configuration for GAM model
    set_discrete <- FALSE
    set_nthreads <- 6
    
    # Filter ABP data according to selected time range
    abp_data <- reactive({
      req(abp_data_unfiltered(), input$plots_time_range)
      df <- abp_data_unfiltered()
      df[df$time >= input$plots_time_range[1] & df$time <= input$plots_time_range[2], ]
    })

    
    # Fit GAM to ABP data when user clicks the fit button
    gam_abp <- eventReactive(input$fitGAMButton, {
      withProgress(message = "Fitting GAM model...", {
        shinyjs::show("loading")
        
        model <- bam(
          ABP ~ s(P_wave_index, bs = "cr", k = 40) +
            s(insp_rel_index, bs = "cc", k = 30) +
            ti(P_wave_index, insp_rel_index, bs = c("cr", "cc"), k = c(40, 30)) +
            s(time, bs = "cr"),
          knots = list(insp_rel_index = c(0, 1)),
          gamma = 5,
          data = abp_data(),
          discrete = set_discrete,
          nthreads = set_nthreads
        )
        
        shinyjs::hide("loading")
        model
      })
    })
    
    # -------------------------
    # PLOTTING FUNCTIONS
    # -------------------------
    
    # Plot observed ABP data with annotations for events
    generate_abp_plot <- function(data, time_range) {
      filtered_data <- data %>%
        filter(time >= time_range[1], time <= time_range[2])
      
      insp_points <- data_in()$data$sample_cvp$insp_start %>%
        filter(time >= time_range[1], time <= time_range[2])
      
      qrs_points <- data_in()$data$sample_cvp$qrs %>%
        filter(time >= time_range[1], time <= time_range[2])
      
      fluid_start <- data_in()$data$sample_cvp$fluid_start
      fluid_end <- data_in()$data$sample_cvp$fluid_end
      
      has_intervention <- !is.null(fluid_start) && !is.null(fluid_end) &&
        !anyNA(fluid_start) && !anyNA(fluid_end)
      
      cvp_range <- range(filtered_data$CVP, na.rm = TRUE)
      
      # Highlight intervention period
      if (has_intervention) {
        intervention_times <- data.frame(
          xmin = fluid_start,
          xmax = fluid_end,
          ymin = min(filtered_data$ABP, na.rm = TRUE),
          ymax = max(filtered_data$ABP, na.rm = TRUE)
        )
      }
      
      p <- ggplot(filtered_data, aes(time, ABP))
      
      plot_layers <- list()
      
      if (has_intervention) {
        plot_layers <- append(plot_layers, list(
          geom_rect(data = intervention_times,
                    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "Intervention"),
                    alpha = 0.6, inherit.aes = FALSE)
          
        ))
      }
      
      # Add signal line, inspiration and QRS markers, etc.
      plot_layers <- append(plot_layers, list(
        geom_line(), 
        labs(title = "Observed ABP Signal"), 
        ylab("ABP [mmHg]"), 
        xlab("Time [s]"), 
        geom_point(data = insp_points, aes(x = time, y = min(filtered_data$ABP) - 8, shape = "Inspiration start"),
                   size = 2, color = "black"), 
        geom_point(data = qrs_points, aes(x = time, y = max(filtered_data$ABP) + 10, shape = "QRS complex"),
                   size = 2, color = "black"),
        scale_shape_manual(name = " ", values = c("Inspiration start" = 17, "QRS complex" = 16)),
        theme_minimal(),
        ylim(min(filtered_data$ABP) - 15, max(filtered_data$ABP) + 15),
        theme(legend.position = "bottom", legend.key = element_blank()),
        scale_x_continuous(limits = c(time_range[1], time_range[2])),
        scale_fill_manual(name = " ", values = c("Intervention" = "lightblue")),
        theme(legend.position = "bottom", legend.key = element_blank())
        ))
        
        # Apply all plot layers
        for (layer in plot_layers) {
          p <- p + layer
        }
      
      return(p)
    }
    
    # Plot 1: Effect of cardiac cycle on ABP
    generate_gam_plot1 <- function(model, data, time_range) {
      filtered <- data %>% filter(time >= time_range[1], time <= time_range[2])
      new_data <- data.frame(
        P_wave_index = range(filtered$P_wave_index, na.rm = TRUE),
        insp_rel_index = mean(filtered$insp_rel_index, na.rm = TRUE),
        time = mean(filtered$time, na.rm = TRUE)
      )
      y_vals <- predict(model, newdata = new_data, type = "terms")[, "s(P_wave_index)"]
      gratia::draw(model, select = 1, residuals = TRUE, rug = FALSE) +
        theme_minimal() +
        labs(title = "Position in the Cardiac Cycle", x = "Position in cardiac cycle (relative to P wave)", y = "Partial Effect on ABP [mmHg]") +
        geom_point(aes(x = new_data$P_wave_index[1], y = y_vals[1] - 0.15), shape = 16, size = 3) +
        geom_point(aes(x = new_data$P_wave_index[2], y = y_vals[2] - 0.15), shape = 16, size = 3)
    }
    
    #  Plot 2: Effect of respiratory cycle on ABP
    generate_gam_plot2 <- function(model, data, time_range) {
      filtered <- data %>% filter(time >= time_range[1], time <= time_range[2])
      new_data <- data.frame(
        P_wave_index = mean(filtered$P_wave_index, na.rm = TRUE),
        insp_rel_index = range(filtered$insp_rel_index, na.rm = TRUE),
        time = mean(filtered$time, na.rm = TRUE)
      )
      y_vals <- predict(model, newdata = new_data, type = "terms")[, "s(insp_rel_index)"]
      gratia::draw(model, select = 2, residuals = TRUE, rug = FALSE) +
        theme_minimal() +
        labs(title = "Position in the Respiratory Cycle", x = "Position in respiratory cycle (relative to Inspiration Start)", y = "Partial Effect on ABP [mmHg]") +
        geom_point(aes(x = new_data$insp_rel_index[1], y = y_vals[1] + 0.02), shape = 17, size = 3) +
        geom_point(aes(x = new_data$insp_rel_index[2], y = y_vals[2] + 0.02), shape = 17, size = 3)
    }
    
    # Plot 3: Interaction effect between cardiac and respiratory cycles
    generate_gam_plot3 <- function(model) {
      gratia::draw(model, select = 3, residuals = TRUE, rug = FALSE) +
        theme_minimal() +
        labs(title = "Interaction between Cardiac and Respiratory Cycles", 
             subtitle = "Contour heights represent Partial Effect on ABP [mmHg]",
             fill = "Partial Effect on ABP [mmHg]") +
        xlab("Position in cardiac cycle (relative to P wave)") +
        ylab("Respiratory cycle (relative to Inspiration Start)") +
        theme(legend.position = "right") +
        geom_label_contour(aes(z = .estimate, label = label_number(accuracy = 0.1)(..level..)),
                           bins = 10, size = 3, color = "black", fill = "white",
                           label.padding = unit(0.1, "lines"))
    }
    
    # Plot 4: Effect of time on ABP
    generate_gam_plot4 <- function(model) {
      p <- gratia::draw(model, select = 4, residuals = TRUE, rug = FALSE, partial_match = TRUE)[[1]]
      p$layers <- p$layers[!sapply(p$layers, function(layer) inherits(layer$geom, "GeomRibbon"))]
      p + theme_minimal() + labs(title = "Partial Effect of Time", x = "Time [s]", y = "Partial Effect on ABP [mmHg]")
    }
    
    # -------------------------
    # RENDER PLOTS
    # ------------------------- 
    
    output$abpPlot <- renderPlot({
      shinyjs::show("loading")
      req(abp_data(), input$plots_time_range)
      time_range <- input$plots_time_range
      abp_plot <- generate_abp_plot(abp_data(), time_range)
      shinyjs::hide("loading")
      abp_plot
    })
    
    output$abpPlot <- renderPlot({
      shinyjs::show("loading")
      req(abp_data(), input$plots_time_range)
      abp_plot <- generate_abp_plot(abp_data(), input$plots_time_range)
      shinyjs::hide("loading")
      abp_plot
    })
    
    output$gamPlot1 <- renderPlot({ req(gam_abp()); generate_gam_plot1(gam_abp(), abp_data(), input$plots_time_range) })
    output$gamPlot2 <- renderPlot({ req(gam_abp()); generate_gam_plot2(gam_abp(), abp_data(), input$plots_time_range) })
    output$gamPlot3 <- renderPlot({ req(gam_abp()); generate_gam_plot3(gam_abp()) })
    output$gamPlot4 <- renderPlot({ req(gam_abp()); generate_gam_plot4(gam_abp()) })
    
    # -------------------------
    # DOWNLOAD HANDLERS
    # -------------------------
    
    # Enable download after GAM model is available
    observe({
      req(gam_abp())
      shinyjs::enable("download_gam_plots")
    })
    
    output$download_abp_plot <- downloadHandler(
      filename = function() {
        paste0("abp_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        time_range <- input$plots_time_range
        plot <- generate_abp_plot(abp_data(), time_range)
        ggsave(file, plot = plot, width = 10, height = 4, dpi = 300)
      }
    )
    
    output$download_gam_plots <- downloadHandler(
      filename = function() {
        paste0("arterial_blood_pressure_gam_plots_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(gam_abp())
        temp_dir <- tempdir()
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        
        plots <- list(
          generate_gam_plot1(gam_abp(), abp_data(), input$plots_time_range),
          generate_gam_plot2(gam_abp(), abp_data(), input$plots_time_range),
          generate_gam_plot3(gam_abp()),
          generate_gam_plot4(gam_abp())
        )
        
        filenames <- c(
          "position_in_the_cardiac_cycle.png",
          "position_in_the_respiratory_cycle.png",
          "interaction_cardiac_respiration.png",
          "effect_of_time.png"
        )
        
        for (i in seq_along(plots)) {
          ggsave(filename = filenames[i], plot = plots[[i]], width = 6, height = 4, dpi = 300)
        }
        
        zip::zip(zipfile = file, files = filenames)
      }
    )
  })
}
