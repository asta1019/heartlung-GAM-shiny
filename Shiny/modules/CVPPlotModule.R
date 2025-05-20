# =====================================
# Modules: CVPPlotModule.R
# =====================================
# -------------------------------------
# UI Function for CVP Plot Module
# -------------------------------------
cvpGAMPlotModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    h2("Central Venous Pressure Plots and GAM Models"),
    
    tags$p(
      "Here, the GAM of a full CVP waveform can be calculated and visualised. This modelling takes some time (in the range of minutes).", tags$br(),tags$br(),
      "In the upper figure, the raw CVP waveform is shown along with ECG and CVP plots corresponding to the selected time range via the sliders.", tags$br(),
      "The four figures plotted below show (after finished modelling) the components that the GAM has decomposed the CVP waveform into.",tags$br(),
      "Of these four GAM figures:"
    ),
    tags$ul(
      tags$li("The upper left shows the CVP for the average heartbeat."),
      tags$li("The upper right shows how CVP is generally affected by the respiratory phase."),
      tags$li("The lower left shows the interaction between the cardiac cycle and the respiratory cycle — the average heartbeat depends somewhat on its position in the respiratory cycle."),
      tags$li("The lower right illustrates the slight overall changes in the CVP throughout the selected time frame.")
    ),
    tags$p(
      tags$b("Please note:"), " If no plots appear, try increasing the maximum slider value — there may be too few data points to fit the model properly."
    ),
    
    # Time range slider and download button row
    fluidRow(
      column(12,
             div(class = "card shadow-sm p-3 mb-3", style = "margin-bottom: 10px;",
                 
          
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
                              downloadButton(ns("download_cvp_and_ecg_plots"), "Download CVP and ECG plots")
                          )
                   )
                 ),
                 
                 tags$h4("CVP Signal with Event Markers"),
                 plotOutput(ns("TotalcvpPlot"), height = "250px")
             )  
      )  
    ),
    
    
    # Side-by-side CVP and ECG plots for selected time range
    fluidRow(
      column(6,
             div(class = "card shadow-sm p-3 mb-3",
                 tags$h5("CVP Signal within Selected Time Range"),
                 plotOutput(ns("cvpPlot"), height = "250px"))
      ),
      column(6,
             div(class = "card shadow-sm p-3 mb-3",
                 tags$h5("ECG Signal within Selected Time Range"),
                 plotOutput(ns("ecgPlot"), height = "250px"))
      )
    ),
    
    br(),
    
    # Section with GAM model formula
    fluidRow(
      column(
        12,
        tags$h4("GAM model", style = "margin-bottom: 10px;"),
        div(
          style = "background-color: white; padding: 10px 10px 10px 10px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-bottom: 15px;",
          withMathJax(
            tagList(
              div(style = "text-align: center; font-size: 14px;",
                  '\\( CVP \\sim s(P\\_wave\\_index) + s(insp\\_rel\\_index) + ti(P\\_wave\\_index, insp\\_rel\\_index) + s(time) + \\varepsilon \\)'
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
    
    
    # Show/hide model code button and box
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
                "model <- mgcv::bam(CVP ~ s(P_wave_index, bs = 'cr', k = 40) +
                s(insp_rel_index, bs = 'cc', k = 30) +
                ti(P_wave_index, insp_rel_index, bs = c('cr', 'cc'), k = c(40, 30)) +
                s(time, bs = 'cr'),
                knots = list(insp_rel_index = c(0, 1)),
                gamma = 5,
                data = cvp_data(),
                discrete = FALSE,
                nthreads = 6)"
              ))
            )
          )
        )
      )
    ),
    
    # GAM fitting button for fitting the GAM model
    fluidRow(
      column(12,
             br(),  # Tilføjer en tom linje over teksten
             tags$p(
               tags$b("Please note:"), "The GAM animation may take a few minutes to generate. Please be patient. Once started, the process cannot be interrupted or stopped."
             ),
             actionButton(ns("fitGAMButton"), "Fit GAM Model", style = "margin-top: 10px;"),
             downloadButton(ns("download_gam_plots"), "Download GAM plots", style = "margin-left: 5px; margin-top: 10px;"))
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

# -------------------------------------
# Server Function for CVP Plot Module
# -------------------------------------
cvpGAMPlotModuleServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    
    # GAM model – R code (expand/collapse)
    observeEvent(input$toggle_code, {
      shinyjs::toggle("code_box")
    })
    
    # Download button for GAM plots
    shinyjs::disable("download_gam_plots")
    
    
    # Reactive expression to extract CVP-data
    time <- reactive({
      req(data_in())
      data_in()$data$sample_cvp$cvp
    })
    
    # Reactive expression to extract ECG-data
    ecg_data <- reactive({
      req(data_in())
      data_in()$data$sample_cvp$ecg
    })
    
    # Update the time range slider
    observeEvent(data_in(), {
      req(data_in())
      cvp <- data_in()$data$sample_cvp
      
      # Check if fluid_start exists and is valid
      if (isTRUE(!is.na(cvp$fluid_start[1]))) {
        min_time <- round(min(cvp$cvp$time, na.rm = TRUE))
        max_time <- round(max(cvp$cvp$time, na.rm = TRUE))
        
        # Set slider values around fluid start event
        min_fluid <- round(cvp$fluid_start - 30)
        max_fluid <- round(cvp$fluid_start)
        
        # Update slider input for time range selection
        updateSliderInput(session, "plots_time_range", 
                          min = min_time, 
                          max = max_time, 
                          value = c(min_fluid, max_fluid))
        
      } else {
        # Default slider range to entire CVP time range if no fluid_start
        min_time <- round(min(cvp$cvp$time, na.rm = TRUE))
        max_time <- round(max(cvp$cvp$time, na.rm = TRUE))
        
        updateSliderInput(session, "plots_time_range", 
                          min = min_time, 
                          max = max_time, 
                          value = c(min_time, max_time))
      }
    })
    
    # Reactive expression for unfiltered CVP data adjusted for inspiration index
    cvp_data_unfiltered <- reactive({
      req(data_in())
      
      cvp <- data_in()$data$sample_cvp$cvp
      qrs <- data_in()$data$sample_cvp$qrs$time
      insp <- data_in()$data$sample_cvp$insp_start$time
      
      # Adjust CVP data based on inspiration and QRS timings
      adjust_cvp_insp_index(cvp, qrs, insp)
    })
    
    #########################################################
    #observe({
    #  cores <- parallel::detectCores()
    #  nthreads <- max(1, floor(cores * 0.75))
    #  
    #  # Print til R-konsollen (serverens konsol)
    #  print(paste("Antal kerner fundet:", cores))
    #  print(paste("Bruger nthreads sat til:", nthreads))
    #})
    #########################################################
    
    # Filtered by the sliders 
    cvp_data <- reactive({
      req(cvp_data_unfiltered(), input$plots_time_range)
      df <- cvp_data_unfiltered()
      df[df$time >= input$plots_time_range[1] & df$time <= input$plots_time_range[2], ]
    })
  
    # Configuration flags for GAM fitting
    set_discrete <- FALSE
    set_nthreads <- 6
    
    # Fit the GAM model when the user clicks the fit button
    gam_cvp <- eventReactive(input$fitGAMButton, {
      req(cvp_data())
      
      if (nrow(cvp_data()) < 100) {
        showNotification("Not enough data points to fit GAM model", type = "error")
        return(NULL)
      }
      
      withProgress(message = "Fitting GAM model...", {
        tryCatch({
          model <- bam(
            CVP ~ s(P_wave_index, bs = "cr", k = 40) +
              s(insp_rel_index, bs = "cc", k = 30) +
              ti(P_wave_index, insp_rel_index, bs = c("cr", "cc"), k = c(40, 30)) +
              s(time, bs = "cr"),
            knots = list(insp_rel_index = c(0, 1)),
            gamma = 5,
            data = cvp_data(),
            discrete = set_discrete,
            nthreads = set_nthreads
          )
          return(model)
        }, error = function(e) {
          showNotification(paste("GAM fitting failed:", e$message), type = "error")
          return(NULL)
        })
      })
    })
    
    # Function to generate total CVP plot with highlights and intervention shading
    generate_total_cvp_plot <- function(data, time_highlight_range, full_time_range) {
      filtered_data <- data %>%
        filter(time >= full_time_range[1], time <= full_time_range[2])
      
      insp_points <- data_in()$data$sample_cvp$insp_start %>%
        filter(time >= full_time_range[1], time <= full_time_range[2])
      
      qrs_points <- data_in()$data$sample_cvp$qrs %>%
        filter(time >= full_time_range[1], time <= full_time_range[2])
      
      cvp_range <- range(filtered_data$CVP, na.rm = TRUE)
      
      # Sliderinput for GAM (lightblue area)
      time_rect <- data.frame(
        xmin = time_highlight_range[1],
        xmax = time_highlight_range[2],
        ymin = cvp_range[1],
        ymax = cvp_range[2]
      )
      
      # Check if intervention start and end times exist and are valid
      fluid_start <- data_in()$data$sample_cvp$fluid_start
      fluid_end <- data_in()$data$sample_cvp$fluid_end
      
      has_intervention <- !is.null(fluid_start) && !is.null(fluid_end) &&
        !anyNA(fluid_start) && !anyNA(fluid_end)
      
      if (has_intervention) {
        # Define intervention time rectangle for shadin
        intervention_times <- data.frame(
          xmin = fluid_start,
          xmax = fluid_end,
          ymin = cvp_range[1],
          ymax = cvp_range[2]
        )
      }
      
      # -------------------------
      # PLOTTING FUNCTIONS
      # -------------------------
      
      # Start ggplot
      p <- ggplot(filtered_data, aes(x = time, y = CVP))
      
      # Create a list to hold different ggplot layers
      plot_layers <- list()
      
      # Add intervention shading layer if intervention is active
      if (has_intervention) {
        plot_layers <- append(plot_layers, list(
          geom_rect(data = intervention_times,
                    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "Intervention"),
                    alpha = 0.6, inherit.aes = FALSE)
          
        ))
      }
      
      # Add time highlight rectangle and other plot layers
      plot_layers <- append(plot_layers, list(
        geom_rect(data = time_rect,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = "dodgerblue2", alpha = 0.4, inherit.aes = FALSE),
        
        geom_line(),
        
        # Plot inspiration start points below CVP curve
        geom_point(data = insp_points,
                   aes(x = time, y = min(filtered_data$CVP, na.rm = TRUE) - 0.6, shape = "Inspiration start"),
                   size = 2, color = "black"),
        
        # Plot QRS complex points above CVP curve
        geom_point(data = qrs_points,
                   aes(x = time, y = max(filtered_data$CVP, na.rm = TRUE) + 1, shape = "QRS complex"),
                   size = 2, color = "black"),
        
        scale_shape_manual(name = " ", values = c("Inspiration start" = 17, "QRS complex" = 16)),
        
        scale_fill_manual(name = " ", values = c("Intervention" = "lightblue")),
        
        labs(title = "Observed CVP Signal (Static)", y = "CVP [mmHg]", x = "Time [s]"),
        
        theme_minimal(),
        
        theme(legend.position = "bottom", legend.key = element_blank())
      ))
      
      # Add all layers to the plot
      for (layer in plot_layers) {
        p <- p + layer
      }
      
      return(p)
      
    }
    
    # Function to generate ECG plot for given time range
    generate_ecg_plot <- function(data, time_range) {
      filtered_data <- data %>%
        filter(time >= time_range[1], time <= time_range[2])
      
      if (nrow(filtered_data) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No ECG data in selected time range") +
                 theme_void())
      }
      
      # Determine which ECG column to use for plotting
      ecg_col <- if ("ECG_II" %in% names(filtered_data)) "ECG_II" else names(filtered_data)[2]
      
      # Generate line plot for ECG signal
      ggplot(filtered_data, aes_string(x = "time", y = ecg_col)) +
        geom_line() +
        labs(title = "ECG Signal", y = "Amplitude [mV]", x = "Time [s]") +
        scale_x_continuous(limits = c(time_range[1], time_range[2]))
    }
    
    # Function to generate CVP plot for given time range with markers
    generate_cvp_plot <- function(data, time_range) {
      filtered_data <- data %>%
        filter(time >= time_range[1], time <= time_range[2])
      
      insp_points <- data_in()$data$sample_cvp$insp_start %>%
        filter(time >= time_range[1], time <= time_range[2])
      
      qrs_points <- data_in()$data$sample_cvp$qrs %>%
        filter(time >= time_range[1], time <= time_range[2])
      
      ggplot(filtered_data, aes(x = time, y = CVP)) +
        geom_line() +
        labs(title = "Observed CVP Signal", y = "CVP [mmHg]", x = "Time [s]") +
        geom_hline(yintercept = min(filtered_data$CVP, na.rm = TRUE) - 1, color = "black") +
        geom_point(data = insp_points,
                   aes(x = time, y = min(filtered_data$CVP, na.rm = TRUE) - 0.6, shape = "Inspiration start"),
                   size = 2, color = "black") +
        geom_point(data = qrs_points,
                   aes(x = time, y = max(filtered_data$CVP, na.rm = TRUE) + 1, shape = "QRS complex"),
                   size = 2, color = "black") +
        scale_shape_manual(name = " ", values = c("Inspiration start" = 17, "QRS complex" = 16)) +
        theme(legend.position = "bottom", legend.key = element_blank()) +
        scale_x_continuous(limits = c(time_range[1], time_range[2]))
    }
    
    # Plot 1: Effect of cardiac cycle on CVP
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
        labs(title = "Position in the Cardiac Cycle", x = "Index to QRS (relative)", y = "Partial CVP [mmHg]") +
        geom_point(aes(x = new_data$P_wave_index[1], y = y_vals[1] - 0.15), shape = 16, size = 3) +
        geom_point(aes(x = new_data$P_wave_index[2], y = y_vals[2] - 0.15), shape = 16, size = 3)
    }
    
    #  Plot 2: Effect of respiratory cycle on CVP
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
        labs(title = "Position in the Respiratory Cycle", x = "Index to Inspiration Start (relative)", y = "Partial CVP [mmHg]") +
        geom_point(aes(x = new_data$insp_rel_index[1], y = y_vals[1] + 0.02), shape = 17, size = 3) +
        geom_point(aes(x = new_data$insp_rel_index[2], y = y_vals[2] + 0.02), shape = 17, size = 3)
    }
    
    # Plot 3: Interaction effect between cardiac and respiratory cycles
    generate_gam_plot3 <- function(model) {
      gratia::draw(model, select = 3, residuals = FALSE, rug = FALSE) +
        theme_minimal() +
        labs(title = "Interaction between Cardiac and Respiratory Cycles", 
             subtitle = "Contour heights represent Partial CVP [mmHg]",
             fill = "Partial Effect on CVP") +
        xlab("Index to QRS (relative)") +
        ylab("Index to Inspiration Start (relative)") +
        theme(legend.position = "right") +
        geom_label_contour(aes(z = .estimate, label = label_number(accuracy = 0.1)(..level..)),
                           bins = 10, size = 3, color = "black", fill = "white",
                           label.padding = unit(0.1, "lines"))
    }
    
    # Plot 4: Effect of time on CVP
    generate_gam_plot4 <- function(model) {
      p <- gratia::draw(model, select = 4, residuals = TRUE, rug = FALSE, partial_match = TRUE)[[1]]
      p$layers <- p$layers[!sapply(p$layers, function(layer) inherits(layer$geom, "GeomRibbon"))]
      p + theme_minimal() + labs(title = "Effect of Time", x = "Time [s]", y = "Partial CVP [mmHg]")
    }
    
    # -------------------------
    # RENDER PLOTS
    # -------------------------
    
    output$TotalcvpPlot <- renderPlot({
      shinyjs::show("loading")
      req(cvp_data_unfiltered())  
      req(input$plots_time_range)
      
      generate_total_cvp_plot(
        data = cvp_data_unfiltered(),
        time_highlight_range = input$plots_time_range,
        full_time_range = range(cvp_data_unfiltered()$time, na.rm = TRUE)
      )
    })
    
    output$ecgPlot <- renderPlot({
      shinyjs::show("loading")
      req(ecg_data(), input$plots_time_range)
      time_range <- input$plots_time_range
      ecg_plot <- generate_ecg_plot(ecg_data(), time_range)
      shinyjs::hide("loading")
      ecg_plot
    })
    
    output$cvpPlot <- renderPlot({
      shinyjs::show("loading")
      req(cvp_data(), input$plots_time_range)
      time_range <- input$plots_time_range
      cvp_plot_interactive <- generate_cvp_plot(cvp_data(), time_range)
      shinyjs::hide("loading")
      cvp_plot_interactive
    })
    
    output$gamPlot1 <- renderPlot({ req(gam_cvp()); generate_gam_plot1(gam_cvp(), cvp_data(), input$plots_time_range)})
    output$gamPlot2 <- renderPlot({ req(gam_cvp()); generate_gam_plot2(gam_cvp(), cvp_data(), input$plots_time_range)})
    output$gamPlot3 <- renderPlot({ req(gam_cvp()); generate_gam_plot3(gam_cvp())})
    output$gamPlot4 <- renderPlot({ req(gam_cvp()); generate_gam_plot4(gam_cvp())})

    # -------------------------
    # DOWNLOAD HANDLERS
    # -------------------------
    
    # Enable download after GAM model is available
    observe({
      req(gam_cvp())
      shinyjs::enable("download_gam_plots")
    })
    
    output$download_cvp_and_ecg_plots <- downloadHandler(
      filename = function() {
        paste0("cvp_and_ecg_plots_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(cvp_data(), ecg_data(), input$plots_time_range)
        
        temp_dir <- tempdir()
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd), add = TRUE)
      
        plots <- list(
          generate_total_cvp_plot(cvp_data(), input$plots_time_range, range(cvp_data_unfiltered()$time, na.rm = TRUE)),
          generate_ecg_plot(ecg_data(), input$plots_time_range),
          generate_cvp_plot(cvp_data(), input$plots_time_range)
        )
        
        filenames <- c(
          "total_cvp_plot.png",
          "ecg_plot.png",
          "cvp_plot.png"
        )
        
        for (i in seq_along(plots)) {
          ggsave(filename = filenames[i], plot = plots[[i]], width = 10, height = 4, dpi = 300)
        }
        
        zip::zip(zipfile = file, files = filenames)
      }
    )
    
    
    output$download_gam_plots <- downloadHandler(
      filename = function() {
        paste0("central_venous_pressure_gam_plots_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(gam_cvp())
        temp_dir <- tempdir()
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        
        plots <- list(
          generate_gam_plot1(gam_cvp(), cvp_data(), input$plots_time_range),
          generate_gam_plot2(gam_cvp(), cvp_data(), input$plots_time_range),
          generate_gam_plot3(gam_cvp()),
          generate_gam_plot4(gam_cvp())
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
