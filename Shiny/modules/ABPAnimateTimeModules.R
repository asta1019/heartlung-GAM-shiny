# ======================================
# Modules: ABPAnimateModules.R
# ======================================
# --------------------------------------
# UI Function for ABP Animation Module
# --------------------------------------
abpAnimationTimeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    
    h2("Animation of Interventions over Time for ABP"),
    
    tags$p(
      "Here it is possible to visualise how an intervention affects the general heart beat in the ABP waveform." , tags$br(), tags$br(),
      "The left figure is a .gif animation of how the heart beat changes with the intervention,",
      "while the right figure shows the timing of the intervention as well as how the waveform overall increases or decreases in the context of the intervention."
    ),
    
    tags$p(
      tags$b("Please note:")
    ),
    tags$ul(
      tags$li("The GAM animation may take a long time to generate, so please be patient. Once started, the process cannot be interrupted or stopped."),
      tags$li("If no plots appear after the animation finishes, it may be because the intervention start and end times have not been set. Please specify these times in the 'Upload Data' tab.")
    ),
    
    # Mathematical formula used in the GAM model
    fluidRow(
      column(
        12,
        tags$h4("GAM model", style = "margin-bottom: 10px;"),
        div(
          style = "background-color: white; padding: 5px 10px 10px 10px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-bottom: 15px;",
          tags$h4(""),
          withMathJax(
            tagList(
              div(style = "text-align: center; font-size: 14px;",
                  '\\( ABP \\sim s(P\\_wave\\_index) + s(insp\\_rel\\_index) + ti(P\\_wave\\_index, insp\\_rel\\_index, time) + s(time) + \\varepsilon \\)'
              ),
              div(style = "font-size: 11px; margin-top: 10px;",
                  '\\( \\boldsymbol{P\\_wave\\_index} \\): The position in the cardiac cycle', tags$br(),
                  '\\( \\boldsymbol{insp\\_rel\\_index} \\): The position in the respiratory cycle'
              )
            )
          )
        )
      )
    ),
    
    
    # Show/hide underlying GAM model code
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
                "gam_abp_total <- mgcv::bam(ABP ~ te(P_wave_index, insp_rel_index, time, fx = TRUE, 
                bs = c('cc', 'cc', 'cr'), k = c(10, 10, 5)),
                data = pp_data(),
                rho = 0.95,
                discrete = FALSE,
                nthreads = 6)"
              ))
            )
          )
        )
      )
    ),
    
    br(),
    
    # Control buttons
    fluidRow(
      column(12,
             h4("Combined Animation: ABP across Cardiac and Respiratory Phases over Time"),
             div(
               style = "display: flex; gap: 5px; justify-content: flex-start;",
               actionButton(ns("generate_animation"), "Generate Animation"),
               downloadButton(ns("download_mp4"), "Download MP4"),
               downloadButton(ns("download_chrome"), "Open in Chrome")
               
             ),
             br(),
             div(style = "margin-top: 20px;",
                 imageOutput(ns("abp_animation"), width = "100%", height = "400px"))
      )
    )
  )
}

# ------------------------------------------
# Server Function for ABP Animation Module
# ------------------------------------------
abpAnimationTimeServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # GAM model – R code (expand/collapse)
    observeEvent(input$toggle_code, {
      shinyjs::toggle("code_box")
    })
    
    # Download buttons
    shinyjs::disable("download_gam_mp4")
    shinyjs::disable("download_gam_chrome")
    
    # Configuration flags for GAM fitting
    set_discrete <- FALSE    # Use discrete BAM for speed
    set_nthreads <- 6       # Number of CPU threads
    
    # Reactive expression to generate adjusted ABP data
    pp_data <- reactive({
      req(data_in())
      
      # Adjust ABP based on QRS and inspiration timings
      adjust_abp_insp_index(
        abp = data_in()$data$sample_pp$abp,
        cvp = data_in()$data$sample_cvp$cvp,
        qrs_times = data_in()$data$sample_cvp$qrs$time,
        insp_start_times = data_in()$data$sample_cvp$insp_start$time
      )
    })
    
    # -------------------------------
    # GAM
    # -------------------------------
    
    # Reactive animation
    abp_plot <- eventReactive(input$generate_animation, {
      req(pp_data())
      total_frames <- 200
      
      withProgress(
        message = 'Generating animation',
        detail = 'Starting...', value = 0, {
          
          # Fit GAM model for ABP
          incProgress(0.05, detail = "Fitting model...")
          gam_abp_total <- mgcv::bam(
            ABP ~ te(P_wave_index, insp_rel_index, time, fx = TRUE, 
                     bs = c('cc', 'cc', 'cr'), k = c(10, 10, 5)),
            data = pp_data(),
            rho = 0.95,
            discrete = set_discrete,
            nthreads = set_nthreads
          )
          
        # -------------------------------
        # ANIMATIONS
        # -------------------------------
          
          # Prepare prediction grid for animation
          incProgress(0.10, detail = "Preparing prediction grid...")
          len <- max(gam_abp_total$model$time)
          structured_data_total <- tidyr::expand_grid(
            insp_rel_index = seq(0, 1, length.out = 50),
            P_wave_index = seq(0, 1, length.out = 100),
            time = seq(0, len, length.out = total_frames)
          )
          
          # Predict ABP values using the fitted model
          structured_data_total$ABP <- predict(gam_abp_total, newdata = structured_data_total)
          
          # Extract intervention administration timing
          incProgress(0.10, detail = "Preparing intervention data...")
          fluid_data <- data.frame(
            start = data_in()$data$sample_cvp$fluid_start,
            end = data_in()$data$sample_cvp$fluid_end
          )
          
          # Get time frames overlapping with intervention
          fluid_rect_data <- structured_data_total %>%
            filter(time >= fluid_data$start & time <= fluid_data$end) %>%
            select(time) %>%
            distinct()
          
          # Prepare time effect prediction data
          n_times <- 100
          times <- seq(min(gam_abp_total$model$time),
                       max(gam_abp_total$model$time),
                       length.out = n_times)
          
          # Create new data where p_index and respirations_index are 0
          new_data <- data.frame(
            time = times,
            P_wave_index = 0,
            insp_rel_index = 0
          )
          
          # Plot A
          incProgress(0.10, detail = "Laver plot A...")
          p_A <- ggplot() +
            geom_line(data = structured_data_total,
                      aes(P_wave_index, ABP, 
                          color = insp_rel_index, group = interaction(insp_rel_index, time)),
                      alpha = 0.2) +
            geom_rect(data = fluid_rect_data,
                      aes(xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf),
                      fill = "blue", alpha = 0.1, inherit.aes = FALSE) +
            scale_color_viridis_c(option = "turbo") +labs(
              title = paste0(
                "Time: {sprintf('%.0f', frame_time)}<br>",
                "<span style='font-size:11pt;'>",
                "The ABP waveform for an individual heart beat and its<br>", 
                "dependence on the respiratory cycle. The animation<br>", 
                "shows how these change with interventions over time.",
                "</span>"
              ),
              x = "Position in cardiac cycle (Relative to P wave)", 
              y = "ABP [mmHg]"
            )+
            transition_time(time) +
            ease_aes('linear') +
            theme_minimal() +
            theme(
              plot.title = ggtext::element_markdown(size = 14, hjust = 0, lineheight = 1),
              plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm")
            )
          
          
          # Plot B
          incProgress(0.10, detail = "Laver plot B...")
          time_effect <- gratia::fitted_values(gam_abp_total, data = new_data)
          states_B <- data.frame(state = seq(0, len, length.out = total_frames))
          
          p_B <- ggplot() +
            geom_line(data = time_effect, aes(x = time, y = .fitted)) +
            geom_ribbon(data = time_effect,
                        aes(x = time, ymin = .fitted - 1.96 * .se, ymax = .fitted + 1.96 * .se),
                        alpha = 0.5) +
            geom_rect(data = fluid_data,
                      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                      fill = "blue", alpha = 0.2) +
            geom_vline(data = states_B, aes(xintercept = state), color = "red") +
            labs(
              x = "Time [s]",
              y = "Partial effect of time in ABP",
              subtitle = "The overall change in ABP level and an indication (blue area) of when an \nintervention has occurred."
            ) +
            transition_states(state, transition_length = 0, state_length = 1, wrap = FALSE) +
            theme(plot.margin = unit(c(2.0, 0.2, 0.2, 0.2), "cm"))
          
          # Render both animations (A and B) 
          incProgress(0.10, detail = "Renderer animationer...")
          a <- animate(p_A, nframes = total_frames, width = 600, height = 400, res = 100, renderer = magick_renderer())
          b <- animate(p_B, nframes = total_frames, width = 600, height = 400, res = 100, renderer = magick_renderer())
          
          # Combine animations side by side
          combined <- image_append(c(a[1], b[1]))
          for (i in 2:total_frames) {
            combined <- c(combined, image_append(c(a[i], b[i])))
            incProgress(0.35 / total_frames, detail = paste0("Kombinerer frame ", i, " af ", total_frames))
          }
          
          # Finalize and enable download options
          incProgress(0.10, detail = "Færdiggør animation...")
          shinyjs::enable("download_mp4")
          shinyjs::enable("download_chrome")
          
          combined
        }
      )
    })
    
    # Render ABP animation
    output$abp_animation <- renderImage({
      req(abp_plot())
      tmpfile <- tempfile(fileext = ".gif")
      image_write(abp_plot(), path = tmpfile, format = "gif")
      list(src = tmpfile, contentType = "image/gif")
    }, deleteFile = TRUE)
    
    # Download animation as MP4
    output$download_mp4 <- downloadHandler(
      filename = function() {
        paste0("abp_animation_", Sys.Date(), ".mp4")
      },
      content = function(file) {
        tmpgif <- tempfile(fileext = ".gif")
        image_write(abp_plot(), path = tmpgif, format = "gif")
        system2(
          command = "ffmpeg",
          args = c(
            "-y",
            "-i", tmpgif,
            "-movflags", "+faststart",
            "-pix_fmt", "yuv420p",
            "-vf", "scale='trunc(iw/2)*2:trunc(ih/2)*2'",
            file
          )
        )
      }
    )
    
    # -------------------------------
    # DOWNLOAD HANDLERS
    # -------------------------------
    
    # Download and open in Chrome
    output$download_chrome <- downloadHandler(
      filename = function() {
        paste0("ABP_gam_animation_time_", Sys.Date(), ".mp4")
      },
      content = function(file) {
        tmpgif <- tempfile(fileext = ".gif")
        image_write(abp_plot(), path = tmpgif, format = "gif")
        # Convert GIF to MP4
        system2(
          command = "ffmpeg",
          args = c(
            "-y",
            "-i", tmpgif,
            "-movflags", "+faststart",
            "-pix_fmt", "yuv420p",
            "-vf", "scale='trunc(iw/2)*2:trunc(ih/2)*2'",
            file
          )
        )
        # Move file to Downloads folder
        destination <- file.path(Sys.getenv("HOME"), "Downloads", basename(file))
        file.copy(from = file, to = destination, overwrite = TRUE)
        
        # Open in Chrome
        system(paste0("open -a 'Google Chrome' ", shQuote(destination)))
      }
    )
  })
}

