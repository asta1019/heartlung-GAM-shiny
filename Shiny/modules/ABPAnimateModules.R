# =============================================
# Modules: ABPAnimateInteractionTermModules.R
# =============================================
# ---------------------------------------------
# UI Function for ABP Animation Module
# ---------------------------------------------
abpAnimationUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2("Animation of Interventions of Respiratory Cycle for ABP"),
    
    tags$p(
      "Animation of the respiratory cycle's influence on the ABP waveform before the intervention.", tags$br(), tags$br(),
      "The left plot shows how the average heartbeat is affected by the respiratory phase.",
      "The right plot shows how the respiratory phase affect the average ABP level.",
      "It is therefore effectively also illustrating the respiratory phase it self, because ABP increses with increased airway pressure."
    ),
    
    tags$p(
      tags$b("Please note:"), "The GAM animation may take a few minutes to generate. Please be patient. Once started, the process cannot be interrupted or stopped."
    ),
    
    # Mathematical formulation of the GAM model
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
    
    
    # Show/hide the GAM model R code
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
                "gam_model <- mgcv::bam(ABP ~ s(P_wave_rel_index, bs = 'cr', k = 40) +
                s(insp_rel_index, bs = 'cc', k = 30) +
                ti(P_wave_rel_index, insp_rel_index, bs = c('cr', 'cc'), k = c(40, 30)) +
                s(time, k = 20),
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
    
    # UI for animation rendering and download
    fluidRow(
      column(12,
             h4("Combined Animation: ABP across Cardiac and Respiratory Phases"),
             div(style = "display: flex; gap: 5px;",
                 actionButton(ns("generate_gam_animation"), "Generate GAM Animation"),
                 downloadButton(ns("download_gam_mp4"), "Download MP4"),
                 downloadButton(ns("download_gam_chrome"), "Open in Chrome")
             ),
             br(),
             imageOutput(ns("gam_animation"), height = "400px")
      )
    )
  )
}

# ---------------------------------------------
# Server Function for animation 1 for ABP
# ---------------------------------------------
abpAnimationServer <- function(id, data_in) {
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
    set_discrete <- FALSE   # Use discrete BAM for speed
    set_nthreads <- 6       # Number of CPU threads
    
    # Reactive expression to generate adjusted ABP data
    pp_data <- reactive({
      req(data_in())
      
      # Adjust ABP based on QRS and inspiration timings
      adjusted_abp <- adjust_abp_insp_index(
        abp = data_in()$data$sample_pp$abp,
        cvp = data_in()$data$sample_cvp$cvp,
        qrs_times = data_in()$data$sample_cvp$qrs$time,
        insp_start_times = data_in()$data$sample_cvp$insp_start$time
      )
      
      adjusted_abp
    })
    
    # -------------------------------
    # GAM
    # -------------------------------
    
    # Animation generation triggered by button
    gam_anim <- eventReactive(input$generate_gam_animation, {
      req(pp_data())
      total_frames <- 200
      
      withProgress(message = "Generating animation...", value = 0, {
        incProgress(0.1, detail = "Fitting GAM model...")
        
        # Fit GAM model
        gam_model <- bam(
          ABP ~ s(P_wave_rel_index, bs = 'cr', k = 40) +
            s(insp_rel_index, bs = 'cc', k = 30) +
            ti(P_wave_rel_index, insp_rel_index, bs = c('cr', 'cc'), k = c(40, 30)) +
            s(time, k = 20),
          data = pp_data(),
          rho = 0.95,
          discrete = set_discrete,
          nthreads = set_nthreads
        )
        
      # -------------------------------
      # ANIMATIONS
      # -------------------------------
        
        # Create prediction grid
        incProgress(0.2, detail = "Preparing prediction grid...")
        structured_data <- expand_grid(
          insp_rel_index = seq(0, 1, length.out = 500),
          P_wave_rel_index = seq(0, 1, length.out = 100),
          time = 0
        )
        structured_data$ABP <- predict(gam_model, newdata = structured_data)
        
        # Animation A: How the CVP waveform varies with the cardiac and respiratory cycles.
        incProgress(0.2, detail = "Generating animation A...")
        p_A <- ggplot(structured_data, aes(P_wave_rel_index, ABP)) +
          geom_line(alpha = 0.2) +
          labs(
            x = "Position in cardiac cycle (relative to P wave)",
            y = "ABP [mmHg]")+
          ggtitle(
            label = paste0("Relative position in the respiratory cycle: {sprintf('%.2f', as.numeric(closest_state))}"), 
            subtitle = "The ABP waveform for a heart beat and how it depends on/changes with the\nrespiratory cycle") +
          theme_grey(base_size = 12) +
          theme(
            plot.title = element_text(size = 14, hjust = 0, lineheight = 1.2),
            plot.subtitle = element_text(size = 11, hjust = 0, lineheight = 1, margin = margin(t = 9, b = 6)),
            plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm")) +
          transition_states(insp_rel_index, transition_length = 0.0005)
        
        
        anim_A <- animate(p_A, nframes = total_frames, width = 600, height = 400, res = 100, renderer = magick_renderer())
        
        # Animation B: Effect of respiration on average ABP level
        incProgress(0.2, detail = "Processing animation B...")
        time_smooth <- gratia::smooth_estimates(gam_model, 's(insp_rel_index)')
        states_B <- data.frame(state = seq(0, 1, length.out = total_frames))
        
        p_B <- ggplot(time_smooth, aes(insp_rel_index, .estimate)) +
          geom_line() +
          geom_vline(aes(xintercept = state), data = states_B) +
          labs(
            x = "Position in respiratory cycle (relative to inspiration start)",
            y = "Partial effect of respiration in ABP",
            subtitle = "The impact of the respiratory cycle on the average ABP level. This figure\n illustrates the phases of the respiratory cycle"
          ) +
          theme_grey(base_size = 12) +
          theme(
            plot.title = element_blank(),
            plot.subtitle = element_text(size = 11, hjust = 0, lineheight = 1),
            plot.margin = unit(c(1.6, 0.2, 0.2, 0.2), "cm")
          ) +
          transition_states(state, transition_length = 0.0005)
        
        
        anim_B <- animate(p_B, nframes = total_frames, width = 600, height = 400, res = 100, renderer = magick_renderer())
        
        # Combine the two animations frame-by-frame
        combined <- image_append(c(anim_A[1], anim_B[1]))
        for (i in 2:total_frames) {
          combined <- c(combined, image_append(c(anim_A[i], anim_B[i])))
          incProgress(0.3 / total_frames)
        }
        shinyjs::enable("download_gam_mp4")
        shinyjs::enable("download_gam_chrome")
        combined
      })
    })
    
    # Render the animated image
    output$gam_animation <- renderImage({
      req(gam_anim())
      tmpfile <- tempfile(fileext = ".gif")
      image_write(gam_anim(), path = tmpfile, format = "gif")
      list(src = tmpfile, contentType = "image/gif")
    }, deleteFile = TRUE)
    
    
    # -------------------------------
    # DOWNLOAD HANDLERS
    # -------------------------------
    
    # Provide download as MP4
    output$download_gam_mp4 <- downloadHandler(
      filename = function() {
        paste0("ABP_gam_animation_", Sys.Date(), ".mp4")
      },
      content = function(file) {
        tmpgif <- tempfile(fileext = ".gif")
        image_write(gam_anim(), path = tmpgif, format = "gif")
        system2("ffmpeg", args = c(
          "-y", "-i", tmpgif,
          "-movflags", "+faststart",
          "-pix_fmt", "yuv420p",
          "-vf", "scale='trunc(iw/2)*2:trunc(ih/2)*2'",
          file
        ))
      }
    )
    
    # Download and open in Chrome
    output$download_gam_chrome <- downloadHandler(
      filename = function() {
        paste0("ABP_gam_animation_", Sys.Date(), ".mp4")
      },
      content = function(file) {
        tmpgif <- tempfile(fileext = ".gif")
        image_write(gam_anim(), path = tmpgif, format = "gif")
        # Convert GIF to MP4
        system2("ffmpeg", args = c(
          "-y", "-i", tmpgif,
          "-movflags", "+faststart",
          "-pix_fmt", "yuv420p",
          "-vf", "scale='trunc(iw/2)*2:trunc(ih/2)*2'",
          file
        ))
        
        # Move file to Downloads folder
        destination <- file.path(Sys.getenv("HOME"), "Downloads", basename(file))
        file.copy(from = file, to = destination, overwrite = TRUE)
        
        # Open file in Google Chrome
        system(paste0("open -a 'Google Chrome' ", shQuote(destination)))
      }
    )
  })
}
