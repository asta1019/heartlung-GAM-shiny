# =============================================
# app.R
# =============================================
# The app file is the central place where you place combine all your modules and launch the full Shiny app by clicking "Run App" in Rstudio. 

# ---------------------------------------------
# Source packages, functions, modules etc.
# ---------------------------------------------
source("global.R") 

# ---------------------------------------------
# User Interface (UI)
# ---------------------------------------------
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Heart-Lung Interaction App"),
                    
                    # Sidebars 
                    dashboardSidebar(width = 180,
                                     sidebarMenu(
                                       menuItem("Home", tabName = "frontpage", icon = icon("home")),
                                       menuItem("1) Upload Data", tabName = "upload"),
                                       
                                       menuItem("2) Clean Data", tabName = "clean", startExpanded = FALSE,
                                                menuSubItem("Clean PP", tabName = "cleanPP"),
                                                menuSubItem("Manual Clean PP", tabName = "manuealcleanPP"),
                                                menuSubItem("Manual Clean ECG", tabName = "manualcleanECG"),
                                                menuSubItem("Download Data", tabName = "download")
                                       ),
                                       menuItem("3) Visualize Data", tabName = "plot", 
                                                menuSubItem("PP Plot", tabName = "plotPP"),
                                                menuSubItem("CVP Plot", tabName = "plotCVP"),
                                                menuSubItem("ABP Plot", tabName = "plotABP"),
                                                menuSubItem(HTML("CVP Animation of<br><div style='padding-left: 15px;'>Respiratory Cycle</div>"), tabName = "animationCVP1"),
                                                menuSubItem(HTML("ABP Animation of<br><div style='padding-left: 15px;'>Respiratory Cycle</div>"), tabName = "animationABP1"),
                                                menuSubItem(HTML("CVP Animation of<br><div style='padding-left: 15px;'>intervention over time</div>"), tabName = "animationCVP2"),
                                                menuSubItem(HTML("ABP Animation of<br><div style='padding-left: 15px;'>intervention over time</div>"), tabName = "animationABP2")
                                       ),
                                       selected = "frontpage"
                                     )
                    ),
                    
                    # Homepage/frontpage 
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "frontpage",
                                fluidRow(
                                  column(12,
                                         div(style = "text-align: center;",
                                             h1("Welcome to Heart-Lung Interaction Analysis with Generalised Additive Modeling")
                                         ),
                                         p("This interactive analysis platform allows you to make detailed analyses of your own hemodyamic waveform data collected with e.g. the VitalRecorder software in the three steps lines out below"),
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(4,
                                         wellPanel(
                                           h4(icon("upload"), " 1. Upload Data"),
                                           p("In step one, you can upload your physiological data files in RDS or EDF format or you can load waveform data directly from the VitalDB database. It is also possible to load data that you have previously loaded and cleaned.")
                                         )
                                  ),
                                  column(4,
                                         wellPanel(
                                           h4(icon("broom"), " 2. Clean Data"),
                                           p("In step two, you can clean and prepare your data for analysis with interactive tools.")
                                         )
                                  ),
                                  column(4,
                                         wellPanel(
                                           h4(icon("chart-line"), " 3. Visualization"),
                                           p("In step three, it is possible to analyze and visualize cardiopulmonary interactions with advanced plots based on the cleaned data.")
                                         )
                                  )
                                )
                        ),
                        
                        # Tabs in dashboard 
                        tabItem(tabName = "upload",
                                DataUploadUI("upload")
                        ),
                        
                        tabItem(tabName = "cleanPP",
                                CleanPPModuleUI("cleanPP")
                        ),
                       
                        tabItem(tabName = "manuealcleanPP",
                               ManualCleanPPUI("manuealcleanPP")
                        ),
                               
                        tabItem(tabName = "manualcleanECG",
                                CleanECGModuleUI("manualcleanECG") 
                        ),
                        
                        tabItem(tabName = "download",
                                DataDownloadUI("download") 
                        ),
                        
                        tabItem(tabName = "plotPP",
                                ppGAMPlotModuleUI("plotPP")
                        ),
                        
                        tabItem(tabName = "plotCVP",
                                cvpGAMPlotModuleUI("plotCVP")
                        ),
                       
                        tabItem(tabName = "plotABP",
                                abpGAMPlotModuleUI("plotABP")
                        ),
                        
                        tabItem(tabName = "animationCVP1",
                                cvpAnimationUI("animationCVP1")
                        ),
                       
                        tabItem(tabName = "animationABP1",
                                abpAnimationUI("animationABP1")
                        ),
                       
                        tabItem(tabName = "animationCVP2",
                                cvpAnimationTimeUI("animationCVP2")
                        ),
                        
                        tabItem(tabName = "animationABP2",
                                abpAnimationTimeUI("animationABP2")
                        )
                      )
                    )
)

# ---------------------------------------------
# Server-side logic
# ---------------------------------------------
server <- function(input, output, session) {
  data_rv <- reactiveValues(data = NULL)
  
  # Upload 
  DataUploadServer("upload", data_rv)
  
  # Clean 
  clean_pp <- CleanPPModuleServer("cleanPP", data_rv)
  clean_pp_manual <- ManualCleanPPServer("manuealcleanPP", data_in = clean_pp)
  clean_cvp <- CleanECGModuleServer("manualcleanECG", data_in = clean_pp_manual)
  
  # Download
  DataDownloadServer("download", data_in = clean_cvp)
  
  # Plots 
  ppGAMPlotModuleServer("plotPP", clean_pp_manual)
  cvpGAMPlotModuleServer("plotCVP", data_in = clean_cvp)
  abpGAMPlotModuleServer("plotABP", data_in = clean_cvp)
  
  # Animations 
  cvpAnimationServer("animationCVP1", data_in = clean_cvp)
  abpAnimationServer("animationABP1", data_in = clean_cvp)
  cvpAnimationTimeServer("animationCVP2", data_in = clean_cvp)
  abpAnimationTimeServer("animationABP2", data_in = clean_cvp)
}

# ---------------------------------------------
# Run the application
# ---------------------------------------------
shinyApp(ui, server)