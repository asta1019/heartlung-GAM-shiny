# =========================================================
# global.R
# =========================================================
# ---------------------------------------------------------
# Load required libraries
# ---------------------------------------------------------
# Core Frameworks and UI Tools
library(shiny)                     # Core framework for building interactive web applications in R
library(shinydashboard)            # Layout and UI tools for dashboard-style Shiny applications
library(shinyjs)                   # Enables JavaScript functionality in Shiny apps (e.g., hide/show elements)

# Data Visualization
library(plotly)                    # Creates interactive versions of ggplot2
library(gganimate)                 # Adds animation capabilities to ggplot2 plots
library(ggtext)                    # Allows HTML/Markdown elements in ggplot2 plots
library(grid)                      # Low-level graphics system used by ggplot2
library(gridExtra)                 # Functions for arranging multiple grid-based plots
library(scales)                    # Provides scale transformations and custom axis formatting

# Statistical Modeling and GAMs
library(mgcv)                      # Fitting generalized additive models (GAMs)
library(gratia)                    # Visualization and diagnostics for GAMs (based on mgcv)
# library(broom)                   # Converts statistical model outputs into tidy data frames

# Specialized Scientific Tools
library(metR)                      # Tools for adding contour-labels
library(rsleep)                    # Tools for detecting R-peaks in ECG
library(edfReader)                 # Read and extract data from European Data Format (EDF) files
library(VitalDBR)                  # Import and analyze vital sign data from the VitalDB repository

# Data Tables and Manipulation
library(DT)                        # Library for interactive tables (used in cleaning modules)
library(tidyverse)                 # Meta-package including ggplot2, dplyr, tidyr, readr, etc.

# Animation and Image Tools
library(magick)                    # Advanced image processing and manipulation (supports GIFs, PNGs, etc.)
library(gifski)                    # High-quality GIF encoder used for gganimate output
library(transformr)                # Helper package for tweening ("in-betweens") transitions in gganimate

# Install package form GitHub
library(devtools)                                      # Needed only for development or GitHub installs
# install_github('legendenomgeorg/VitalDBR/VitalDBR')  # Install VitalDBR package from GitHub
# install_github("boupetch/rsleep")                    # Install rsleep package from GitHub

# Set the maximum file upload size in Shiny to 50 MB (default is 5 MB)
options(shiny.maxRequestSize = 50 * 1024^2)


# ---------------------------------------------------------
# Source external functions (from the 'functions' folder)
# ---------------------------------------------------------
source("functions/functions.R")                                 # Johannes Enevoldsen helper functions 
source("functions/data_prep.R")                                 # Data preprocessing 
source("functions/helper_functions.R")                          # Helper functions used across the app

# ---------------------------------------------------------
# Source Shiny modules (from the 'modules' folder)
# ---------------------------------------------------------
source("modules/DataUploadModule.R")                            # Module for uploading data files

source("modules/PPCleaningModule.R")                            # Module for cleaning PP signals
source("modules/PPManualCleaningModule.R")                      # Module for manual cleaning PP signals 
source("modules/ECGManualCleaningModule.R")                     # Module for manual cleaning ECG signals (e.g., R-peaks)

source("modules/DataDownloadModule.R")                          # Module to allow downloading of processed data

source("modules/PPPlotModule.R")                                # Module for plotting PP GAM models
source("modules/ABPPlotModule.R")                               # Module for plotting ABP GAM models
source("modules/CVPPlotModule.R")                               # Module for plotting CVP GAM models 

source("modules/CVPAnimateModules.R")                           # Module for animated CVP plots and interactions
source("modules/ABPAnimateModules.R")                           # Module for animated ABP plots and interactions
source("modules/CVPAnimateTimeModules.R")                       # Module for animated CVP plots and interactions with time/intervention 
source("modules/ABPAnimateTimeModules.R")                       # Module for animated ABP plots and interactions with time/intervention

