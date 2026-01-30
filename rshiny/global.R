# global.R
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyLP)
library(shinyjs)

library(future)
library(promises)
library(later)
plan(sequential)

library(tidyverse)
library(readxl)
library(readr)
library(DT)

library(patchwork)
library(magick)
library(grid)
library(colourpicker)
library(av)

library(DBI)
library(config)
library(uuid)

library(stringr)
library(fs)
library(jsonlite)
library(zip)

options(shiny.maxRequestSize = 10000*1024^2)

# Reticulate
library(reticulate)
# use_condaenv("speciesnet", required = TRUE)
# use_virtualenv("/home/jovyan/.venvs/speciesnet", required = TRUE)
use_python(Sys.which("python"), required = TRUE)
reticulate::py_config()
