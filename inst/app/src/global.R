library(shiny)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(echarts4r)
library(DT)
library(shinyjs)
library(dqshiny)
library(yaml)
library(sass)

source(file = "./lib/policy_simulator_wrappers.R")
app_data <- import_simulator_data()

source(file = "./lib/all_backend_tables.R")
source(file = "./lib/app_metadata.R")
source(file = "./lib/download_script.R")
source(file = "./lib/custom_funs.R")
source(file = "./modules/foldable_table.R")
source(file = "./modules/chart.R")

jsResetCode <- "shinyjs.resetcode = function() {history.go(0)}"

options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 1)

sass(
  sass::sass_file("styles/main.scss"),
  ## cache_options = sass_cache_options(FALSE),
  options = sass_options(output_style = "compressed"),
  output = "www/css/sass.min.css"
)
