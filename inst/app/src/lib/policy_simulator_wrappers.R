# this is required to enable shinyapps deployment
library(data.validator)
library(readxl)
library(rio)
library(R.utils)
library(varhandle)

#' ::: hack solution for hiding package reference (from packrat)
#'
#' @param pkg package name
#' @param name function name
#'
#' @return function
`%:::%` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}

get_level_col <- 'PolicySimulator' %:::% 'get_level_col'
get_numals_vals <- 'PolicySimulator' %:::% 'get_numals_vals'
aggregate_scores <- 'PolicySimulator' %:::% 'aggregate_scores'
compute_score <- 'PolicySimulator' %:::% 'compute_score'
attach_ui_elements <- 'PolicySimulator' %:::% 'attach_ui_elements'
is_text <- 'PolicySimulator' %:::% 'is_text'
isNumeric <- 'PolicySimulator' %:::% 'isNumeric'
Isnumeric <- 'PolicySimulator' %:::% 'Isnumeric'
get_levels_meta <- 'PolicySimulator' %:::% 'get_levels_meta'
import_simulator_data <- 'PolicySimulator' %:::% 'import_simulator_data'
get_apps_value <- 'PolicySimulator' %:::% 'get_apps_value'
