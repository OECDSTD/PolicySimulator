#' Create Policy Simulator App structure
#' 
#' The function created the target application code in desired directory.
#' 
#' @param template List of tables included in the original template Sheets.
#' @param target-app_path Target application path.
#' @param data Preprocessed data based on \code{template} that should be directly used in the app.
create_app_structure <- function(template, target_app_path, data) {
  app_name <- template$Apps$Name[1]
  app_path <- file.path(target_app_path, app_name)
  if (dir.exists(app_path)) {
    unlink(app_path, recursive = TRUE)
  }

  dir.create(app_path)
  if (!dir.exists(file.path(app_path, "src"))) {
    dir.create(file.path(app_path, "src"))
    dir.create(file.path(app_path, "src/data"))
  }
  
  package_path <- system.file("", package = "PolicySimulator")
  R.utils::copyDirectory(glue::glue("{package_path}/app"), app_path)
  
  export_template_data(template, app_path)
  rio::export(data, glue::glue("{app_path}/src/data/Master_Table.rds"))

  message("Please run app.R file to open policy simulator.")
  if (interactive() & rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(file.path(app_path, "app.R"))
  }
  setwd(app_path)
}
