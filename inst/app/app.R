if (!interactive()) {
  # for shinyapps deployed app force installing the package from source
  if (!file.exists("R-lib")) {
    dir.create("R-lib")
  }
  
  .libPaths(c(normalizePath("R-lib/"), .libPaths()))
  
  if (!do.call("require", list("PolicySimulator"))) {
    remotes::install_local("PolicySimulator_0.2.1.tar.gz", repos = NULL, type = "source", dependencies = FALSE)
  }  
}

shiny::enableBookmarking("url")
shiny::shinyAppDir("src")
