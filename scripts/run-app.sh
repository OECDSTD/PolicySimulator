#!/bin/bash

# assuming we are in base directory, e.g. /c/src/R/pmr-oced-datamatics1, execute:
# bash ./scripts/run-app.sh

# APPDIR="stri_apec/Services Trade Restrictiveness Index Simulator"
APPDIR="debug_target/Demo"
# APPDIR="eco_pmr/Product Market Regulation"

cd "./$APPDIR" && Rscript -e 'devtools::load_all(); shiny::enableBookmarking("url"); shiny::shinyAppDir("src", options = list(port = 9000))'
