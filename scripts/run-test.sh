#!/bin/bash

REPORTER="progress"
LOADALL="devtools::load_all();"
DEPENDENCIES="library(testthat);"

TESTFILE="test-data_generate.R"

Rscript -e "$LOADALL $DEPENDENCIES test_file(\"tests/testthat/$TESTFILE\", reporter = \"$REPORTER\")"
