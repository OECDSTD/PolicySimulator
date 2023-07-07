## devtools::load_all();library(testthat)
context("data_generate")

test_dt <- data.frame(a = 1, b = 2)
test_dt_out <- data.frame(a = "1", b = "2", stringsAsFactors = FALSE)

## ./inst/app/src/global.R
## app_data <- import_simulator_data(path = file.path(root_dir, "data"))
app_data <-
    list(
        ## "Apps" sheet of Matrix_Simulator_Template_APEC_scaleback.xlsx
        Apps = data.frame(
            MetadataName = c("Application name", "Logo", "Homepage", "Copyright", "About",
                             "Select"),
            Name = c("Services Trade Restrictiveness Index Simulator",
                     "https://cor.europa.eu/en/events/PublishingImages/Migrated/OECD_TEXT_20cm_HD_4c.jpg",
                     "STRI Home",
                     "© OECD. All rights reserved | Terms and Conditions",
                     "About the STRI",
                     "Select a sector and country to start the Policy Simulator.")))

## ./inst/app/src/lib/all_backend_tables.R
Aggregation_Table <- data.frame(
    countryname = rep("AUS", 8),
    CH1 = c("1_1", "1_1", "1_1", "1_1", "1_1", "1_2", "1_2", "1_2"),
    CH2 = c("a", "a", "b", "b", "b", "c", "c", "c"),
    WH1 = c(rep(1, 4), rep(0.5, 3), 1),
    WH2 = c(rep(1, 6), rep(0.5, 2)),
    Score = c(1, 2, 1, NaN, 3, 1, 2, NaN)
)

levels_meta1 <- list(
    all = c("countryname", "CH1", "CH2"),
    weighted = c("CH1")
)

levels_meta2 <- list(
    all = c("countryname", "CH1", "CH2"),
    weighted = c("CH1", "CH2")
)

test_that("Validator is empty after initialization", {
  expect_equal(PolicySimulator:::convert_to_character(test_dt), test_dt_out)
})

test_that("get_apps_value can extracts metadata as expected", {
    res <- get_apps_value(app_data, items = c("Homepage", "Aggregation"))
    expected <- c(Homepage = "STRI Home", Aggregation = "DEFAULT")
    expect_equal(res, expected)
})

test_that("level_score applies weights as expected", {

    res1 <- level_score(levels = levels_meta1$all,
                        aggregation_data = Aggregation_Table,
                        levels_meta = levels_meta1,
                        score_column = "Score"
                        )
    ## no aggregation takes place at last level, only applying last level
    ## weights to Score column
    expected1 <- Aggregation_Table$WH2 * Aggregation_Table$Score
    expect_equal(res1$Score, expected1)

    ## for PMR, aggregation takes place at last level, weights are set to zero
    ## if Score is missing and sum of Score is divided groupwise by sum of
    ## corrected weights
    res2 <- level_score(levels = levels_meta2$all,
                       aggregation_data = Aggregation_Table,
                       levels_meta = levels_meta2,
                       correct_weights=TRUE,
                       weighted_average=TRUE,
                       score_column = "Score"
                       )
    expected2 <- c(1.5, 2, 2/1.5) # 2 / 1.5 = 1.33
    testthat::expect_equal(res2$Score, expected2)

})

test_that("aggregate_scores applies level_score as expected", {

    res1 <-
        aggregate_scores(Aggregation_Table, levels_meta1)
    expected1 <- length(unique(Aggregation_Table$CH1))
    expect_equal(nrow(res1$CH1), expected1)

    res2 <-
        aggregate_scores(
            Aggregation_Table, levels_meta2,
            method = "CORRECTEDWEIGHTS" # pick up from Apps sheet:
                                        # template$Apps$Name[template$Apps$MetadataName=="Aggregation"]
        )
    expected2 <- c(1.5, 2, 2/1.5)
    testthat::expect_equal(res2$CH2$Score, expected2)

})
