context("validation")

validator <- data.validator::create_validator()
df <- data.frame(a = 1:2, b = c("a", "b"))
df %>% 
  assertr::assert(assertr::in_set(c(0, 1)), a, error_fun = assertr::error_append) %>% 
  data.validator::add_results(validator, name = "tempdata")


test_that("get_violated_data properly subsets violated data and attaches assertion meta", {
  path <- tempdir()
  violated_data <- get_violated_data(df, validator, save = FALSE, name = "tempdata")
  expect_equal(violated_data$a, df$a[2])
  expect_equal(violated_data$type, "error")
})
