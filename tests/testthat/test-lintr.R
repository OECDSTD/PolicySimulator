linters_config <- lintr::with_defaults(
  # Purposefully disabled linters:
  object_usage_linter = NULL, # Conflicts with standard usage of dplyr.
  camel_case_linter = NULL, # Conflicts with Shiny functions which are camelCase
  # Linters temporarily disabled - we should enable them and fix errors:
  infix_spaces_linter = NULL,
  single_quotes_linter = NULL,
  spaces_left_parentheses_linter = NULL,
  # Enabled linters with custom arguments:
  open_curly_linter = lintr::open_curly_linter(allow_single_line = TRUE),
  closed_curly_linter = lintr::closed_curly_linter(allow_single_line = TRUE),
  line_length_linter = lintr::line_length_linter(140),
  object_length_linter = lintr::object_length_linter(40),
  object_name_linter = NULL,
  cyclocomp_linter = NULL,
  # Enabled linters with defaults (we leave them here for future reference):
  # absolute_paths_linter = NULL,
  # assignment_linter = NULL,
  # commas_linter = NULL,
  # trailing_blank_lines_linter = NULL,
  trailing_whitespace_linter = NULL
  # spaces_inside_linter = NULL,
  # no_tab_linter = NULL,
)

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free(linters = linters_config)
  })
}
