#' Validate Template data
#' 
#' The below set of functions aims to check validity of the template data.
#' 
#' Each template Sheet, is validated by corresponding \code{validate_{sheet_name}} function.
#' 
#' Function \code{validate_answers} joins 'Data' and 'Indicator' sheets and validates the whole, joined dataset.
#' 
#' Function \code{validate_scores} is used during postprocessing validation check. 
#' It checks whether computed scores matches the one originally provided in the template.
#' 
#' @param hierarchy_data Hierarchy sheet data content.
#' @param validator Validator object to store results in. See \link[data.validator]{create_valdiator} for more.
#' @param path Target path where violated data should be saved.
#' 
#' @rdname validation_functions
validate_hierarchy <- function(hierarchy_data, validator, path) {
  res <-
    hierarchy_data %>%
    assertr::chain_start(store_success = TRUE) %>% 
    assertr::verify(description = "All required columns exist",
                    assertr::has_all_names("Code", "Name", "Aggregation method", "Selector1", "Selector2", "CompareSelector", "Display as"),
                    obligatory = TRUE
    ) %>% 
    assertr::assert(description = "Aggregation method store only values 'WeightedSum', 'WeightedAverage', 'WeightedProduct' , 'CorrectedWeights', 'evaluate' or 'none' ",
                    case_in_set(c("weightedsum", "weightedaverage", "weightedproduct", "correctedweights", "evaluate", "none")), `Aggregation method`) %>% 
    assertr::assert(description = "Selector1, Selector2, CompareSelector store only 'YES' or 'NO' values",
                    case_in_set(c("yes", "no")), Selector1, Selector2, CompareSelector) %>% 
    assertr::verify(description = "Selector1 contain only one 'YES' value",
                    "YES" %in% Selector1) %>% 
    assertr::verify(description = "Selector2 contain only one 'YES' value",
                    "YES" %in% Selector2) %>% 
    assertr::verify(description = "CompareSelector contain only one 'YES' value",
                    "YES" %in% CompareSelector) %>% 
    assertr::assert(description = "Code should store only unique values", assertr::is_uniq, Code) %>% 
    assertr::verify(description = "Code should store at least three unique levels (excluding country)",
                    length(setdiff(Code, "countryname")) >= 3) %>% 
    assertr::verify(description = "Template should contain 10 hierarchies as a maximum",
                    length(setdiff(Code, "countryname")) <= 10) %>% 
    assertr::chain_end(error_fun = assertr::error_append) %>%
    data.validator::add_results(validator, name = "Hierarchy") # %>%

    res %>% get_violated_data(validator, name = "Hierarchy", path = path)
}

#' @param list_data List sheet data content.
#' 
#' @rdname validation_functions
validate_list <- function(list_data, validator, path) {
  lists <- gsub("V$", "", colnames(list_data)) %>% unique()

  res <-
    list_data %>% 
    assertr::chain_start(store_success = TRUE) %>% 
    assertr::verify(description = "All UI columns have corresponding value assigned (column with the same name but with 'V' suffix)",
                    all(paste0(lists, "V") %in% colnames(list_data))
    ) %>% 
    assertr::verify(description = "All inputs start with supported format (Radio, list, Text)",
                    all(grepl("^(Radio|list|Text)", colnames(list_data)))
    ) %>% 
    assertr::chain_end(error_fun = assertr::error_append) %>%
  data.validator::add_results(validator, name = "List") # %>%

  res %>% get_violated_data(validator, name = "List", path = path)
}

#' @param country_data Data sheet data content.
#' 
#' @rdname validation_functions
validate_data <- function(country_data, validator, path) {
    res1 <-
        country_data %>%
        dplyr::distinct(countryname, country_id) %>%
        dplyr::left_join(
                   country_data %>%
                   const_in_group(vector = "countryname", groups_cols = c("country_id"), target_col = "is_const_countryname")
               ) %>%
        dplyr::left_join(
                   country_data %>%
                   const_in_group(vector = "country_id", groups_cols = "countryname", target_col = "is_const_country_id")
               ) %>%
        assertr::assert(
                     description = "Countryname should be unique for each country_id and vice versa",
                     is_true, all_of(c("is_const_country_id", "is_const_countryname")),
                     success_fun = assertr::success_append, error_fun = assertr::error_append
                 ) %>%
        data.validator::add_results(validator, name = "Data_countryname_id")

    res1 %>% get_violated_data(validator, name = "Data_countryname_id", path = path)

  res2 <-
    country_data %>% 
    assertr::chain_start(store_success = TRUE) %>% 
    assertr::verify(description = "All required columns exist",
                    assertr::has_all_names("countryname", "country_id", "LineID", "QuestionCode", "answer", "numans"),
                    obligatory = TRUE
    ) %>% 
    assertr::assert_rows(
      description = "LineID should be unique across each country", 
      paste_cols, assertr::is_uniq, all_of(c("countryname", "LineID"))
    ) %>% 
    assertr::chain_end(error_fun = assertr::error_append) %>%
    data.validator::add_results(validator, name = "Data")

    res2 %>% get_violated_data(validator, name = "Data", path = path)
}

col2hex <- function(cname)
{
  colMat <- col2rgb(cname)
  rgb(
    red=colMat[1,]/255,
    green=colMat[2,]/255,
    blue=colMat[3,]/255
  )
}

#' @param indicator_data Indicator sheet data content.
#' @param levels_meta Metadata of hierarchy levels.
#' 
#' @rdname validation_functions
validate_indicator <- function(indicator_data, list_data, validator, levels_meta, path) {
  ui_elements_in_list <- do.call(assertr::in_set, as.list(colnames(list_data)))
  attr(ui_elements_in_list, "call") <- "ui_elements_in_list"
  attr(ui_elements_in_list, "assertr_vectorized") <- TRUE
  indicator_colnames <- c(
    "LineID", levels_meta$all[-1], get_level_col(levels_meta$all[-1], "L"),
    get_level_col(levels_meta$all[-1], "W"), get_level_col(levels_meta$all[-1], "O"),
    "Color", paste0("Cond", 1:15)
  )

  indicator_data %>% 
    assertr::chain_start(store_success = TRUE) %>% 
    assertr::verify(description = sprintf("All required columns exist: %s", paste(indicator_colnames, collapse = ", ")), 
                    do.call(assertr::has_all_names, as.list(indicator_colnames)), obligatory = TRUE) %>% 
    assertr::assert(description = "UI Element column values must exist in the List sheet", ui_elements_in_list, `UI Element`) %>% 
    assertr::assert_rows(
      description = "LineID should be unique across each indicator", 
      paste_cols, assertr::is_uniq, all_of(c(levels_meta$all[2], "LineID"))
    ) %>% 
    assertr::assert_rows(
      description = "Color should be only one of the following values from the R colors found here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf", 
      paste_cols, assertr::in_set(toupper(colors()), colors(), col2hex(colors())), "Color") %>% 
    assertr::assert_rows(
      description = glue::glue("Questions names ({get_level_col(levels_meta$evaluate)}) should be unique per each foldable level"), 
      paste_cols, assertr::is_uniq, all_of(c(levels_meta$sup_and_foldable[-1], get_level_col(levels_meta$evaluate))),
      skip_chain_opts = TRUE,
      error_fun = assertr::warning_append
    ) %>% 
    
    assertr::assert(
      description = "Condition values are valid R expressions",
      correct_expressions, Cond1:Cond15
    ) %>% 
    assertr::assert(
      description = "Used condition variables are numans and num[lineid] only",
      conds_in_vars_set, Cond1:Cond15
    ) %>% 
    assertr::chain_end(error_fun = assertr::error_append) %>%
    data.validator::add_results(validator, name = "Indicator")
  
  for (level in levels_meta$all[-1]) {
    
    weight_col <- get_level_col(level, "W")
    label_col <- get_level_col(level, "L")
    oh_col <- get_level_col(level, "O")
    sub_levels <- levels_meta$all[-1][1:which(levels_meta$all[-1] == level)]
    table_name <- glue::glue("Indicator {level} sublevels grouped")

    res1 <-
      dplyr::left_join(
        indicator_data %>% 
          const_in_group(weight_col, sub_levels, target_col = "is_const_weight"),
        indicator_data %>%
          const_in_group(label_col, sub_levels, target_col = "is_const_label", omit = "-1")
      ) %>% 
      # assertr::assert(
      #   description = "Weight should be equal on each level (0's excluded)", 
      #   const_per_group, is_const_weight,
      #   success_fun = assertr::success_append, error_fun = assertr::error_append
      # ) %>% 
      assertr::assert(
        description = "Label should be equal on each level", 
        const_per_group, is_const_label,
        success_fun = assertr::success_append, error_fun = assertr::error_append
      ) %>% 
      dplyr::ungroup() %>% 
        data.validator::add_results(validator, name = table_name) # %>%

      res1 %>%
        get_violated_data(validator, name = table_name, path = path)
    
    table_name <- glue::glue("Indicator {level} sublevels distinct")

    res2 <-
      indicator_data %>% 
      dplyr::select_at(vars(c(sub_levels, oh_col))) %>% 
      dplyr::distinct() %>% 
      assertr::assert_rows(
        description = "OH should be distinct on each level", 
        paste_cols, assertr::is_uniq, dplyr::all_of(c(sub_levels[-length(sub_levels)], oh_col)),
        success_fun = assertr::success_append, error_fun = assertr::error_append
      ) %>% 
      dplyr::ungroup() %>% 
      data.validator::add_results(validator, name = table_name) # %>%

      res2 %>%
        get_violated_data(validator, name = table_name, path = path)
    
  }
  
  table_name <- glue::glue("Foldable Level Colors distinct")
  
  res3 <- 
    indicator_data %>% 
    dplyr::select(dplyr::all_of(c(levels_meta$foldable, "Color"))) %>%   
    dplyr::distinct() %>% 
    assertr::assert_rows(
      description = "Color values must be different for each foldable section", 
      paste_cols, assertr::is_uniq, "Color",
      success_fun = assertr::success_append, error_fun = assertr::error_append
    ) %>% 
    dplyr::ungroup() %>% 
    data.validator::add_results(validator, name = table_name)
  
  res3 %>%
    get_violated_data(validator, name = table_name, path = path)
  
  
  get_violated_data(indicator_data, validator, name = "Indicator", path = path)
  
}

#' @rdname validation_functions
validate_answers <- function(country_data, indicator_data, list_data, levels_meta, validator, path) {
  ## check if answer in list
  answer_in_list <- construct_answer_in_list(list_data, ui_col = "UI Element")
  num_ui_match <- function(data) {
    ret.vec <- apply(data, 1, answer_in_list) %>% 
      as.numeric()
  }
  attr(num_ui_match, "call") <- "num_ui_match"
  attr(num_ui_match, "assertr_vectorized") <- TRUE
  
  ## check if numans in list
  numans_in_list <- construct_numans_in_list(list_data, ui_col = "UI Element")
  num_ans_match <- function(data) {
    ret.vec <- data %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(check = numans_in_list(numans, answer, `UI Element`)) %>% 
      dplyr::pull(check) %>% 
      as.numeric()
  }
  attr(num_ans_match, "call") <- "num_ans_match"
  attr(num_ans_match, "assertr_vectorized") <- TRUE

  ## add validation for LineID
  res1 <-
    country_data %>%
    assertr::assert(
      description = "LineID from the data sheet must exist in the indicator sheet",
      assertr::in_set(unique(indicator_data$LineID)), LineID,
      success_fun = assertr::success_append,
      error_fun = assertr::error_append) %>%
      data.validator::add_results(validator, name = "Unmatched LineID")
  res1 %>%
      get_violated_data(validator, name = "Unmatched LineID", path = path)

  ## use inner join to remove additional LineIDs
  data_and_indicators_joined <- dplyr::inner_join(country_data, indicator_data, by = "LineID")
  
  data_and_indicators_joined %>% 
    assertr::chain_start(store_success = TRUE) %>% 
    assertr::assert_rows(
      description = "Values of 'answer' column in Data sheet must exist in the corresponding UI Element options in the List sheet",
      num_ui_match, assertr::within_bounds(0.9, 1.1), all_of(c("numans", "answer", "UI Element"))) %>% 
    assertr::assert_rows(
      description = "Values of 'numans' column in Data sheet must exist in the corresponding UI Element scores in the List sheet",
      num_ans_match, assertr::within_bounds(0.9, 1.1), all_of(c("numans", "answer", "UI Element"))
     ,skip_chain_opts = TRUE # chain_end error_fun only applied to first call of assert_rows
    ,error_fun = assertr::warning_append
      ) %>%
    assertr::chain_end(error_fun = assertr::warning_append) %>%
    data.validator::add_results(validator, name = "Data and Indicators Joined")
  
  data_and_indicators_joined %>%
      dplyr::mutate(LineID = paste0("num", trimws(LineID)),
                    index = dplyr::row_number()) %>%
    dplyr::mutate_at(vars(dplyr::starts_with("Cond")), ~numans_to_numline(., LineID)) %>%
    dplyr::group_by_at(vars(setdiff(levels_meta$sup_and_foldable, levels_meta$foldable))) %>%
    dplyr::group_modify(~ num_in_lineid(.x)) %>%
    dplyr::ungroup() %>% 
    dplyr::relocate(`UI Element`, index, .after = LineID) %>% # request SDD 2021-08-30
    assertr::assert(
      description = "All line ids used in conditions should exist as lineID", 
      is_true, dplyr::ends_with("cond_vars_correct"), 
      success_fun = assertr::success_append, error_fun = assertr::error_append
    ) %>% 
    data.validator::add_results(validator, name = "Data and Indicators Joined") %>% 
    get_violated_data(validator, name = "Data and Indicators Joined", path = path)
  
}

#' @param score_data Final application data storing original and computed scores.
#' 
#' @rdname validation_functions
validate_scores <- function(score_data, levels_meta,  validator, path) {

    res <-
        score_data %>% 
        dplyr::ungroup() %>% 
        assertr::assert_rows(
                     description = "Computed scores should be equal to scores provided in Value column", 
                     is_equal, is_true, all_of(c("template_value", "simulator_value")), 
                     success_fun = assertr::success_append, error_fun = assertr::warning_append
                 ) %>% 
        data.validator::add_results(validator, name = "Data and Indicators Joined with scores") # %>%

    res %>% get_violated_data(validator, name = "Data and Indicators Joined with scores", path = path)

}


#' Extract all 'num<line_id>' strings from condition text
#' 
#' @param condition Any string defining scoring condition, like 'num123 < 2'
extract_numlines <- function(condition) {
  stringr::str_extract_all(condition, pattern = "num[:alnum:]*")
}

#' Check whether all values are included in set
#' 
#' @param vals Values which existence is checked in a set.
#' @param set Values set in which we check existence of values.
#' @param na_true If TRUE, then existence of NA in \code{vals} returns TRUE.
vals_in_set <- function(vals, set, na_true = TRUE) {
  if (na_true && any(is.na(vals))) {
    return(TRUE)
  } else {
    all(vals %in% set)
  }
}

#' Check whether all numlines exist in line ids set.
#' 
#' @param num_lines Vector of \code{num_<lineid>} like values.
#' @param line_ids Desired set of possible numlines.
numline_in_lineid <- function(num_lines, line_ids) {
  purrr::map_lgl(num_lines, ~ vals_in_set(., line_ids))
}


#' Check whether all conditions included in 'Code*' columns, fits in possible set of values defined in LineID column
#' 
#' @param data containing conditions (Cond* columns) and line ids (LineID column).
num_in_lineid <- function(data) {
  data %>% 
    dplyr::mutate_at(
      vars(dplyr::starts_with("Cond")), 
      list(cond_vars = extract_numlines)
    ) %>% 
    dplyr::mutate_at(
      vars(dplyr::ends_with("cond_vars")), 
      list(correct = numline_in_lineid),
      line_ids = data$LineID
    ) %>% 
    dplyr::select(-dplyr::ends_with("cond_vars"))
}

#' Get violated data rows
#' 
#' @param data Validated dataset.
#' @param validator Validator object in which validation results of \code{data} are stored.
#' @param name Validated data ID. Should be consistent with the one provided in \link[data.validator]{add_results}.
#' @param save Should the data be saved to csv file?
#' @param path Where the data should be saved?
get_violated_data <- function(data, validator, name = NULL, save = TRUE, path = getwd()) {
  results_data <- data.validator::get_results(validator, unnest = TRUE) %>% 
    dplyr::filter(type != "success")
  if (!nrow(results_data)) {
    return(results_data)
  }
  results_data <- results_data %>%  # max. levels of hierarchy using verb
    dplyr::filter(verb != "verify") # "verify" and are ingored
  name <- ifelse(!is.null(name), name, data.validator:::get_first_name(data))
  results_data <- results_data %>% 
    dplyr::filter(table_name == !!name)
  if (!nrow(results_data)) { # do not continue if dataset empty
    return(results_data)     # (!0 evaluates to TRUE)
  }
  ## violated_data <- cbind(data[results_data$index, ], results_data)
  data2 <-
      if ("index" %in% names(data)) {
          data
      } else {
          data %>% dplyr::mutate(index = dplyr::row_number())
      }
  violated_data <- dplyr::inner_join(data2, results_data, by = "index")
  if (save) {
    data.table::fwrite(violated_data, file = glue::glue("{path}/{name}.csv"))
    message(glue::glue("Violated data saved to {path}/{name}.csv"))
  }
  return(violated_data)
}

#' Paste all columns of the data (with ' ' separator)
#' 
#' @param data Data which columns should be pasted
paste_cols <- function(data) {
  if (ncol(data) <= 1) {
    return(data[[1]])
  }
  do.call(paste, data)
}

#' Compare vector values to TRUE object
#' 
#' @param vec Vector of logical values.
is_true <- function(vec) {
  vec == TRUE
}
attr(is_true, "call") <- "is_true"
attr(is_true, "assertr_vectorized") <- TRUE

#' Validation report template
#' 
#' @details Stored as a text due to lack of read/write permissions.
validation_report_template <- "
---
title: Data validation report
output: html_document
params:
  generate_report_html: !expr function(...) {}
  extra_params: !expr list()
---

#### `r format(Sys.time(), '%Y-%m-%d %H:%M:%S')`

```{r generate_report, echo = FALSE}
params$generate_report_html(params$extra_params)
```

<script>
function attach_file_links() {
  var $results_segments = $('.ui.raised.segment');
  $results_segments.each(function(index, value) {
    var label = $(this).find('div.ribbon.label');
    var violated = $(this).find('.red.label');
    if (violated.length == 1) {
      label.html('<a href = \"' + label.html() + '.csv\"><i class = \"ui icon download\"></i></a>'  + label.html());
    }
  });
}

$(window).load(attach_file_links);
</script>
"

#' Case insensitive version of \link[assertr]{in_set}
#' 
#' @param ... Objects that make up the set
case_in_set <- function (...) {
  the_call <- deparse(sys.call())
  set <- c(...)
  if (!length(set)) 
    stop("can not test for membership in empty set")
  fun <- function(x) {
    if (is.null(x)) 
      stop("nothing to check set membership to")
    raw_result <- toupper(x) %in% toupper(set)
    return(raw_result)
  }
  attr(fun, "assertr_vectorized") <- TRUE
  attr(fun, "call") <- the_call
  return(fun)
}


#' Check type of UI Element
#' 
#' Set of functions responsible for checking UI Element type.
#' Currently supported types:
#' \itemize{
#'   \item{radio buttons}{UI Element which name starts with "Radio"}
#'   \item{dropdown}{UI Element which name starts with "list"}
#'   \item{text input}{UI Element which name starts with "Text"}
#' }
#' 
#' @param ui_element UI element name
#' @rdname ui_elements
is_radio <- function(ui_element) {
  startsWith(ui_element, "Radio")
}

#' @rdname ui_elements
is_list <- function(ui_element) {
  startsWith(ui_element, "list")
}

#' @rdname ui_elements
is_text <- function(ui_element) {
  startsWith(ui_element, "Text")
}

#' Alternative function names for \code{is.numeric}
#' 
#' @details used for correct evaluation of condition rules and ui elements conditions
#' @param x object to be coerced to numeric
#' @rdname check_numeric
isNumeric <- is.numeric

#' @rdname check_numeric  
Isnumeric <- is.numeric

#' Helper predicate constructor for validation of answers offered by UI Element
#' 
#' @param list_data List data sheet content.
#' @param ui_col Name of the column where UI Element names are stored in the target data.
construct_answer_in_list <- function(list_data, ui_col = "UI Element") {
  function(data) {
    answer <- data[["answer"]]
    ui_element <- data[[ui_col]]
    if (is_radio(ui_element) | is_list(ui_element)) {
      return(answer %in% na.omit(list_data[[ui_element]]))
    }
    if (is_text(ui_element)) {
      if (varhandle::check.numeric(answer)) {
        answer <- as.numeric(answer)
        values <- na.omit(list_data[[ui_element]])
        condition <- values[grep("answer", values)]
        return(eval(parse(text = condition)))
      }
      return(answer %in% na.omit(list_data[[ui_element]]))
    }
  }
}

#' Helper predicate constructor for validation of numans offered by UI Element
#' 
#' @param list_data List data sheet content.
#' @param ui_col Name of the column where UI Element names are stored in the target data.
#' @examples
#' numans_in_list <- construct_numans_in_list(list_data, ui_col = "UI Element")
#' data_and_indicators_joined %>%
#'     dplyr::filter(LineID %in% c("1040", "1042"), country_id %in% c("ALB", "CAN")) %>%
#'     dplyr::select(country_id, LineID, QuestionCode, answer, numans, Value, Source, `UI Element`) %>%
#'     dplyr::rowwise() %>%
#'     dplyr::mutate(check = numans_in_list(numans = numans, answer = answer, ui_element = `UI Element`)) %>%
#'     dplyr::pull(check) %>%
#'     as.numeric()
construct_numans_in_list <- function(list_data, ui_col = "UI Element") {
    function(numans, answer, ui_element) {
    ## ui_element <- "Text8"
    ## answer <- "not applicable"
    ## answer <- 9
    ui_element_value <- paste0(ui_element, "V")
    ui_list_data <- na.omit(list_data[, c(ui_element, ui_element_value)])
    if (is_radio(ui_element) | is_list(ui_element)) {
      binded_list <- dplyr::bind_rows(ui_list_data, data.frame(answer, numans) %>% stats::setNames(colnames(ui_list_data)))
      return(nrow(ui_list_data) == nrow(dplyr::distinct(binded_list)))
    }
    if (is_text(ui_element)) {
      if (varhandle::check.numeric(answer)) {
        answer <- as.numeric(answer)
        labels <- list_data[[ui_element]]
        value_condition <- ui_list_data[grep("answer", labels), 2]
        return(eval(parse(text = value_condition)) == answer)
      }
      if (answer %in% c("not available",
                        "not applicable",
                        "sector does not exist")) {
        return(numans == -1)
      }
      return(FALSE)
    }
  }
}

#' Check whether vector stores constant values
#' 
#' @param vector Checked vector.
#' @param omit Exception values to be skipped in check (like 0, "-1")
all_equal <- function(vector, omit) {
  vector <- vector[vector != omit]
  all(vector == vector[1])
}

#' Check whether target column is constant within each group
#' 
#' @param data Data in which check should be done.
#' @param vector Column name that should be checked for constant values.
#' @param group_cols Column defining groups.
#' @param target_col Name of the target column in which results should be stored.
#' @param omit What values should be ignored in constants check.
const_in_group <- function(data, vector, groups_cols, target_col, omit = 0) {
  includes_missing <- dplyr::pull(data, !!sym(vector)) %>% is.na() %>% any()
  if(includes_missing) stop("Missing values in ", vector, ". Please make sure that all WH and LH columns are filled.")
  dplyr::group_by_at(data, vars(groups_cols)) %>% 
    dplyr::summarise(!!target_col := all_equal(!!sym(vector), omit), .groups = "drop")
}

#' Helper function that returns provided value
#' 
#' @details Used for making results of constants check more readable
#' @param value Value to be returned.
const_per_group <- function(value) {
  value
}
attr(const_per_group, "call") <- "const_per_group"
attr(const_per_group, "assertr_vectorized") <- TRUE

#' Check whether expression included in string is a proper R expression
#' 
#' @param text_expr Text expression to be checks.
correct_expr <- function(text_expr) tryCatch(is.expression(parse(text = text_expr)), error = function(e) FALSE)

#' Check validity of expressions set
#' 
#' Vectorized version of \link{correct_expr}
correct_expressions <- function(expressions) {
  eval(parse(text = fun_vectorize(expressions, "correct_expr")))
}
attr(correct_expressions, "call") <- "correct_expressions"
attr(correct_expressions, "assertr_vectorized") <- TRUE

#' Extract all variables used in expression
#' 
#' @param condition Text vector of R expressions from which variables should be extracted.
get_cond_vars <- function(condition) {
  all.vars(as.formula(sprintf("~ %s + 1", gsub("\\||\\&|\\=|<|>|\\(|\\)|!|/|\\*", " + ", condition))))
}

#' Check whether only defined set of variables is included in conditions set
#' 
#' By default only \code{numans} and \code{num_<lineID>} are allowed
#' 
#' @param conds_vec Vector of conditions.
#' @param set_regexp Regular expression defining variables set.
conds_in_vars_set <- function(conds_vec, set_regexp = "numans|num[:alnum:]*") {
  conds_vec <- ifelse(is.na(conds_vec), "numans", conds_vec)
  eval(parse(text = fun_vectorize(conds_vec, "get_cond_vars", holder = "list"))) %>% 
    purrr::map_lgl(~ all(grepl("numans|num[:alnum:]*", .)))
}
attr(conds_in_vars_set, "call") <- "conds_in_vars_set"
attr(conds_in_vars_set, "assertr_vectorized") <- TRUE

#' Generate vectorized function evaluation code
#' 
#' @param arg Vector of arguments to be passed of the function
#' @param fun Name of the function to be evaluated.
#' @param holder What type of output object should be returned (e.g. 'c' or 'list').
fun_vectorize <- function(arg, func, holder = "c") {
  paste0(holder, "(", paste(sprintf("%s(\"%s\")", func, arg), collapse = ", "), ")")
}

#' Are the two columns of the data equal
#' 
#' @param data Data consisting of two columns.
is_equal <- function(data) {
    dplyr::near(data[[1]], data[[2]])
}

#' Define follow up steps on violated data
#' 
#' @details 
#' The function checks for existence of validation violations and performs 
#' the following (optional based on specified options) steps:
#' \itemize{
#'   \item{}{Saving the validation report when errors or warnings exist.}
#'   \item{}{Opens the report in the browser.}
#'   \item{}{Stops execution or returns warning about violations.}
#' }
#' @param validator Validator object.
#' @param target_app_path Target path when report should be saved.
#' @param template_file Validation template path.
#' @param callback Function that should be applied when violation exists. By default \code{stop}.
#' @param report_file Report file name.
validation_error_callback <- function(validator, target_app_path, template_file, callback = stop, report_file = "validation_report.html") {
  errors_exist <- "error" %in% data.validator::get_results(validator)$type
  warnings_exist <- "warning" %in% data.validator::get_results(validator)$type
  
  if (errors_exist || warnings_exist) {
    write(validation_report_template, template_file)
    data.validator::save_report(validator, output_dir = target_app_path, template = template_file, output_file = report_file)
    if (interactive() & rstudioapi::isAvailable()) { # rstudioapi::verifyAvailable()
      rstudioapi::viewer(file.path(target_app_path, report_file))
    }
    if (getOption("pass.violated", FALSE) || !errors_exist) {
      callback <- warning
    }
    callback(glue::glue("Validation failed. Please check validation report to see the details."))
  }
}
