#' Select columns that match reactive expression
#' 
#' @param data Data sect from which to extract matching column names.
#' @param reg_exp Regular expression from which to extract columns.
match_cols <- function(data, reg_exp) {
  colnames(data)[grepl(reg_exp, colnames(data))]
}

#' Convert 'numans' string in condition to a corresponding \code{num_{line_id}} one.
#' @param conditions_vector Vector with conditions.
#' @param line_numbers Corresponding line ids.
numans_to_numline <- function(conditions_vector, line_numbers) {
  purrr::map2_chr(line_numbers, conditions_vector, ~ gsub("numans", .x, .y))
}

#' Convert conditions column to a single string prepared for evaluation.
#' 
#' @param conditions_vector Vector with conditions.
conds_to_vec <- function(conditions_vector) {
  conditions_vector <- ifelse(
    is.na(conditions_vector) | conditions_vector == "", "FALSE", conditions_vector
  )
  glue::glue("c({paste(conditions_vector, collapse = ', ')})")
}

#' Evaluate vector of conditions stored in a string.
#' @param conditions_vector Vector with conditions.
#' @param envir Environment of variables values that should be used in evaluation.
eval_conds_vec <- function(conditions_vector, envir) {
  eval(parse(text = conds_to_vec(conditions_vector)), envir = envir)
}

#' Convert line ID numans to an environment.
#' @param numans Vector of line ID numans values.
#' @param line_id Vector of Line IDs.
line_numans_to_env <- function(numans, line_id) {
  numans_env <- new.env()
  list2env(numans %>% setNames(line_id) %>% as.list(), numans_env)
  numans_env
}

#' Generate line ID <-> numans environment for selector data.
#' 
#' \code{get_numals_selector_2} creates environment for a single sector.
#' \code{get_numals_vals} iterates over all the selector_1 values and applies get_numals_selector_2.
#' 
#' @param per_selector_1_data Scores data for a specific sector (selector1 value)
#' @param levels_meta Hierarchy levels meta information object.
#' 
#' @rdname numals_environment
get_numals_selector_2 <- function(per_selector_1_data, levels_meta) {
  grouped_data <- per_selector_1_data %>% 
    dplyr::group_by(!!sym(levels_meta$selector_2))
  keys <- grouped_data %>% dplyr::group_keys()
  grouped_data %>% 
    dplyr::group_map(~ line_numans_to_env(.x$numans, .x$LineID)) %>% 
    setNames(unlist(keys))
}

#' @param per_selector_1_data Scores data for all sectors (selector1 values).
#' @rdname numals_environment
get_numals_vals <- function(country_indicator_data, levels_meta) {
  grouped_data <- country_indicator_data %>% 
    dplyr::group_by(!!sym(levels_meta$selector_1))
  keys <- grouped_data %>% dplyr::group_keys()
  grouped_data %>% 
    dplyr::group_map(~ get_numals_selector_2(.x, levels_meta)) %>% 
    setNames(unlist(keys))
    
}

#' Extract which value from a logical vector is the first TRUE object; NA if all FALSE
#' 
#' @param logical_vector Logical vector to be checked.
first_true <- function(logical_vector) {
  ## expand vector by one element: use Score0 if none of the conditions evaluate to TRUE
  logical_vector <- c(logical_vector, TRUE)
  which_true <- which(logical_vector)[1]
  logical_vector <- logical(length(logical_vector))
  logical_vector[which_true] <- TRUE
  logical_vector
}

#' Extract scores based on evaluated conditions; use Score0 if none of the conditions evaluate to TRUE
#' 
#' @param data Data containing evaluated conditions ("Cond*_evaluated", and related scores).
#' ## @param default Default score when all the evaluated conditions are FALSE.
#' @param score_col Name of the column in which to store extracted scores.
calc_score <- function(data,
                       ## default = 0,
                       score_col = "Score") {
  first_condition_mask <- data %>%
    dplyr::select(starts_with("Cond") & ends_with("evaluated")) %>%
    apply(1, first_true) %>% 
    t()
  scores_df <- data %>%
    dplyr::select(starts_with("Score") & ends_with("evaluated")) %>%
    dplyr::relocate(Score0_evaluated, .after = last_col())
  if ("Score" %in% colnames(scores_df)) {
    scores_df$Score <- NULL
  }
  if (!length(scores_df) == ncol(first_condition_mask)) stop("number of Score and Cond columns unbalanced")
  scores <- t(scores_df)[t(first_condition_mask)]
  data %>%
    ## dplyr::mutate(!!sym(score_col) := ifelse(is.na(scores), default, scores)) %>% 
    dplyr::mutate(!!sym(score_col) := scores) %>% 
    dplyr::select(-dplyr::ends_with("evaluated"))
}

#' Apply computing the scores logic
#' 
#' The function is responsible to perform all the required steps to extract proper condition scores.
#' The steps are:
#' 1. Grouping the data based on selector1 and selector2.
#' 2. Evaluation of conditions based on provided environment.
#' 3. Extracting the score attached to the first met condition.
#' 4. Assigning selected score to a target column.
#' 
#' @param country_indicator_data Joined Data and Indicator sheets data.
#' @param numals_env Environment storing line ID related numans.
#' @param levels_meta Hierarchy levels meta information object.
#' @param score_col Name of the column storing final score.
#' @param update If TRUE, additionally saves the computed scores to 'EditedValue' column.
#' 
compute_score <- function(country_indicator_data, numals_env, levels_meta, score_col = "Score", update = FALSE) {
  country_indicator_data <- country_indicator_data %>%
    dplyr::group_by(!!sym(levels_meta$selector_1), !!sym(levels_meta$selector_2)) %>%
    dplyr::mutate_at(
      dplyr::vars(tidyselect::matches("^Cond[0-9]+$|^Score[0-9]+$")),
      list(evaluated = ~ eval_conds_vec(
        ., numals_env[[dplyr::first(!!sym(levels_meta$selector_1))]][[dplyr::first(!!sym(levels_meta$selector_2))]])
    )) %>%
    ## dplyr::group_modify(~ calc_score(.x, 0, score_col)) %>%
    dplyr::group_modify(~ calc_score(.x, score_col)) %>%
    dplyr::mutate(!!sym(score_col) := eval_conds_vec(
      !!sym(score_col), numals_env[[dplyr::first(!!sym(levels_meta$selector_1))]][[dplyr::first(!!sym(levels_meta$selector_2))]]
    ))
  if (update) {
    country_indicator_data <- country_indicator_data %>% 
      dplyr::mutate(EditedValue = eval_conds_vec(
        LineID, numals_env[[dplyr::first(!!sym(levels_meta$selector_1))]][[dplyr::first(!!sym(levels_meta$selector_2))]]
      ))
  }
  country_indicator_data
}

#' Generate UI Element and attach it's code to a column
#' 
#' \code{attach_ui_element} Generates UI Element for a single data row.
#' \code{attach_ui_element} Vectorizes version of \code{attach_ui_element}.
#' 
#' @param ui_element UI Element name.
#' @param selector_1 ID of chosen selector 1.
#' @param selector_2 ID of chosen selector 2.
#' @param line_id Line ID of corresponding UI Element.
#' @param numans Initial UI Element value (numans).
#' @param foldable_id ID of foldable level in which UI Element should be generated.
#' @param list_data List sheet data content.
#' 
#' @rdname attach_ui
attach_ui_element <- function(ui_element, selector_1, selector_2, line_id, numans, foldable_id, list_data) {
  id <- paste0(selector_1, "_", selector_2, "_", line_id)
  namespace_id <- paste0(foldable_id, "-", id)
  if (is_text(ui_element)) {
    return(
      shiny::div(
        shiny::numericInput(inputId = paste0(foldable_id, "-val_", id), label = NULL, value = numans),
        shiny::actionButton(inputId = namespace_id, "GO", class = "accept-value")
      ) %>% as.character()
    )
  }
  
  ui_element_value <- paste0(ui_element, "V")
  choices_names <- na.omit(list_data[[ui_element]])
  choices_values <- na.omit(list_data[[ui_element_value]])
  if (is_radio(ui_element)) {
    return(
      shiny::radioButtons(
        inputId = namespace_id,
        choiceNames = choices_names,
        choiceValues = choices_values,
        selected = numans,
        inline = TRUE,
        label = NULL
      ) %>% as.character()
    )
  }
  if (is_list(ui_element)) {
    return(
      shiny::selectInput(
        inputId = namespace_id,
        choices = stats::setNames(choices_values, choices_names),
        selected = numans,
        label = NULL
      ) %>% as.character()
    )
  }

  return("")
}

#' @rdname attach_ui
attach_ui_elements <- function(ui_element, selector_1, selector_2, line_id, numans, foldable_id, list_data) {
  purrr::pmap_chr(
    list(ui_element, selector_1, selector_2, line_id, numans, foldable_id), 
    attach_ui_element, list_data = list_data
  )
}

#' Get name of corresponding meta level attached to ID.
#' 
#' @param level Level ID Name.
#' @param prefix Desired level prefix. "L" default.
#' 
#' @examples 
#' get_level_col("CH1", "W")
#' # WH1
#' 
#' get_level_col("CH1")
#' # LH1
#' 
get_level_col <- function(level, prefix = "L") {
  gsub("C", prefix, level)
}
