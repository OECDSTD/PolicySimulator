#' Set of functions to extract levels set of specific types
#' 
#' \code{get_levels_meta} Extracts a list of required levels meta in the package.
#' 
#' @param data Hierarchy sheet dataset.
#' @rdname hierarchy_levels
get_evaluate_level <- function(data) {
  data %>% dplyr::filter(`Aggregation method` == "EVALUATE") %>% dplyr::pull(Code)
}

#' @rdname hierarchy_levels
get_foldable_level <- function(data) {
  data %>% dplyr::filter(`Display as` == "FOLDABLE SECTION") %>% dplyr::pull(Code)
}

#' @rdname hierarchy_levels
get_hierarchy_levels <- function(data) {
  data %>% dplyr::pull(`Foldable Section`)
}

#' @rdname hierarchy_levels
get_hidden_levels <- function(data) {
  data %>% dplyr::filter(`Display as` == "HIDDEN") %>% dplyr::pull(Code)
}

#' @rdname hierarchy_levels
get_weighted_levels <- function(data) {
 # data %>% dplyr::filter(`Aggregation method` == "WEIGHTEDAVERAGE") %>% dplyr::pull(Code)
  data %>% dplyr::filter(`Aggregation method` != "NONE" & `Aggregation method` != "EVALUATE" ) %>% dplyr::pull(Code)
}

#' @rdname hierarchy_levels
get_weightedsum_levels <- function(data) {
  data %>% dplyr::filter(`Aggregation method` == "WEIGHTEDSUM") %>% dplyr::pull(Code)
}

#' @rdname hierarchy_levels
get_weightedaverage_levels <- function(data) {
  data %>% dplyr::filter(`Aggregation method` == "WEIGHTEDAVERAGE") %>% dplyr::pull(Code)
}

#' @rdname hierarchy_levels
get_weightedproduct_levels <- function(data) {
  data %>% dplyr::filter(`Aggregation method` == "WEIGHTEDPRODUCT") %>% dplyr::pull(Code)
}

#' @rdname hierarchy_levels
get_correctedweights_levels <- function(data) {
  data %>% dplyr::filter(`Aggregation method` == "CORRECTEDWEIGHTS") %>% dplyr::pull(Code)
}

#' @param selector_name Name of the selector ("Selector1", "Selector2" or "CompareSelector")
#' @rdname hierarchy_levels
get_selector_level <- function(data, selector_name) {
  data %>% dplyr::filter(!!sym(selector_name) == "YES") %>% dplyr::pull(Code)
}

#' @rdname hierarchy_levels
get_levels_meta <- function(data) {
  levels <- list(
    all = data$Code,
    evaluate = get_evaluate_level(data),
    foldable = get_foldable_level(data),
    hidden = get_hidden_levels(data),
    weighted = get_weighted_levels(data),
    weightedsum = get_weightedsum_levels(data),
    weightedaverage = get_weightedaverage_levels(data),
    weightedproduct = get_weightedproduct_levels(data),
    correctedweights = get_correctedweights_levels(data),
    selector_1 = get_selector_level(data, "Selector1"),
    selector_2 = get_selector_level(data, "Selector2"),
    compare_selector = get_selector_level(data, "CompareSelector")
  )
  levels$subfoldable <- levels$all[(which(levels$foldable == levels$all) + 1):length(levels$all)]
  levels$sup_and_foldable <- setdiff(levels$all, levels$subfoldable)
  levels
}

#' Attach hidden levels indicators to the main application data.
#' 
#' @param data Joined Data and Indicator data, storing the scores.
#' @param subfoldable_levels Levels with the hierarchy lower than foldable.
#' @param levels_meta Levels meta information object.
add_level_indicator <- function(data, subfoldable_levels, foldable_label, levels_meta) {
  indicator_col <- paste0("ind_", subfoldable_levels[1])
  if (length(subfoldable_levels) > 1) {
    data[[indicator_col]] <- NA_real_
    if (subfoldable_levels[1] %in% levels_meta$hidden) {
      return(
        data %>% 
          dplyr::group_by_at(dplyr::vars(dplyr::one_of(subfoldable_levels[1]))) %>% 
          dplyr::group_modify(~ add_level_indicator(., subfoldable_levels[-1], foldable_label, levels_meta))
      )
    }
    new_row <- data[0, ]
    new_row[1, "Question"] <- data[[gsub("C", "L", subfoldable_levels[1])]][1]
    new_row[1, indicator_col] <- 1
    new_row[1, foldable_label] <- data[[foldable_label]][1]
    new_row[, subfoldable_levels[1]] <- NULL
    return(
      data %>% 
        dplyr::group_by_at(dplyr::vars(dplyr::one_of(subfoldable_levels[1]))) %>% 
        dplyr::group_modify(~ dplyr::bind_rows(new_row, add_level_indicator(., subfoldable_levels[-1], foldable_label, levels_meta)))
    )
  } else {
    data[[indicator_col]] <- 1
    data[["Question"]] <- paste(data[["QuestionCode"]], data[[gsub("C", "L", subfoldable_levels[1])]])
    return(data)
  }
}

#' Add hidden levels indicators to each foldable level
#' 
#' @param data Joined Data and Indicator data, storing the scores.
#' @param levels_meta Levels meta information object.
restructure_hierarchy <- function(data, levels_meta) {
  subfoldable_levels <- levels_meta$subfoldable
  sup_and_foldable_levels <- levels_meta$sup_and_foldable
  foldable_label <- gsub("C", "L", levels_meta$foldable)
  
  data <- data %>% 
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(sup_and_foldable_levels))) %>% 
    dplyr::group_modify(~ add_level_indicator(.x, subfoldable_levels, foldable_label, levels_meta))
  if (length(subfoldable_levels) > 1) {
    data <- data %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(!!sym(levels_meta$selector_2)) %>% 
      dplyr::arrange(!!sym(subfoldable_levels[1]), .by_group = TRUE)
  }
  
  return(data)
}

#' Add hidden levels indicators to each foldable level and attach placeholders for future edited scored and values
#' 
#' @param data Joined Data and Indicator data, storing the scores.
#' @param levels_meta Levels meta information object.
make_hierarchy_structure <- function(data, levels_meta) {

  countries_merged_data <- data %>%
    restructure_hierarchy(levels_meta)

  # For PMR Table Structuring
  countries_merged_data$Value <- countries_merged_data$numans
  countries_merged_data$ComparedTo <- ""
  countries_merged_data$EditedScore <- countries_merged_data$Score
  countries_merged_data$EditedValue <- countries_merged_data$Value

  return(countries_merged_data)
}
