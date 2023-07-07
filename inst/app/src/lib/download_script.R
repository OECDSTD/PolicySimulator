sup_and_foldable_levels <- levels_meta$sup_and_foldable
sup_and_foldable_labels <- get_level_col(sup_and_foldable_levels, "L")

merge_scores <- function(data, indicator) {
  indicator <- unlist(indicator)
  aggregated_data <- aggregated_scores[[indicator]] %>% select(countryname, !!sym(indicator), Score)
  left_join(data, aggregated_data, by = c("countryname" = "countryname", "Level_value" = as.character(indicator)))
}

# todo to improve (the structure should include levels hierarchy)
data_levels <- na.omit(Master_Table[, sup_and_foldable_levels]) %>% 
  dplyr::distinct() %>% 
  tidyr::gather(key = "Level", value = "Level_value", -countryname) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(Level) %>% 
  dplyr::group_modify(merge_scores) %>% 
  dplyr::arrange(countryname)
