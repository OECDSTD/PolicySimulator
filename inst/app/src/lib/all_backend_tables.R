# This file load the file that is created in data folder after Validation. It takes the input file and build other tables that are used.
if (!exists("Debug_global", envir = .GlobalEnv)) {
  Debug_global <<- FALSE
}

Master_Table <- readRDS("./data/Master_Table.rds")
levels_meta <- get_levels_meta(app_data$Hierarchy)
ind_to_label <- function(data, indicators) {
  data[, indicators] <- data[, gsub("C", "L", indicators)]
  data
}

# Creating  Tables for developing other tables with no NULL or NA.
scores_table <- Master_Table[!(is.na(Master_Table[[levels_meta$foldable]]) | Master_Table[[levels_meta$foldable]] == ""), ] %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(dplyr::across(!!get_level_col(levels_meta$all[-1], "O")))

# Unique Sequence Indicators
un_sq <- unique(scores_table[[get_level_col(levels_meta$foldable)]])

# Unique Sequence countryname names
countries <- unique(scores_table$countryname)

# For Indicator Vs countryname Graph and countryname comparision Graph
Aggregation_Table <- scores_table %>% 
  dplyr::select(c(levels_meta$all, get_level_col(levels_meta$all[-1], "W"), Score, widget_id)) %>% 
  dplyr::filter(!is.na(Score))

## Aggregation method looked up in "Apps" sheet of Excel template
## - get_apps_value() returns "DEFAULT" if missing in Excel template
## - for PMR aggregation logic, add line MetadataName "Aggregation", Name
##   "CORRECTEDWEIGHTS" to "Apps" sheet
aggregation_method <- get_apps_value(app_data, items = c("Aggregation"))
aggregated_scores <- aggregate_scores(Aggregation_Table, levels_meta,
                                      method = aggregation_method)
download_file_path <- get_apps_value(app_data, items = c("Download file (optional)"))


## add step to simplify reduce function
## rename "Score" columns: add level to make unique
## remove "WH" columns
aggregated_scores_rename <-
    lapply(names(aggregated_scores),
           function(x) {
               new_name_up <- paste0("Score_", x)
               aggregated_scores[[x]] %>%
                   dplyr::select(-starts_with("WH")) %>%
                   dplyr::rename(!!new_name_up := Score)
           })
names(aggregated_scores_rename) <- names(aggregated_scores)

######################################################################################

id_label_table <- scores_table %>% 
  dplyr::ungroup() %>%  
  dplyr::select(levels_meta$all[-1], get_level_col(levels_meta$all[-1])) %>% 
  dplyr::distinct()

replace_id_with_label <- function(data, level) {
  labels_data <- id_label_table %>% 
    dplyr::select(all_of(c(level, get_level_col(level)))) %>% 
    dplyr::distinct()
  data %>%
    dplyr::left_join(labels_data, by = level) %>%
    dplyr::select(-!!sym(level)) %>% 
    dplyr::rename(!!sym(level) := !!sym(get_level_col(level)))
}

# For BAR Graphs and Column Graph
Graphs_data <- aggregated_scores[[levels_meta$foldable]] %>% 
  dplyr::select(!!sym(levels_meta$selector_2), !!sym(levels_meta$selector_1), !!sym(levels_meta$foldable), Score) %>% 
  replace_id_with_label(levels_meta$foldable)

# For Transpose Indicator Vs Country
Converted_Graph_data <- tidyr::spread(Graphs_data, !!sym(levels_meta$foldable), Score, fill = 0) %>% 
  dplyr::mutate_if(is.numeric, round, 3)
class(Converted_Graph_data) <- "data.frame" # highcharter doesn't work with tibbles fro some reason

foldable_levels_info <- dplyr::left_join(app_data$Data, app_data$Indicator, by = "LineID") %>% 
  dplyr::select(!!sym(levels_meta$selector_1), !!sym(get_level_col(levels_meta$selector_2)), 
                !!sym(levels_meta$foldable), !!sym(get_level_col(levels_meta$foldable)), Color) %>% 
  dplyr::mutate(Color = gsub(" ", "", tolower(Color))) %>% 
  dplyr::distinct() %>% 
  stats::setNames(c("selector_1", "selector_2", "id", "label", "color"))

## Create data frame with scores and labels for each level ======================
## unused: only works for one iteration, e.g. STRI; ECO PMR has 4 levels, CH0-CH3
## join_scores <- function(level_up, level_down, data) {
##   new_name_down <- paste0("Score_", level_down)
##   new_name_up <- paste0("Score_", level_up)
##   label <- paste0("Score_", level_down)
##   dplyr::left_join(
##     data[[level_down]] %>% dplyr::rename(!!new_name_down := Score),
##     data[[level_up]] %>% dplyr::rename(!!new_name_up := Score),
##     by = c("countryname", level_up)
##   )
## }

sub_and_foldable_sectors <- levels_meta$sup_and_foldable[-1]
sub_and_foldable_sectors_labels <- get_level_col(sub_and_foldable_sectors)

id_label_table_filtered <- id_label_table %>%
  dplyr::select(all_of(c(sub_and_foldable_sectors, sub_and_foldable_sectors_labels))) %>%
  dplyr::distinct()

id_color_table <- foldable_levels_info %>%
  dplyr::select(id, color) %>%
  dplyr::rename(!!levels_meta$foldable := "id") %>%
  dplyr::distinct()

add_labels_colors <- function(df) {
  df %>%
    ## dplyr::select(-starts_with("WH")) %>%
    dplyr::left_join(., id_label_table_filtered, by = sub_and_foldable_sectors) %>%
    dplyr::left_join(., id_color_table, by = levels_meta$foldable) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    dplyr::group_by_at(vars(get_level_col(levels_meta$foldable)))
}

## sub_and_foldable_scores_df <-  sub_and_foldable_sectors %>%
##   purrr::reduce(join_scores, data = aggregated_scores, .dir = "backward") %>% 
##   add_labels_colors()

## inner_join operation communtative
sub_and_foldable_scores_df <-
    purrr::reduce(
               .x = aggregated_scores_rename[sub_and_foldable_sectors],
               .f = dplyr::inner_join
           ) %>%
    add_labels_colors()


col2hex <- function(cname) {
  if (startsWith(cname, "#")) {
    return(cname)
  } else {
    colMat <- col2rgb(cname)
    return(sprintf("#%02X%02X%02X", colMat[1], colMat[2], colMat[3]))
  }
}

 

scss_path <- file.path(getwd(), "styles", "modules", "_config.scss")

 

config.scss <- readLines(scss_path, warn = FALSE)
new_values <- unique(Master_Table$Color)
nb_colors <- length(new_values)

 

for (i in 15:29) {
  if (i - 14 > nb_colors) {
    break
  }

  color_number <- i - 14
  color_value <- col2hex(new_values[color_number])
  config.scss[i] <- paste0("$color-", color_number, ": ", color_value, ";")
}

writeLines(config.scss, scss_path)

