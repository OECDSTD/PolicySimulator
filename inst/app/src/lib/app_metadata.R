# Creating Generic Selector Using Filter from DPLYR , and unique function and maching it.
app_meta <- list()
########  Generic Selectors #########
app_meta$Selector1_Array <- unique(na.omit(scores_table[[levels_meta$selector_1]])) %>% 
  setNames(unique(na.omit(scores_table[[get_level_col(levels_meta$selector_1)]])))
app_meta$Selector1_Name <- app_data$Hierarchy %>% 
  dplyr::filter(Code == levels_meta$selector_1) %>% 
  dplyr::pull(Name)
get_selector_2_list <- function(data, selector_1, levels_meta, default = "") {
  selector_2_list <- data %>% 
    dplyr::filter(!!sym(levels_meta$selector_1) == selector_1) %>% 
    pull(!!sym(levels_meta$selector_2)) %>% 
    na.omit() %>% 
    unique()
  c("", selector_2_list)
}
app_meta$Selector2_Array <- get_selector_2_list(scores_table, app_meta$Selector1_Array[1], levels_meta)
app_meta$Selector2_Name <- app_data$Hierarchy %>% 
  dplyr::filter(Code == levels_meta$selector_2) %>% 
  dplyr::pull(Name)
app_meta$Compare_Selector_Array <- get_selector_2_list(scores_table, app_meta$Selector1_Array[1], levels_meta, NULL)
app_meta$Compare_Selector_Name <- app_data$Hierarchy %>% 
  dplyr::filter(Code == levels_meta$compare_selector) %>% 
  dplyr::pull(Name)
####### Dashboard Title #######
app_meta$Title <- dplyr::filter(app_data$Apps, MetadataName == "Application name") %>% dplyr::pull(Name)
app_meta$Title_Link <- dplyr::filter(app_data$Apps, MetadataName == "Application name") %>% dplyr::pull(Link)
####### Dashboard Footer #######
app_meta$Footer <- dplyr::filter(app_data$Apps, MetadataName == "Copyright") %>% dplyr::pull(Name)
app_meta$Footer_Link <- dplyr::filter(app_data$Apps, MetadataName == "Copyright") %>% dplyr::pull(Link)
####### LOGO  #######
app_meta$Logo <- dplyr::filter(app_data$Apps, MetadataName == "Logo") %>% dplyr::pull(Name)
app_meta$Logo_Updated <- "assets/logo-oecd.png"
app_meta$Logo_Link <- dplyr::filter(app_data$Apps, MetadataName == "Logo") %>% dplyr::pull(Link)
####### About   #######
app_meta$About <- dplyr::filter(app_data$Apps, MetadataName == "About") %>% dplyr::pull(Name)
app_meta$About_Link <- dplyr::filter(app_data$Apps, MetadataName == "About") %>% dplyr::pull(Link)
app_meta$About_Link <- gsub('"', "'", app_meta$About_Link)
#######  Homepage   #######
app_meta$Homepage <- dplyr::filter(app_data$Apps, MetadataName == "Homepage") %>% dplyr::pull(Name)
app_meta$Homepage_Link <- dplyr::filter(app_data$Apps, MetadataName == "Homepage") %>% dplyr::pull(Link)
app_meta$Homepage_Link <- gsub('"', "'", app_meta$Homepage_Link)
