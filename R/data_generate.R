#' List of Sheets required in the template
template_data_sheets <- c("Hierarchy", "Data", "Indicator", "Apps", "List")

#' Source template Excel file sheets and export to separate RDS files
#'
#' @param template Template resulting data to be exported
#' @param app_path Path to target application directory
export_template_data <- function(template, app_path) {

  for (data in names(template)) {
    rio::export(template[[data]], glue::glue("{app_path}/src/data/{data}.rds"))
  }
}

#' Sheet Column Types
#'
#' Set Excel sheet column types for loading.
#'
#' Set type of columns starting with "Score" and "Cond" to "text".
#'
#' @param sheet template data sheet
#' @examples
#' excel_template_path <- "../TAD STRI/Matrix_Simulator_Template_APEC_scaleback.xlsx"
#' sheet_col_types("Indicator", excel_template_path)
#' sheet_col_types()
sheet_col_types <- function(sheet="Data", excel_template_path) {
    col_types_out <-
        if (sheet == "Indicator") {
            dat <- readxl::read_excel(excel_template_path,
                                      sheet = sheet, na = "",
                                      n_max = 0)
            col_types <-
                names(dat) %>%
                stringr::str_detect("^Score|^Cond") %>%
                ifelse("text", "guess")
            names(col_types) <- names(dat)
            col_types
        } else {
            "guess"
        }
    return(col_types_out)
}

#' Import template Excel file sheets as list
#'
#' @param excel_template_path Path to template excel file.
import_template_data <- function(excel_template_path) {
  template <- list()
  for (sheet in template_data_sheets) {
      template[[sheet]] <-
          readxl::read_excel(
                      excel_template_path, sheet = sheet, na = "",
                      col_types = sheet_col_types(sheet, excel_template_path),
                      .name_repair = "check_unique")
  }
  ## Issue 126 Column names are case sensitive
  names(template$Data) <- correct_case(names(template$Data))

  return(template)
}

#' Correct case of column names
#'
#' Report missing columns
#'
#' @param x character vector of column names
#' @examples
#' names_old <- c("country_id", "countryname", "LineID", "QuestionCode", "answer", "numans", "value", "Source", "comment")
#' correct_case(x = names_old)

correct_case <- function(x) {
    names_new <- c("Comment", "country_id", "countryname", "LineID",
                   "QuestionCode", "answer", "numans", "Value", "Source")
    names_new_sorted <-
        names_new[match(tolower(x), tolower(names_new))]
    if(any(is.na(names_new_sorted))) {
        missing <- setdiff(names_new, names_new_sorted)
        stop("the following columns could not be found: ", paste(missing, collapse = ", "))
    }
    changed_to <- setdiff(names_new_sorted, x)
    changed_from <- setdiff(x, names_new_sorted)
    if(length(changed_to) > 0) warning("the following columns have been renamed during import: \n  ",
                                       paste(changed_from, "->", changed_to, collapse = "\n  "))
    return(names_new_sorted)
}

#' Import RDS Simulator base files
#'
#' @param path Path to simulator rds files
import_simulator_data <- function(path = "data") {
  template <- list()
  for (sheet in template_data_sheets) {
    template[[sheet]] <- readRDS(glue::glue("{path}/{sheet}.rds"))
  }
  return(template)
}

#' Convert selected columns to upper values
#'
#' @param data Table object
#' @param columns Columns to convert
convert_to_upper <- function(data, columns = colnames(data)[-1]) {
  dplyr::mutate_at(data, columns, toupper)
}

#' Convert empty string to NA
#' 
#' @param x string
convna <- function(x) {
  if (is.character(x)) {
    x <- ifelse(x == "", NA, x)
  }
  return(x)
}

#' Convert empty cell to NA
#'
#' @param data Table object
convert_empty_to_na <- function(data) {
  dplyr::mutate_all(data, convna)
}

#' Convert selected columns to character
#'
#' @param data Table object
#' @param columns Columns to convert
convert_to_character <- function(data, columns = colnames(data)) {
  dplyr::mutate_at(data, columns, as.character)
}

#' Get Apps Value
#'
#' Get value from "Apps" sheet of template.
#'
#' Function to extract metadata item from "Apps" sheet. If not found, return
#' "DEFAULT"
#' @param app_data Application data object created with
#'     \code{import_simulator_data}
#' @param item Character the metadata item(s) of interest, e.g. "Copyright" or
#'     \code{c("About", "Aggregation")}
#' @return Character string
#' @examples
#' app_data <- import_simulator_data(path = file.path(root_dir, "data"))
#' get_apps_value(app_data, items = "Aggregation")
#' get_apps_value(app_data, items = c("Logo", "Aggregation"))

get_apps_value <- function(app_data, items) {
    res <- sapply(items,
                  function(x) {
                      value <- app_data$Apps$Name[app_data$Apps$MetadataName == x]
                      ifelse(length(value)==0, "DEFAULT", value)
                  },
                  USE.NAMES = TRUE)
    return(res)
}

#' Calculate level-based scores
#'
#' The function takes original scores data and levels meta information, and performs
#' aggregation for all weighted levels that were defined in hierarchy Sheet.
#'
#' @param levels Hierarchy levels.
#' @param aggregation_data Data storing scores, and weights for each hierarchy
#'     level.
#' @param levels_meta Named list of hierarchy levels, including "weighted" (e.g.
#'     CH0, CH1, CH2); Note: for PMR aggregation, \code{levels_meta$weighted}
#'     must also contain last value of \code{levels} (e.g. "CH3"); for STRI it
#'     mustn't.
#' @param score_column Column to use for Score values, default: "Score".
#' @param correct_weights Logical set WH columns to zero if score missing (PMR
#'     logic).
#' @param weighted_average Logical divide sum of Score by sum of corrected
#'     weights (PMR logic).
#' @param weighted_product   
#' @examples
#' e <- rlang::env()
#' load(file = "../example/compute_score_data.rda", envir = e)
#' ## used in ./inst/app/src/lib/all_backend_tables.R
#' Aggregation_Table <- readRDS(file = "../example/Aggregation_Table.rds") # generated in ./vignettes/aggregation.Rmd (based on "./inst/app/src/lib/all_backend_tables.R")
#' levels <- e$levels_meta$all
#' aggregation_levels <- purrr::map((length(levels) - 1):1,~levels[1:(1 + .)])
#' level_score(levels = aggregation_levels[[1]], aggregation_data = Aggregation_Table, levels_meta = e$levels_meta, score_column = "Score")



level_score_wSum <- function(levels,
                        aggregation_data,
                        levels_meta,
                        score_column="Score") {

  #grouping of the questions and then multiply score column by current weights, replace in score
  aggregation_data_1 <-
    aggregation_data %>%
    dplyr::group_by_at(vars(one_of(levels))) %>% 
    dplyr::mutate(!!sym(score_column) := !!sym(score_column) * !!sym(get_level_col(dplyr::last(levels), "W")))

    aggregation_data_out <-
    if (!dplyr::last(levels) %in% levels_meta$weighted) {
      ##process leaf level
      ## last level (leaf questions) are evaluated and are not in levels_meta$weighted, only higher levels are aggregated 
      aggregation_data_1
    } else {
      ##process all reamaining levels
      ## for columns in levels_meta$weighted, calculate group size
      group_n <- aggregation_data_1 %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n)
      
        ##Calculate sum of score and replace in score, Calculate sum of weight and replace in weight 
      aggregation_data_2 <- aggregation_data_1%>%
          dplyr::summarise_at(vars(score_column, get_level_col(levels[-1], "W")), ~ sum(., na.rm = TRUE))
        
        ## STRI: divide weights by group size
        aggregation_data_2$group_n <- group_n
        aggregation_data_2 <- aggregation_data_2 %>%
          dplyr::mutate_at(vars(get_level_col(levels[-1], "W")), ~ . / group_n) %>%
          dplyr::select(- group_n)
        
        if(Debug_global){
          p_aggregation_data_2 <-  aggregation_data_2
          p_aggregation_data_2 %>% as_tibble() %>% print(n= Inf)
        }
        aggregation_data_2
        
        
    }
  
  return(dplyr::ungroup(aggregation_data_out))
}

level_score_wproduct <- function(levels,
                        aggregation_data,
                        levels_meta,
                        score_column="Score") {
  
  
  #grouping of the questions and then multiply score column by current weights, replace in score
  
  
  aggregation_data_1 <-
    aggregation_data %>%
    dplyr::group_by_at(vars(one_of(levels))) %>%
    dplyr::mutate(!!sym(score_column) := !!sym(score_column) * !!sym(get_level_col(dplyr::last(levels), "W")))
  
  
  aggregation_data_out <-
    if (!dplyr::last(levels) %in% levels_meta$weighted) {
      ##process leaf level
      ## last level (leaf questions) are evaluated and are not in levels_meta$weighted, only higher levels are aggregated 
      aggregation_data_1
    } else {
      ##process all remaining levels
      ## for columns in levels_meta$weighted, calculate sum and group size
      group_n <- aggregation_data_1 %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull(n)
      
      ## Calculate Product of score_column, calculate sum of weights and merge both to aggregation_data_2
        aggregation_data_prod_1 <- aggregation_data_1 %>% dplyr::summarise_at(vars(score_column), ~ abs(prod(., na.rm = TRUE))) 
        aggregation_data_prod_2 <- aggregation_data_1 %>% dplyr::summarise_at(vars(get_level_col(levels[-1], "W")), ~ mean(., na.rm = TRUE))
        aggregation_data_2 <- merge(aggregation_data_prod_1, aggregation_data_prod_2 )
        
        


        # STRI: divide weights by group size
        # aggregation_data_2$group_n <- group_n
        # aggregation_data_2 %>%
        #   dplyr::mutate_at(vars(get_level_col(levels[-1], "W")), ~ . / group_n) %>%
        #   dplyr::select(- group_n)

        if(Debug_global){
          p_aggregation_data_2 <-  aggregation_data_2
          p_aggregation_data_2 %>% as_tibble() %>% print(n= Inf)
        }
        aggregation_data_2 
    }
  
  
  return(dplyr::ungroup(aggregation_data_out))
}


level_score_waverage <- function(levels,
                        aggregation_data,
                        levels_meta,
                        score_column="Score") {

  #     aggregation_data_1 <-
  #         aggregation_data_1 %>%
  #         dplyr::mutate_at(
  #                    vars(get_level_col(levels[-1], "W")),  ~ . * ifelse(is.nan(!!sym(score_column)), 0, 1)
  #                )
  
  
  #Ignore -1 values in the numerator i.e -1 is treated as a missing value and is not added to the score, scores are replaced by a 0
  
      aggregation_data <-
      aggregation_data %>%
      dplyr::mutate_at(
        vars(score_column),  ~ . * ifelse(is.finite(.), ifelse(. == -1, 0, 1), NA)
                      )
    
  
  #grouping of the questions and then multiply score column by current weights, replace in score
  aggregation_data_1 <-
    aggregation_data %>%
    dplyr::group_by_at(vars(one_of(levels))) %>% 
  dplyr::mutate(!!sym(score_column) := !!sym(score_column) * !!sym(get_level_col(dplyr::last(levels), "W")))
    
  
  aggregation_data_out <-
    if (!dplyr::last(levels) %in% levels_meta$weighted) {
      ##process leaf level
      ## last level (leaf questions) are evaluated and are not in levels_meta$weighted, only higher levels are aggregated 
      aggregation_data_1
    } else {
      ##process all remaining levels
      ## for columns in levels_meta$weighted, calculate sum and group size
      group_n <- aggregation_data_1 %>% dplyr::summarise_at(vars( get_level_col(dplyr::last(levels), "W")), ~ sum(., na.rm = TRUE)) %>%
        dplyr::pull(get_level_col(dplyr::last(levels), "W"))
    
      
      aggregation_data_sum <- aggregation_data_1%>%
        #dplyr::summarise_at(vars(score_column, get_level_col(levels[-1], "W")), ~ sum(., na.rm = TRUE))
        dplyr::summarise_at(vars(score_column), ~ sum(., na.rm = TRUE)) 
      
      aggregation_data_w <- aggregation_data_1%>%
        dplyr::summarise_at(vars(get_level_col(levels[-1], "W")),~ mean(., na.rm = TRUE) )
      
      aggregation_data_2 <- merge(aggregation_data_sum, aggregation_data_w)
        
        ## PMR: divide sum of Score by sum of corrected weights
        # aggregation_data_2 <- aggregation_data_2 %>%
        #   dplyr::mutate(!!sym(score_column) := !!sym(score_column) / !!sym(get_level_col(dplyr::last(levels), "W")))
    
      aggregation_data_2$group_n <- group_n
      aggregation_data_2 <- aggregation_data_2 %>%
         dplyr::mutate(!!sym(score_column) := !!sym(score_column) / group_n)
      aggregation_data_2<- aggregation_data_2 %>%  dplyr::select(- group_n)
      
      if(Debug_global){
        p_aggregation_data_2 <-  aggregation_data_2
        p_aggregation_data_2 %>% as_tibble() %>% print(n= Inf)
      }
      aggregation_data_2 
      
        
    }
  
  
  return(dplyr::ungroup(aggregation_data_out))
}


level_score_correctedw <- function(levels,
                        aggregation_data,
                        levels_meta,
                        score_column="Score") {
  

  #     aggregation_data_1 <-
  #         aggregation_data_1 %>%
  #         dplyr::mutate_at(
  #                    vars(get_level_col(levels[-1], "W")),  ~ . * ifelse(is.nan(!!sym(score_column)), 0, 1)
  #                )

  
  #Ignore -1 values in the denominator i.e Sum of normalized weights for non -1 values, weights are set to 0 
     aggregation_data <-
      aggregation_data%>%
      dplyr::mutate_at(
        vars(get_level_col(levels[-1], "W")),  ~ . * ifelse(!!sym(score_column) == -1, NA , 1)
                      )
  #Ignore -1 values in the numerator i.e -1 is treated as a missing value and is not added to the score, scores are replaced by a 0
  
    aggregation_data <-
      aggregation_data %>%
      dplyr::mutate_at(
        vars(score_column),  ~ . * ifelse(is.finite(.), ifelse(. == -1, 0, 1), NA)
                       )
    
    #grouping of the questions and then multiply score column by current weights, replace in score
     aggregation_data_1 <-
      aggregation_data %>%
      dplyr::group_by_at(vars(one_of(levels))) %>%
    dplyr::mutate(!!sym(score_column) := !!sym(score_column) * !!sym(get_level_col(dplyr::last(levels), "W")))
  
  
   aggregation_data_out <-
    if (!dplyr::last(levels) %in% levels_meta$weighted) {
      ##process leaf level
      ## last level (leaf questions) are evaluated and are not in levels_meta$weighted, only higher levels are aggregated 
      aggregation_data_1
    } else {
      ##process all remaining levels
      ## for columns in levels_meta$weighted, calculate sum and group size
      group_n <- aggregation_data_1 %>% dplyr::summarise_at(vars( get_level_col(dplyr::last(levels), "W")), ~ sum(., na.rm = TRUE)) %>%
                                      dplyr::pull(get_level_col(dplyr::last(levels), "W"))

      #Divide weights by itself to normalise weights for current level to 1
      
        # aggregation_data_int <- aggregation_data_1 %>%
        #   dplyr::mutate(!!sym(get_level_col(dplyr::last(levels), "W")) := !!sym(get_level_col(dplyr::last(levels), "W")) / 
        #                   !!sym(get_level_col(dplyr::last(levels), "W")))
        
      
      aggregation_data_sum <- aggregation_data_1%>%
        dplyr::summarise_at(vars(score_column), ~ sum(., na.rm = TRUE)) 
      
      aggregation_data_w <- aggregation_data_1%>%
        dplyr::summarise_at(vars(get_level_col(levels[-1], "W")),~ mean(., na.rm = TRUE) )
      
      aggregation_data_2 <- merge(aggregation_data_sum, aggregation_data_w)
        
      
      aggregation_data_2$group_n <- group_n
      aggregation_data_2 <- aggregation_data_2 %>%
        dplyr::mutate(!!sym(score_column) := !!sym(score_column) / group_n)
      aggregation_data_2<- aggregation_data_2 %>%  dplyr::select(- group_n)
    
        ## PMR: divide Score by corrected weights
        # aggregation_data_2 <- aggregation_data_2 %>%
        #   dplyr::mutate(!!sym(score_column) := !!sym(score_column) / !!sym(get_level_col(dplyr::last(levels), "W")))
        
      if(Debug_global){
        p_aggregation_data_2 <-  aggregation_data_2
        p_aggregation_data_2 %>% as_tibble() %>% print(n= Inf)
      }
      aggregation_data_2
    }
  
  return(dplyr::ungroup(aggregation_data_out))
}



#' Aggregate level-based scores
#' 
#' The function takes original scores data and levels meta information, and performs 
#' aggregation for all weighted levels that were defined in hierarchy Sheet.
#' 
#' @param aggregation_data Data storing scores, and weights for each hierarchy level.
#' @param levels_meta Hierarchy levels meta information object.
#' @param score_column Name of the column that stores scores to be aggregated.
aggregate_scores <- function(aggregation_data, levels_meta,
                             method="DEFAULT",
                             ... # additional parameters like "score_column" get
                                 # passed to level_score function
                             ) {
  levels <- levels_meta$all
  aggregation_levels <- purrr::map(
    (length(levels) - 1):1,
    ~levels[1:(1 + .)]
  )
  ## use provided data at first step of iteration
  aggregations <- aggregation_data
  aggregations_list <- list()
  for (levels in aggregation_levels) {
      aggregations_list[[dplyr::last(levels)]] <-
         ## if (method == "CORRECTEDWEIGHTS") {
             if (dplyr::last(levels) %in% levels_meta$correctedweights){
              level_score_correctedw(levels = levels,
                          aggregation_data = aggregations,
                          levels_meta = levels_meta,
                          ...
                          )
          } else if (dplyr::last(levels) %in% levels_meta$weightedproduct) {
            level_score_wproduct(levels = levels,
                        aggregation_data = aggregations,
                        levels_meta = levels_meta,
                        ...
                         )
            
          } else if (dplyr::last(levels) %in% levels_meta$weightedsum) {
              level_score_wSum(levels = levels,
                          aggregation_data = aggregations,
                          levels_meta = levels_meta,
                          ...
                          )
            
          } else if (dplyr::last(levels) %in% levels_meta$weightedaverage) {
            level_score_waverage(levels = levels,
                        aggregation_data = aggregations,
                        levels_meta = levels_meta,
                        ...
                       )
        #re-use the function weighted sum for the fist level of leaf questions to evaluate
          
          } else { 
            level_score_wSum(levels = levels,
                        aggregation_data = aggregations,
                        levels_meta = levels_meta,
                        ...
            )
          }
    ## update input data with calculation results for subsequent iteration step
    aggregations <- aggregations_list[[dplyr::last(levels)]]

  }
  aggregations_list
}

