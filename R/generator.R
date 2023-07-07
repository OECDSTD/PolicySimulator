#' Validate template and generate policy simulator app
#' 
#' Provide excel template file target application paths in order to generate policy simulator.
#' The template is automatically validated (see Details section to learn more).
#' 
#' @details 
#' Everytime you try to generate policy simulator, provided template is validated in two steps.
#' 
#' The first one, preprocessing validation, takes raw template data and validates it with extensive set of rules
#' (see \code{vignette("template", package = "PolicySimulator")} for more details).
#' 
#' Violation of any of rules, stops further policy generation steps and detailed report is 
#' generated in the target application path.
#' 
#' The second one, postprocessing validation, is run after the target application data is prepared.
#' This step checks whether generated scores matched the one provided in the template. 
#' Such check is only informative, it doesn't stop generation of policy simulator.
#' 
#' @param excel_template_path Path to excel template file.
#' @param target_app_path Target path where policy simulator should be generated. 
#'   Current working directory by default.
#' @export
generate_simulator <- function(excel_template_path, target_app_path = getwd(),
                               validation=TRUE, debug=FALSE) {
  if (!file.exists(excel_template_path)) {
    stop("Provided template file does not exist.")
  }
  if (!dir.exists(target_app_path)) {
    stop("Provided target path does not exist.")
  }

  options(scipen = 999)

  # Data import and initial values conversion
  template <- import_template_data(excel_template_path)
  template$Hierarchy <- convert_to_upper(template$Hierarchy)
  template$Data <- convert_empty_to_na(template$Data)
  template$Indicator <- convert_empty_to_na(template$Indicator) %>%
    convert_to_character(match_cols(., "Cond|Score"))

  levels_meta <- get_levels_meta(template$Hierarchy)
  
   if(debug){
    
    message("Debug is activated for this R session")
    Debug_global <<- TRUE
    
  }else{
    Debug_global <<- FALSE
  }

  # Data validation
  if(validation) {
      message("Validating provided template...")
      validator <- data.validator::create_validator()
      validate_hierarchy(template$Hierarchy, validator, target_app_path)
      validate_list(template$List, validator, target_app_path)
      validate_data(template$Data, validator, target_app_path)
      validate_answers(country_data = template$Data, indicator_data = template$Indicator, list_data = template$List, levels_meta = levels_meta, validator = validator, path = target_app_path)
      validate_indicator(template$Indicator, template$List, validator, levels_meta, target_app_path)
      template_file <- tempfile("template", fileext = ".Rmd")
      validation_error_callback(validator, target_app_path, template_file, report_file = "pre_processing_report.html")
  } else {
      message("Validation skipped")
  }

  ## request issue #119: UI elements should not be case sensitive (starting wtth "Radio" or "List")
  template$List <-
      template$List %>% dplyr::mutate(dplyr::across(dplyr::starts_with(c("Radio", "List")), toupper))
  template$Data <-
      template$Data %>% dplyr::mutate(answer = toupper(answer))

  # custom, faster approach
  message("Computing scores...")
  country_indicator_data <- dplyr::left_join(template$Data, template$Indicator, by = "LineID") %>% 
    dplyr::rename(`UI.Element` = `UI Element`) %>% 
    dplyr::filter(!is.na(LineID)) %>% 
    dplyr::mutate(LineID = paste0("num", trimws(LineID))) %>% 
    dplyr::mutate_at(vars(dplyr::starts_with("Cond")), ~numans_to_numline(., LineID)) %>%
    dplyr::mutate_at(vars(dplyr::starts_with("Score")), ~numans_to_numline(., LineID))
  numals_env <- get_numals_vals(country_indicator_data, levels_meta)
  ## save(country_indicator_data, numals_env, levels_meta, file = "../example/compute_score_data.rda")
  Master_Table <- country_indicator_data %>% 
    compute_score(numals_env, levels_meta) %>% 
    dplyr::mutate(template_value = Value, simulator_value = Score # * !!sym(get_level_col(levels_meta$evaluate, "W"))
                 ) %>% 
    dplyr::mutate(ChangedScore = Score) %T>% {
      message("Constructing application data...")
    } %>% 
    dplyr::mutate(
      ui_widget = attach_ui_elements(
        UI.Element, !!sym(levels_meta$selector_1), !!sym(levels_meta$selector_2), 
        LineID, numans, !!sym(levels_meta$foldable), template$List
      )
    ) %>% 
    dplyr::mutate(
      widget_id = paste0(
        !!sym(levels_meta$foldable), "-", !!sym(levels_meta$selector_1), "_", !!sym(levels_meta$selector_2), "_", LineID
      )
    )
  
  if(validation) {
      message("Post validation...")
      post_validator <- data.validator::create_validator()
      validate_scores(Master_Table, levels_meta, post_validator, target_app_path)
      validation_error_callback(
          post_validator, target_app_path, template_file, callback = warning, report_file = "post_processing_report.html"
      )
  } else {
      message("Post validation skipped")
  }
  
  Master_Table <- make_hierarchy_structure(Master_Table, levels_meta)

  message("Saving application data and files...")
  create_app_structure(template, target_app_path, Master_Table)
}

#' Get Policy Simulator Template skeleton
#' 
#' @param template_dir Path where the template should be saved.
#' 
#' @export
get_template <- function(template_dir = getwd()) {
  file.copy(system.file("template.xlsx", package = "PolicySimulator"), template_dir)
}
