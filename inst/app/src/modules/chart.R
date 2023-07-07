chart_ui <- function(id) {
  ns <- NS(id)
  echarts4r::echarts4rOutput(ns("scores_chart"))
}

chart_server <- function(input, output, session, data, message,
                         horizontal = FALSE) {
  
  output$scores_chart <- renderEcharts4r({
    shiny::validate(need(isTruthy(data()) && nrow(data()) > 0, message))

    foldable_level_colors <- data() %>% 
      dplyr::select(all_of(c(get_level_col(levels_meta$foldable), "color"))) %>% 
      dplyr::distinct() %>% 
      dplyr::pull(color)
    
    foldable_score <- paste0("Score_", levels_meta$foldable)
    custom_fields <- c(
      get_level_col(levels_meta$sup_and_foldable[-1]),
      paste0("Score_", setdiff(levels_meta$sup_and_foldable[-1], levels_meta$foldable))
    )
    rotate <- 45
    country_index <- 0
    if (horizontal) {
      rotate <- 0
      country_index <- 1
    }

    LH_vector <- unique(data()[[get_level_col(levels_meta$foldable)]])
    
    pivoted_data <- data() %>%
      pivot_wider(names_from = get_level_col(levels_meta$foldable), values_from = all_of(foldable_score)) %>%
      select('countryname', all_of(LH_vector))
    
    grouped_data <- pivoted_data %>% group_by(!!sym("countryname"))
    normalised_data <- grouped_data %>%
      summarize(across({{LH_vector}}, \(x) mean(x, na.rm = TRUE)))
    
	temp_custom_fields <- c(
      get_level_col(levels_meta$sup_and_foldable[ -c(1, length(levels_meta$sup_and_foldable))]),
      paste0("Score_", setdiff(levels_meta$sup_and_foldable[-1], levels_meta$foldable))
    )
    
    
    chart <- normalised_data %>%
      e_charts(countryname)
    for (lh in LH_vector) {
      chart <- chart %>% e_bar_(lh, stack = "foldable", bind = lh)
    }
    chart <- chart %>%
      e_color(color = foldable_level_colors) %>%
	   e_add_nested("custom", all_of(temp_custom_fields), .data= data()) %>%
      e_x_axis(axisLabel = list(interval = 0, rotate = rotate, fontSize = 10, fontWeight = "")) %>%
      e_grid(height = "310px", containLabel = TRUE) %>%
      e_tooltip(
        formatter = gen_chart_tooltip(levels_meta, country_index),
        transitionDuration = 0.3,
        backgroundColor = "#fff",
        padding = 15,
        extraCssText = "box-shadow: 0 3px 10px rgba(0, 0, 0, 0.3);"
      )
    if (horizontal) {
      chart <- e_flip_coords(chart)
    }
    chart
  })
}

single_tooltip_row <- function(label, value) {
  glue::glue(
    '"<p class=\'tooltip__text\'>" + {label} + ": <span class=\'tooltip__text--value\'>" + {value} + "</span></p>"'
  )
}
gen_chart_tooltip <- function(levels_meta, country_index) {
  
  custom_labels <- c("country", paste0("params.data.custom.", get_level_col(setdiff(levels_meta$sup_and_foldable[-1], levels_meta$foldable))), "value")
 
  custom_values <- c(
    glue::glue("params.value[{country_index}]"), 
    paste0("params.data.custom.Score_", setdiff(levels_meta$sup_and_foldable[-1], levels_meta$foldable)),
    glue::glue("params.value[{1 - country_index}]")
  )

 htmlwidgets::JS(
    'function(params) {',
    'var country = "Country"',
    'var value = "Value"',
    'return (',
    purrr::map2_chr(custom_labels, custom_values, single_tooltip_row) %>%
      paste(collapse = " + \n"),
    ")",
    "}"
  )
}
