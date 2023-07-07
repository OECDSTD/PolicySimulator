server <- function(input, output, session) {
  
  # intro ======================================================================
  session$userData$selector_1_value <- reactive({input$Selector1_inputid})
  session$userData$selector_2_value <- reactive({input$Selector2_inputid})
  session$userData$compare_selector_value <- reactive({input$Compare_Selector_inputid})
  session$userData$search2 <- reactive({input$search2})
  
   numals_trigger <- reactiveVal(1)
   scores_trigger <- reactiveVal(1)
   graph_trigger <- reactiveVal(1)
   restore_trigger <- reactiveVal(1)
   called_ids <- reactiveVal(NULL)
   stop_table <- reactiveVal(FALSE)  

  numals_env <- reactiveVal()
  scores_table_per_indicator <- reactiveVal()
  scores_table_per_selector_2 <- reactiveVal()
  restore_mode <- reactiveVal(isolate("_inputs_" %in% names(getQueryString(session))))
  stop_table <- reactiveVal(isolate("_inputs_" %in% names(getQueryString(session))))
  
  observeEvent(input$Selector1_inputid, {
    req(input$Selector1_inputid)
    per_ind_scores_table <- scores_table %>% 
      dplyr::filter(!!sym(levels_meta$selector_1) == input$Selector1_inputid)
    scores_table_per_indicator(per_ind_scores_table)
   
    colnames(per_ind_scores_table)
    if (!restore_mode()) {
      updateSelectInput(
        session, "Selector2_inputid", 
        choices = get_selector_2_list(per_ind_scores_table, input$Selector1_inputid, levels_meta)
      )
      updateSelectInput(
        session, "Compare_Selector_inputid", 
        choices = get_selector_2_list(per_ind_scores_table, input$Selector1_inputid, levels_meta, NULL)
      )  
    }
    restore_mode(FALSE)
  })
  
  
  
  output$chart_title <- renderText({
    req(input$Selector1_inputid)
    sector_label <- id_label_table_filtered %>% 
      dplyr::filter(!!sym(levels_meta$selector_1) == input$Selector1_inputid) %>% 
      dplyr::pull(!!sym(get_level_col(levels_meta$selector_1))) %>% 
      dplyr::first()
      
    glue::glue("Indicators vs. Countries - {sector_label}")
  })
  
  observeEvent(c(input$Selector1_inputid, input$Selector2_inputid), {
    req(input$Selector1_inputid)
    req(input$Selector2_inputid)
    per_selector_2_scores_table <- scores_table_per_indicator() %>% 
      dplyr::filter(!!sym(levels_meta$selector_2) == input$Selector2_inputid)
    
    numals_env(get_numals_vals(per_selector_2_scores_table, levels_meta))
    scores_table_per_selector_2(per_selector_2_scores_table)
  })
  
  per_selector_ids <- reactive({
    req(input$Selector1_inputid)
    foldable_levels_info %>% 
      dplyr::filter(selector_1 == input$Selector1_inputid) %>% 
      dplyr::distinct()
  })
  
  per_selector_2_ids <- reactive({
    req(input$Selector2_inputid)
    per_selector_ids() %>% 
      dplyr::filter(selector_2 == input$Selector2_inputid) %>% 
      dplyr::distinct()
  })
  
  # CHARTS =====================================================================
  # Aggregation Logic
  # In R Shiny , In order to change the values in frontend the data frame needs
  # to be made reactive. In this case there is interactivity assigned with more
  # than one component, hence calculation needs to be performed at each
  # component value change. Hence in order to meet the functional requirement a
  # dynamic aggregation logic also acts as reactive and is called every time
  # when a component value changes dynamically.
  
  # reactive data frame with aggregated values
  all_countries_df <- reactive({
    req(input$Selector1_inputid)
    sub_and_foldable_scores_df %>%
      dplyr::filter(!!sym(levels_meta$selector_1) == input$Selector1_inputid)
  })

  callModule(chart_server, "countries", data = all_countries_df, message = "PLEASE SELECT SECTOR")

  comparison_countries_df <- reactive({
    req(input$Selector2_inputid)
    graph_trigger()

    data_selector_original <- all_countries_df() %>%
      dplyr::filter(!!sym(levels_meta$selector_2) == input$Selector2_inputid)
    data_selector_edited <- NULL
    data_selector_compared <- NULL

    if (isTruthy(input$Compare_Selector_inputid)) {
      data_selector_compared <- all_countries_df() %>%
        dplyr::filter(!!sym(levels_meta$compare_selector) %in% input$Compare_Selector_inputid)
    }

    # To structure the data that can be used by echarts4r graphs
    if (!all(na.omit(scores_table_per_selector_2()$Value == scores_table_per_selector_2()$EditedValue))) {

      # Get data frame with edited scores
      aggregated_scores_edited <- scores_table_per_selector_2() %>%
        aggregate_scores(levels_meta, score_column = "EditedScore")
      aggregated_sector_scores_edited <- aggregated_scores_edited[[levels_meta$foldable]]

      # Set score labels for both levels
      main_score_label <- paste0("Score_", levels_meta$selector_1)
      foldable_score_label <- paste0("Score_", levels_meta$foldable)

      # Add respective labels and colors to each level
      Graphs_data_edited <- add_labels_colors(aggregated_sector_scores_edited)
      # Add column with summed scores and rename EditedScore column
      data_selector_edited <- Graphs_data_edited %>%
        dplyr::mutate(!!main_score_label := sum(Graphs_data_edited$EditedScore)) %>%
        dplyr::rename(!!foldable_score_label := "EditedScore")

      # Add "Edited" suffix to country name
      country_name <- data_selector_edited[[levels_meta$selector_2]]
      data_selector_edited[[levels_meta$selector_2]] <- paste(country_name, "Edited")
    }

    # Collect data to generate comparison chart
    comparison_aggregation_data <- rbind(
      data_selector_original,
      data_selector_edited,
      data_selector_compared
    )
  })
  callModule(
    chart_server, "comparison", data = comparison_countries_df,
    horizontal = TRUE, message = "PLEASE SELECT THE COUNTRY NAME"
  )

  # DYNAMIC DATA TABLE =========================================================
 output$foldable_tables <- renderUI({
    req(per_selector_2_ids())
    per_selector_2_ids() %>% 
      purrr::pmap(function(selector_1, selector_2, id, label, color) {
        Agregagates_scores<- scores_table_per_selector_2() %>%
            aggregate_scores(levels_meta, score_column = "EditedScore")
        Graphs_data_edited <-Agregagates_scores[[levels_meta$foldable]] %>%
                  select(!!sym(levels_meta$selector_2), !!sym(levels_meta$selector_1), !!sym(levels_meta$foldable), EditedScore) %>% 
                  replace_id_with_label(levels_meta$foldable)
                label_col <- levels_meta$foldable
                value <- round(Graphs_data_edited %>%
                         dplyr::filter(!!sym(label_col) == label, !!sym(levels_meta$selector_2) == input$Selector2_inputid) %>% 
                         dplyr::pull(EditedScore), 3)
        title_with_value <- glue::glue("{label}:      {stringr::str_pad(value, width = 30, side = 'right', pad = ' ')}")
        dq_box(
          id = paste0("dq", id), title = title_with_value, 
          single_table_output(id),
          fill = TRUE, width = 12, bg_color = "#fff", open_callback = TRUE,
          collapsed = TRUE
        )
      }) %>% 
      shiny::tagList()
  })
  outputOptions(output, "foldable_tables", suspendWhenHidden = TRUE)
  
  observeEvent(numals_trigger(), {
    previous_scores <- scores_table_per_selector_2()$EditedValue
    new_scores_table <- scores_table_per_selector_2() %>% 
      compute_score(numals_env(), levels_meta, score_col = "EditedScore", update = TRUE) %>% 
      dplyr::ungroup()
    current_scores <- new_scores_table$EditedValue
    changed_row <- which(previous_scores != current_scores)
    if (length(changed_row)) {
      scores_table_per_selector_2({
        tmp_scores_table_per_selector_2 <- new_scores_table
        tmp_scores_table_per_selector_2[changed_row, ] <- tmp_scores_table_per_selector_2[changed_row, ] %>%  
          dplyr::mutate(
            ui_widget = attach_ui_elements(
              UI.Element, !!sym(levels_meta$selector_1), !!sym(levels_meta$selector_2), LineID,
              new_scores_table$EditedValue[changed_row], !!sym(levels_meta$foldable), app_data$List

            )
          )
        tmp_scores_table_per_selector_2
      })
      graph_trigger(runif(1))
    }
  }, ignoreInit = TRUE)
  
  if(Debug_global){
    fichier_sortie <- file("output.csv", "w")
    sink(fichier_sortie, type = "output")
    
    output <- capture.output({
      print(aggregated_scores)
    })
    
    sink(type = "output")
    close(fichier_sortie)
    
    write.csv(data.frame(Affichage = output), "output_structured.csv", row.names = FALSE, quote = TRUE) 
  }
  
  update_numals_env <- function(ids, numals_env) {
    numals_env({
      numval_env_temp <- numals_env()
      for (id in names(ids)) {
        widget_spec <- rev(strsplit(id, "_|-")[[1]])
        numval_env_temp[[widget_spec[3]]][[widget_spec[2]]][[widget_spec[1]]] <- as.numeric(ids[[id]])
      }
      numval_env_temp
    })
  }
  
  onRestored(function(state) {
    if (!is.null(state$input$Selector2_inputid)) {
      update_dq_box("box1", collapsed = TRUE)
      update_dq_box("box2", collapsed = FALSE)
      update_dq_box("box3", collapsed = FALSE)
    }

    table_vals <- state$input[setdiff(c(names(state$input)), c("Selector2_inputid", "Compare_Selector_inputid", "Selector1_inputid"))]
    update_numals_env(table_vals, numals_env)
    stop_table(FALSE)
    numals_trigger(runif(1))
  })
  
  widget_ids <- reactive({na.omit(scores_table_per_selector_2()[["widget_id"]])})
  widget_ui_elements <- reactive({na.omit(scores_table_per_selector_2()[["UI.Element"]])})
  observe({
    ids_to_call <- setdiff(widget_ids(), called_ids())
    called_ids(unique(c(called_ids(), ids_to_call)))
    widgets_to_call <- widget_ui_elements()[ids_to_call %in% widget_ids()]
    purrr::walk2(ids_to_call, widgets_to_call, function(widget_id, widget_ui_element) {
      observeEvent(input[[widget_id]], {
        condition <- TRUE
        answer <- as.numeric(input[[widget_id]])
        condition_message <- ""
        if (is_text(widget_ui_element)) {
          answer <- input[[gsub("-", "-val_", widget_id)]]
          widget_conditions <- na.omit(app_data$List[[widget_ui_element]])
          widget_values <- na.omit(app_data$List[[paste0(widget_ui_element, "V")]])
          condition <- eval(parse(text = widget_conditions[grepl("answer", widget_conditions, fixed = TRUE)]))
          condition_message <- widget_values[grepl("message", widget_conditions, fixed = TRUE)]
        }
        widget_spec <- rev(strsplit(widget_id, "_|-")[[1]]) # no1 = numLineID, no2 = selector_2, no3 = selector_1
        req(as.character(numals_env()[[widget_spec[3]]][[widget_spec[2]]][[widget_spec[1]]]) != as.character(answer))
        if (!condition) {
          showModal(modalDialog(
            title = "Error",
            condition_message,
            easyClose = TRUE
          ))
        }
        req(condition)
        numals_env({
          numval_env_temp <- numals_env()
          numval_env_temp[[widget_spec[3]]][[widget_spec[2]]][[widget_spec[1]]] <- answer
          numval_env_temp
        })
        numals_trigger(runif(1))
      }, ignoreInit = FALSE)
    })
  })
  
  # Excluding all the Ids which are not require to showcased in Share button
  observeEvent(input$bookmark, {
    box_ids <- unique(paste0("dq", foldable_levels_info$id))
    foldable_table_ids <- paste0(unique(foldable_levels_info$id), "-foldable_level_table")
    unchanged_ids <- scores_table_per_selector_2()$widget_id[
      scores_table_per_selector_2()$Value == scores_table_per_selector_2()$EditedValue
    ]
    unchanged_val_ids <- gsub("-", "-val_", unchanged_ids)
    chart_ids <- c("countries", "comparison")
    setBookmarkExclude(c(
      unchanged_ids, unchanged_val_ids,
      paste0(foldable_table_ids, "_cell_clicked"), paste0(foldable_table_ids, "_rows_all"),
      paste0(foldable_table_ids, "_rows_current"), paste0(foldable_table_ids, "_state"),
      paste0(foldable_table_ids, "_search"), paste0(box_ids, "_collapser"),
      paste0(box_ids, "_open"),
      "bookmark", "resetcode", "download", "box1_collapser", "box2_collapser", "box3_collapser",
      "box4_collapser", "box5_collapser", "box1_open", "box2_open", "box3_open", "box4_open",
      "box5_open", "Selector1_inputid-selectized", "Selector1_inputid-selectized",
      "Selector2_inputid-selectized", "Compare_Selector_inputid-selectized",
      "sidebarCollapsed", "sidebarItemExpanded", "_values_", "currentSum",
      "search2", "sidebarItemExpanded", "about", "home", 
      paste0(chart_ids, "-scores_chart_global_out"),
      paste0(chart_ids, "-scores_chart_mouseover_data"),
      paste0(chart_ids, "-scores_chart_mouseover_row"),
      paste0(chart_ids, "-scores_chart_mouseover_data_value"),
      paste0(chart_ids, "-scores_chart_mouseover_serie")
    ))
    session$doBookmark()
  })
  
  # Reset Button Click Event
  
  observeEvent(input$resetcode, {
    js$resetcode()
  })
  # Collapsable decision between both graphs and tables
  #create_dq_box_group(session, "box1", "box2")
  #create_dq_box_group(session, "box1", "box3")
  
  # Country name dropdown selector click event
  observeEvent(input$Selector2_inputid, {
    if (input$Selector2_inputid != "") {
      update_dq_box("box3", collapsed = FALSE)
      update_dq_box("box2", collapsed = FALSE)
      
      compare_selector_remaining_values <- setdiff(app_meta$Compare_Selector_Array, input$Selector2_inputid)
      
      updateSelectInput(session,
                        inputId = "Compare_Selector_inputid",
                        choices = compare_selector_remaining_values, 
                        selected = input$Compare_Selector_inputid
      )
    } else {
      update_dq_box("box1", collapsed = FALSE)
      update_dq_box("box3", collapsed = FALSE)
      update_dq_box("box2", collapsed = FALSE)
    }
  })
  
  # Calling Tables
  observeEvent(per_selector_2_ids(), {
    purrr::pwalk(per_selector_2_ids(), function(selector_1, selector_2, id, label, color) {
      callModule(
        single_table_server, id,
        foldable_level_value = id,
        scores_table = scores_table_per_selector_2,
        stop_table = stop_table
      )
    })
  })
  
  # Functionality of Download Button  with its click View.
   if (file.exists(download_file_path)) {
    file.copy(from = download_file_path, to = "www/assets")
  }
    # List files in the folder "assets"
    file_list <- list.files("www/assets")
    
    # Extract the Excel or CSV files
    excel_csv_files <- file_list[grep(".xlsx$|.csv$", file_list)]
    
    # Take the first Excel or CSV file found
    excel_csv_file <- excel_csv_files[1]
    
    output$download <- if (!is.na(excel_csv_file)) {
      downloadHandler(
      filename = function() {
        excel_csv_file
      },
      content = function(file) {
      filepath <- normalizePath(file.path("www", "assets", excel_csv_file))
       data <- readBin(filepath, what = "raw", n = file.info(filepath)$size)
        writeBin(data, file)
      })
      }else{
        downloadHandler(
        filename = function() {
          paste0("Policy_Simulator", ".csv")
        },
        content = function(file) {
          write.csv(data_levels, file, row.names = FALSE)
        }
    )
      }

  # Show boxes with comparison chart and data tables only when some country is selected
  observeEvent(input$Selector2_inputid, {
    if (isTruthy(input$Selector2_inputid)) {
      runjs("
        var hiddenBoxes = document.querySelectorAll(\'.content__row.content__row--hidden\');
        for (var box of hiddenBoxes) {
          box.classList.remove(\'content__row--hidden\');
        }
      ")
    }
  })

  modal_button <- modalButton("Go Back to Dashboard")

  # Show modal box of "About"
  observeEvent(input$about, {
    showModal(modalDialog(
      title = app_meta$About,
      HTML(as.character(app_meta$About_Link)),
      footer = modal_button,
      size = "l",
      easyClose = TRUE
    ))
  })

  # Show modal box of "Home"
  observeEvent(input$home, {
    showModal(modalDialog(
      title = app_meta$Homepage,
      HTML(as.character(app_meta$Homepage_Link)),
      footer = modal_button,
      size = "m",
      easyClose = TRUE
    ))
  })
}
