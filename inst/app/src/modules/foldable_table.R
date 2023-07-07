single_table_output <- function(id) {
  ns <- NS(id)
  div(
    class = "table",
    div(
      class = "table__left",
      DT::dataTableOutput(ns("foldable_level_table"))
    )
  )
}

single_table_server <- function(input, output, session, foldable_level_value, scores_table, stop_table) {
  ns <- session$ns
  foldable_id <- gsub("-", "", ns(""))

  per_foldable_data <- eventReactive(c(session$userData$selector_2_value(), scores_table()), {
    req(session$userData$selector_2_value())
    scores_table() %>%
      dplyr::filter(!!sym(levels_meta$foldable) == foldable_level_value)
  })

  second_selector_dt <- eventReactive(per_foldable_data(), {
    req(session$userData$selector_2_value())
    per_foldable_data() %>%
      dplyr::filter(!!sym(levels_meta$selector_2) == session$userData$selector_2_value()) %>%
      dplyr::select(
        Question, Answer = ui_widget, Value, Score, Comp_Id = widget_id, Source = Source, 
        Comment = Comment, `Edited Value` = EditedValue, `Edited Score` = EditedScore
      )
  })

  output$foldable_level_table <- DT::renderDataTable({
    req(!stop_table())
    destroy_table_inputs(ns("foldable_level_table"))

    data <- second_selector_dt()
    data$measure <- data$Question
    data$Question <- sapply(
      seq_len(length(data$Question)), function(index) {
        paste(
          "<span>", data[index, ]$Question, "</span>",
          comments_button(
            ns("foldable_level_table"),
            index,
            data[index, ]$Question,
            data[index, ]$Source,
            data[index, ]$Comment
          )
        )
      }
    )
    
    data <- data[, !names(data) %in% c("Source", "Comment")]
    data$changed <- ifelse(data$Value == data$`Edited Value`, "unchanged", "changed")
    row_colors <- c("#edf0f3", "#91c100")
    DT::datatable(
      data,
      selection='none',
      escape = FALSE,
      class = 'cell-border stripe',
      options = list(
        columnDefs = list(
          list(width = '800px', targets = c(1), visible = TRUE, searchable = FALSE),
          list(width = '200px', visible=TRUE, targets=c(2), searchable = FALSE),
          list(className = 'dt-center', targets = 3:7, searchable = FALSE),
          list(visible=FALSE, targets=c(0, 5, 8, 9))
        ),
        ordering = F,
        search = if(is.null(isolate(input$foldable_level_table_search))) {
          list(regex = FALSE)
        } else {
          list(regex = FALSE, search = isolate(input$foldable_level_table_search))
        },
        pageLength = 10000,
        lengthMenu = c(5, 10, 20, 100, 1000, 10000),
        lengthChange = FALSE,
        info=FALSE,
        dom = 'ft',
        drawCallback = JS('function() {Shiny.bindAll(this.api().table().node());} ')
      )
    ) %>%
      formatStyle('changed', target = 'row', 
                  backgroundColor = styleEqual(c("unchanged", "changed"), row_colors))
  }, server = FALSE)

  outputOptions(output, "foldable_level_table", suspendWhenHidden = FALSE)

  foldable_level_table_DT_proxy <- dataTableProxy('foldable_level_table')

  observeEvent(session$userData$search2(), {
    search_value <- session$userData$search2()
    if (search_value != "" & !is.na(search_value) & !is.null(search_value)) {
      if (any(grepl(toupper(search_value), toupper(second_selector_dt()$Question), fixed = TRUE))) {
        update_dq_box(paste0("dq", foldable_id), collapsed = FALSE)
      } else {
        update_dq_box(paste0("dq", foldable_id), collapsed = TRUE)
      }
    }
    updateSearch(
      foldable_level_table_DT_proxy,
      keywords = list(global = search_value, columns = NULL)
    )
  }, ignoreInit = TRUE)

}

comments_button <- function(id, index, question, src, comment) {
  if (is.na(src) && is.na(comment)) {
    return("")
  }
  
  src_fragment <- ifelse(
    is.na(src),
    "",
    HTML(glue::glue('<div class="section source"><h3 class="title">Source</h3> {src} </div>'))
  )
  
  comment_fragment <- ifelse(
    is.na(comment),
    "",
    HTML(glue::glue('<div class="section comment"><h3 class="title">Comments</h3> {comment} </div>'))
  )
  
  HTML(glue::glue('
    <i id="{id}_{index}_button" data-target="{id}_{index}_modal" class="modal-link fas fa-comment-dots"></i>
    <div id="{id}_{index}_modal" class="modal">
      <div class="modal-content">
        <span class="close" data-target="{id}_{index}_modal">&times;</span>
        <div class="measure"> <h3 class="title">Measure</h3> {question} </div>
        {src_fragment}
        {comment_fragment}
      </div>
    </div>
    <script>
      generate_modal_bindings("{id}_{index}_button");
    </script>')
  )
}

destroy_table_inputs <- function(id) {
  # in case of dot in ID, this needs to be escaped with double backslash
  runjs(paste0("Shiny.unbindAll($('#", gsub("\\.", "\\\\\\\\.", id), "'))"))
}
