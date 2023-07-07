sidebar_width <- 250

# App Logo
app_logo <- tags$a(
  class = "logo__link",
  href = app_meta$Logo_Link, target = "_blank", rel = "noopener",
  tags$img(
    class = "logo__img",
    src = app_meta$Logo_Updated,
    alt = "Organisation for Economic Co-operation and Development"
  )
)

ui <- function(request) {
  dashboardPage(
    title = app_meta$Title,

    # App Header
    dashboardHeader(
      title = app_logo,
      titleWidth = sidebar_width,
      tags$li(
        class = "dropdown nav__title",
        tags$h1(class = "nav__heading", app_meta$Title)
      ),
      tags$li(
        class = "dropdown nav__link",
        actionButton("about", app_meta$About)
      ),
      tags$li(
        class = "dropdown nav__link",
        actionButton("home", app_meta$Homepage)
      )
    ),

    # App Sidebar
    dashboardSidebar(
      actionButton(
        "resetcode",
        label = "RESET",
        class = "sidebar__button",
        icon = icon("undo-alt")
      ),
      selectInput(
        inputId = "Selector1_inputid",
        label = app_meta$Selector1_Name,
        choices = app_meta$Selector1_Array
      ),
      selectInput(
        inputId = "Selector2_inputid",
        label = app_meta$Selector2_Name,
        choices = c("", app_meta$Selector2_Array),
        selected = NULL
      ),
      selectInput(
        inputId = "Compare_Selector_inputid",
        label = paste0("COMPARE TO ",
        app_meta$Compare_Selector_Name),
        multiple = TRUE,
        choices = app_meta$Compare_Selector_Array
      ),
      bookmarkButton(
        id = "bookmark",
        label = "SHARE",
        class = "sidebar__button",
        icon = icon("share-alt")
      ),
      downloadButton(
        "download",
        label = "DOWNLOAD",
        class = "sidebar__button"
      ),
      div(
        class = "sidebar__footer",
        tags$a(
          class = "sidebar__copy",
          href = app_meta$Footer_Link,
          target = "_blank",
          rel = "noopener",
          app_meta$Footer
        )
      ),
      width = sidebar_width
    ), 

    # App Body
    dashboardBody(
      fluidRow(
        class = "content__row",
        dq_box(
          id = "box1",
          fill = TRUE,
          title = textOutput("chart_title"),
          width = 12,
          open_callback = TRUE,
          bg_color = "#fff",
          color = "#fff",
          chart_ui("countries")
        )
      ), 
      fluidRow(
        class = "content__row content__row--hidden",
        dq_box(
          id = "box2",
          fill = TRUE,
          title = "Countries Comparison ",
          width = 12,
          open_callback = TRUE,
          bg_color = "#fff",
          color = "#fff",
          collapsed = TRUE,
          chart_ui("comparison")
        )
      ), 
      fluidRow(
        class = "content__row content__row--hidden",
        dq_box(
          id = "box3",
          fill = TRUE,
          title = "Table Calculations",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          collapsed = TRUE,
          open_callback = TRUE,
          bg_color = "#fff",
          color = "#fff",
          fluidRow(
            textInput("search2", label = " ", placeholder = "Search"),
            align = "right"
          ),
          uiOutput("foldable_tables")
        )
      ),

      # Head
      tags$head(
        tags$style(".shiny-notification {position: fixed; top: 60% ;left: 50%}"),
        tags$link(rel = "stylesheet", type = "text/css", href = "css/sass.min.css"),
        tags$script(src = "js/main.js")
      ),
      shinyjs::useShinyjs(),
      extendShinyjs(text = jsResetCode, functions = "resetcode")
    )
  )
}
