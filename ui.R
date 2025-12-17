library(shiny)
library(shinyjs)
library(DT)
library(shinycssloaders)

ui <- tags$html(
  lang = "en",
  fluidPage(
  style = "padding: 0px; margin: 0px;",
  tags$head(
    tags$title("Loughborough University Top 10 items"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # ---- Black banner ----
  tags$div(
    class = "black-banner",
    tags$div(
      class = "banner-content",
      tags$a(
        href = "https://www.lboro.ac.uk",
        target = "_blank",
        tags$img(src = "logo.png", class = "uni-logo", alt = "University Logo")
      ),
      tags$a(
        href = "https://www.lboro.ac.uk/services/library/",
        target = "_blank",
        class = "return-link",
        "University Library"
      )
    )
  ),
  
  # ---- Blue banner ----
  tags$div(
    class = "blue-banner",
    tags$div(
      class = "banner-content",
      tags$span("Open Research Services"),
      tags$a(
        href = "https://repository.lboro.ac.uk/",
        class = "return-link",
        "< Return to Research Repository"
      )
    )
  ),
  
  shinyjs::useShinyjs(),
  
  tags$h2(
    "Loughborough University Research Repository Top 10 items",
    style = "margin: 20px;"
  ),
  
  tags$hr(style = "border-top: 1px solid #d3d3d3;"),
  
  # ---- Sidebar layout ----
  sidebarLayout(
    sidebarPanel(
      class = "sidebarPanel",
      width = 3,
      
      
      
      tags$label("Metric"),
      radioButtons(
        "metric", NULL,
        choices = c("Downloads" = "downloads", "Views" = "views")
      ),
      
      tags$hr(),
      
      selectInput(
        "scope",
        "Scope",
        NULL,
        choices = c("The whole repository" = "scope_all", "Author" = "scope_author")
      ),
      
      textInput(
        "author_name",
        "Author name and surname",
      ),
      
      dateRangeInput(
        "date_range",
        "Date range",
        start = Sys.Date() - 30,
        end   = Sys.Date(),
        max   = Sys.Date(),
        separator = " - "
      ),
      
      checkboxInput("all_time", "All time", value = FALSE),
      
      tags$hr(),
      
      actionButton("refresh", "Go")
    ),
    
    mainPanel(
      width = 9,
      
      withSpinner(
        DTOutput("top10_table"),
        3,
        color.background = "#8D9C27"
      )
    )
  ),
  
  tags$hr(style = "border-top: 1px solid #d3d3d3;"),
  
  # ---- Footer ----
  tags$div(
    class = "footer",
    fluidRow(
      column(
        12,
        tags$a(
          href = "https://doi.org/10.17028/rd.lboro.28525481",
          "Accessibility Statement"
        )
      )
    )
  )
)
)
