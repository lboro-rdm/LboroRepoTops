library(shiny)
library(shinyjs)
library(DT)

ui <- tags$html(
  lang = "en",
  style = "padding: 0px; margin: 0px;",
  tags$head(
    tags$title("Loughborough University Top 10 items"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Black banner
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
  
  # Blue banner
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
  
  tags$h2("Loughborough University Research Repository Top 10 items", style = "margin-left: 20px;"),
  
  tags$hr(style = "border-top: 1px solid #d3d3d3;"),
  
  # Inputs row
  # ---- Inputs row as flex ----
  tags$div(
    style = "display: flex; align-items: center; gap: 30px; padding:20px 0;",
    
    # Metric radio buttons
    tags$div(
      style = "flex: 0 0 150px; margin-left: 20px;",
      tags$label("Metric:"),
      tags$div(style = "margin-top: 10px;",  # adds vertical space
               radioButtons(
                 "metric", NULL,
                 choices = c("Downloads" = "downloads", "Views" = "views")
               )
      )
    ),
    
    
    # Date range
    tags$div(style = "flex: 1 1 auto;", 
             dateRangeInput(
               "date_range",
               "Date range:",
               start = Sys.Date() - 30,
               end = Sys.Date(),
               max = Sys.Date()
             )
    ),
    # OR label
    tags$div(style = "flex: 0 0 auto; margin-left: 0px;",  # horizontal gap
             p(strong("OR"))
    ),
    # All time checkbox
    tags$div(style = "flex: 0 0 auto; margin-left: 0px;", 
             checkboxInput("all_time", "All time", value = FALSE)
    ),
    
    # Refresh button
    tags$div(style = "flex: 0 0 auto; margin-right: 20px; padding-top: 15px;",
             actionButton("refresh", "Refresh Data")
    )
  ),
  
  tags$hr(style = "border-top: 1px solid #d3d3d3;"),
  
  # Table row
  fluidRow(
    column(12,
           tags$div(
             style = "margin-left:20px; margin-right:20px;",
             DTOutput("top10_table")
           )
    )
  ),
  
  # Footer
  tags$div(
    class = "footer", 
    fluidRow(
      column(12, 
             tags$a(
               href = 'https://doi.org/10.17028/rd.lboro.28525481', 
               "Accessibility Statement"
             )
      )
    )
  )
)
