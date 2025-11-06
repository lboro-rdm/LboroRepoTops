library(shiny)

ui <- fluidPage(
  titlePanel("Figshare Repository Insights"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "metric",
        "Metric:",
        choices = c("Downloads" = "downloads", "Views" = "views"),
        selected = "downloads"
      ),
      selectInput(
        "filter_type",
        "Filter by:",
        choices = c("All", "Author", "Collection", "Project"),
        selected = "All"
      ),
      # This text input appears when Author / Collection / Project is selected
      uiOutput("filter_value_ui"),
      
      dateRangeInput(
        "date_range",
        "Date range:",
        start = Sys.Date() - 30,
        end = Sys.Date(),
        max = Sys.Date()
      ),
      actionButton("refresh", "Refresh Data")
    ),
    
    mainPanel(
      DTOutput("top10_table")
    )
  )
)
