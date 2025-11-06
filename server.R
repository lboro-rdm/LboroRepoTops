library(shiny)
library(httr2)
library(tidyverse)
library(DT)
library(shinyjs)

server <- function(input, output, session) {
  
  username <- Sys.getenv("FSusername")
  password <- Sys.getenv("FSpassword")
  
  # ---- Function to fetch citation and URL for a single item_id ----
  fetch_citation <- function(item_id) {
    url <- paste0("https://api.figshare.com/v2/articles/", item_id)
    resp <- request(url) |>
      req_auth_basic(username, password) |>
      req_headers(`User-Agent` = "httr2 - shinyApp/1.0") |>
      req_perform()
    
    info <- resp_body_json(resp, simplifyVector = TRUE)
    
    tibble(
      item_id = item_id,
      figshare_url = info$figshare_url,
      citation = info$citation %||% NA_character_
    )
  }
  
  shinyjs::disable("date_range")
  
  observeEvent(input$all_time, {
    if (input$all_time) {
      shinyjs::disable("date_range")
    } else {
      shinyjs::enable("date_range")
    }
  })
  
  # ---- Reactive URL based on metric ----
  stats_url <- reactive({
    req(input$metric)
    
    url <- paste0("https://stats.figshare.com/lboro/top/", input$metric, "/article")
    
    query <- list(n = 10)
    
    if (!isTRUE(input$all_time)) {
      req(input$date_range)
      query$start_date <- as.character(input$date_range[1])
      query$end_date   <- as.character(input$date_range[2])
    }
    
    list(
      url = url,
      query = query
    )
  })
  
  # ---- Event reactive: top 10 data fetched only when refresh button clicked ----
  top10_data <- eventReactive(input$refresh, {
    req(input$metric)
    
    # Fetch top stats
    stats_req <- request(stats_url()$url) |>
      req_url_query(!!!stats_url()$query) |>
      req_auth_basic(username, password) |>
      req_headers(`User-Agent` = "httr2 - shinyApp/1.0") |>
      req_perform()
    
    
    
    stats_json <- resp_body_json(stats_req, simplifyVector = TRUE)
    
    # Convert to tibble
    top_df <- tibble(
      item_id = as.integer(names(stats_json$top)),
      metric_value = as.integer(unlist(stats_json$top))
    )
    
    # Fetch citations for all top items
    citation_data <- map_dfr(top_df$item_id, fetch_citation)
    
    # Join metric + citation, make citation clickable
    top_df <- left_join(top_df, citation_data, by = "item_id") %>%
      mutate(
        citation = paste0('<a href="', figshare_url, '" target="_blank">', citation, '</a>')
      ) %>%
      select(citation, metric_value)
    
    top_df
  })
  
  # ---- Render DataTable ----
  output$top10_table <- renderDT({
    datatable(
      top10_data(),
      options = list(
        paging = FALSE,       # show all rows
        searching = FALSE,    # optional: remove search box
        info = FALSE          # remove "Showing X of Y" info
      ),
      escape = FALSE          # allow HTML links
    )
  })
  
}
