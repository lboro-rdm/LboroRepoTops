library(shiny)
library(httr2)
library(tidyverse)
library(DT)

server <- function(input, output, session) {
  
  stats_url <- "https://stats.figshare.com/lboro/top/downloads/article"
  
  username <- Sys.getenv("FSusername")
  password <- Sys.getenv("FSpassword")
  
  # Fetch stats
  stats_req <- request(stats_url) |>
    req_auth_basic(username, password) |>
    req_headers(`User-Agent` = "httr2 - shinyApp/1.0") |>
    req_perform()
  
  stats_json <- resp_body_json(stats_req, simplifyVector = TRUE)
  
  # stats_json$top is a named list: names = item_id, values = counts
  top_df <- tibble(
    item_id = as.integer(names(stats_json$top)),
    downloads = as.integer(unlist(stats_json$top))
  )
  
  # Function to fetch citation
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
  
  # Fetch citations for all top items
  citation_data <- map_dfr(top_df$item_id, fetch_citation)
  
  # Join downloads + citation + URL
  top10_table <- left_join(top_df, citation_data, by = "item_id") %>%
    mutate(
      # Make citation a clickable link
      citation = paste0('<a href="', figshare_url, '" target="_blank">', citation, '</a>')
    ) %>%
    select(citation, downloads)
  
  # Render DataTable
  output$top10_table <- renderDT({
    datatable(
      top10_table,
      options = list(pageLength = 10),
      escape = FALSE  # allow HTML in the table
    )
  })
}