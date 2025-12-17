library(shiny)
library(httr2)
library(tidyverse)
library(DT)
library(shinyjs)

server <- function(input, output, session) {
  

# Disables and observes ---------------------------------------------------
  
  shinyjs::disable("refresh")
  shinyjs::disable("author_name")
  shinyjs::disable("date_range")
  
  observeEvent(input$all_time, {
    if (input$all_time) {
      shinyjs::disable("date_range")
    } else {
      shinyjs::enable("date_range")
    }
  })
  
  observe({
    metric_ok <- !is.null(input$metric) && input$metric != ""
    
    author_ok <- TRUE
    if (input$scope == "scope_author") {
      author_ok <- !is.null(input$author_name) && input$author_name != ""
    }
    
    refresh_ok <- metric_ok && author_ok
    
    if (refresh_ok) {
      shinyjs::enable("refresh")
    } else {
      shinyjs::disable("refresh")
    }
  })
  
  observeEvent(input$scope, {
    if (input$scope == "scope_author") {
      shinyjs::enable("author_name")
    } else {
      shinyjs::disable("author_name")
      updateTextInput(session, "author_name", value = "")
    }
  }) 
  
  username <- Sys.getenv("FSusername")
  password <- Sys.getenv("FSpassword")
  api_key <- Sys.getenv("APIkey")
  

# Functions ---------------------------------------------------------------

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
    req(input$scope)
      
    if (input$scope == "scope_author") {
      # --- Author search path ---
      req(input$author_name)
      
      # Perform author search
      # 1. Create a request object
      req <- request("https://api.figshare.com/v2/articles/search") %>%
        req_headers(
          Authorization = paste("token", api_key),
          `Content-Type` = "application/json"
        ) %>%
        req_body_json(list(
          search_for = input$author_name,
          institution = 2
        ))
      
      # 2. Perform the request
      resp <- req_perform(req)
      
      # 3. Parse the JSON
      search_json <- resp_body_json(resp, simplifyVector = TRUE)
      
      print(names(search_json))
      
      # 4. Collect item_ids
      item_ids <- search_json$id
      
      if (length(item_ids) == 0) return(tibble(
        citation = character(),
        metric_value = integer()
      ))
      
      # Fetch metrics for each item
      stats_list <- map_dfr(item_ids, function(item_id) {
        stats_url_item <- paste0("https://stats.figshare.com/lboro/top/", input$metric, "/article/", item_id)
        if (!isTRUE(input$all_time)) {
          stats_url_item <- paste0(stats_url_item,
                                   "?start_date=", as.character(input$date_range[1]),
                                   "&end_date=", as.character(input$date_range[2]))
        }
        resp <- request(stats_url_item) |>
          req_auth_basic(username, password) |>
          req_headers(`User-Agent` = "httr2 - shinyApp/1.0") |>
          req_perform()
        
        json <- resp_body_json(resp, simplifyVector = TRUE)
        tibble(
          item_id = as.integer(names(json$top)),
          metric_value = as.integer(unlist(json$top))
        )
      })
      
      # Only keep top 10
      top_df <- stats_list |> arrange(desc(metric_value)) |> slice_head(n = 10)
      
      # Fetch citations
      citation_data <- map_dfr(top_df$item_id, fetch_citation)
      
      # Join and format
      left_join(top_df, citation_data, by = "item_id") |>
        mutate(
          citation = paste0('<a href="', figshare_url, '" target="_blank">', citation, '</a>')
        ) |>
        select(citation, metric_value)
      
    } else {
    
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
    }
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
