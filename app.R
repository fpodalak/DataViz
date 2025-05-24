# CineMetrics Dashboard - Interactive Movie Analytics
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(stringr)
library(viridis)
library(networkD3)
library(shinycssloaders)


# General data preprocessing
load_general_data <- function() {
  # Load general data from the dataset
  tmdb = read.csv("tmdb5000.csv", stringsAsFactors = FALSE)
  
  # Parse genres
  tmdb$genres = sapply(tmdb$genres, function(x) {
    if (is.na(x) || x == "" || x == "[]") return("")
    tryCatch({
      parsed = fromJSON(x)
      if (is.data.frame(parsed)) return(paste(parsed$name, collapse = ", "))
      return("")
    }, error = function(e) "")
  })
  
  # Parse production_companies
  tmdb$production_companies = sapply(tmdb$production_companies, function(x) {
    if (is.na(x) || x == "" || x == "[]") return("")
    tryCatch({
      parsed = fromJSON(x)
      if (is.data.frame(parsed)) return(paste(parsed$name, collapse = ", "))
      return("")
    }, error = function(e) "")
  })
  
  # Parse cast (top 5 by order)
  tmdb$cast = sapply(tmdb$cast, function(x) {
    if (is.na(x) || x == "" || x == "[]") return("")
    tryCatch({
      parsed = fromJSON(x)
      if (is.data.frame(parsed)) {
        top_cast = parsed[parsed$order < 5, ]
        if (nrow(top_cast) > 0) {
          return(paste(top_cast$name, collapse = ", "))
        } else {
          return("")
        }
      }
      return("")
    }, error = function(e) "")
  })

  # Parse director from crew column (first director found)
  tmdb$director = sapply(tmdb$crew, function(x) {
    if (is.na(x) || x == "" || x == "[]") return("")
    tryCatch({
      parsed = fromJSON(x)
      if (is.data.frame(parsed)) {
        directors = parsed[parsed$job == "Director", ]
        if (nrow(directors) > 0) {
          return(paste(directors$name, collapse = ", "))
        } else {
          return("")
        }
      }
      return("")
    }, error = function(e) "")
  })
  
  # release_year derived from release_date of format YYYY-MM-DD
  tmdb$release_year = as.numeric(substr(tmdb$release_date, 1, 4))
  
  return(data.frame(
    id = tmdb$id,
    title = tmdb$title,
    genres = tmdb$genres,
    vote_average = tmdb$vote_average,
    release_year = tmdb$release_year,
    revenue = tmdb$revenue,
    budget = tmdb$budget,
    overview = tmdb$overview,
    production_companies = tmdb$production_companies,
    release_date = tmdb$release_date,
    runtime = tmdb$runtime,
    tagline = tmdb$tagline,
    cast = tmdb$cast,
    director = tmdb$director,
    stringsAsFactors = FALSE
  ))
}

movies_data <- load_general_data()

get_unique_actors <- function(data) {
  actors <- unlist(strsplit(data$cast, ", "))
  unique_actors <- sort(unique(actors[actors != ""]))
  return(unique_actors)
}

get_unique_directors <- function(data) {
  directors <- unlist(strsplit(data$director, ", "))
  unique_directors <- sort(unique(directors[directors != ""]))
  return(unique_directors)
}









# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "film-solid.svg", height = "30px"),
      "CineMetrics",
      style = "display: flex; align-items: center; gap: 10px;"
    ),
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Industry Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Movie Explorer", tabName = "explorer", icon = icon("search")),
      menuItem("Director & Actor Analytics", tabName = "analytics", icon = icon("users")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),

    # Global filters
    hr(),
    h4("Global Filters", style = "color: white; margin-left: 15px;"),

    sliderInput(
      "year_range", "Year Range",
      min = min(movies_data$release_year, na.rm = TRUE),
      max = 2016,
      value = c(2000, 2016),
      step = 1
    ),

    # Genre filter
    checkboxGroupInput(
      "genre_filter", "Genres",
      choices = {
        genres_vec <- unlist(strsplit(movies_data$genres, ","))
        genres_vec <- trimws(genres_vec)
        genres_unique <- sort(unique(genres_vec[genres_vec != ""]))
        c("Select All" = "ALL", genres_unique)
      },
      selected = {
        genres_vec <- unlist(strsplit(movies_data$genres, ","))
        genres_vec <- trimws(genres_vec)
        genres_unique <- sort(unique(genres_vec[genres_vec != ""]))
        genres_unique[1:3]
      }
    ),

    sliderInput(
      "rating_range", "Rating Range",
      min = 0, max = 10,
      value = c(0, 10),
      step = 0.1
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top: 3px solid #3c8dbc;
        }
        .movie-card {
          background: white;
          border-radius: 8px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # Industry Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Box Office Trends Over Time", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  withSpinner(plotlyOutput("timeline_chart", height = "400px"))
                ),
                box(
                  title = "Quick Stats", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  valueBoxOutput("total_movies", width = 12),
                  valueBoxOutput("avg_rating", width = 12),
                  valueBoxOutput("total_revenue", width = 12)
                )
              ),
              
              fluidRow(
                box(
                  title = "Genre Performance Heatmap", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(plotlyOutput("genre_heatmap", height = "350px"))
                ),
                box(
                  title = "Movies by Year and Genre", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("stacking_option", "Stacking Option:",
                              choices = c("Genre", "None"),
                              selected = "Genre"),
                  withSpinner(plotlyOutput("movies_by_year", height = "300px"))
                )
              )
      ),
      
      # Movie Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "Movie Database", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  withSpinner(DT::dataTableOutput("movies_table"))
                ),
                box(
                  title = "Movie Details", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  uiOutput("movie_detail_panel")
                )
              ),
              
              fluidRow(
                box(
                  title = "Rating Distribution", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  withSpinner(plotlyOutput("rating_distribution", height = "300px"))
                ),
                box(
                  title = "Histogram Bin Width",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  sliderInput(
                  "hist_binwidth",
                  "Bin Width",
                  min = 0.1,
                  max = 1,
                  value = 0.5,
                  step = 0.1
                  )
                )
              )
      ),
      
      # Director & Actor Analytics Tab
      tabItem(tabName = "analytics",
        fluidRow(
          column(
        width = 4,
        div(style = "height: 85vh; display: flex; flex-direction: column;",
          box(
          title = "Select Actor or Director", 
          status = "info", 
          solidHeader = TRUE,
          width = 12,
          height = "80vh",
          tabsetPanel(
            tabPanel("Actors", DT::dataTableOutput("actor_table")),
            tabPanel("Directors", DT::dataTableOutput("director_table"))
          )
        )
        )
          ),
          column(
        width = 8,
        div(style = "height: 85vh; display: flex; flex-direction: column;",
            fluidRow(
            box(
        title = "Collaboration Network", 
        status = "primary", 
        solidHeader = TRUE,
        width = 12,
        height = "40vh",
        div(
        style = "display: flex; align-items: center; gap: 20px; margin-bottom: 10px;",
        strong("Current Person: "),
        textOutput("current_person", inline = TRUE),
        div(style = "flex: 1;"),
        sliderInput("network_depth", "Depth of collaboration network",
          min = 1, max = 2, value = 1, step = 1, width = "300px"
        )
        ),
        withSpinner(networkD3::forceNetworkOutput("collaboration_network", height = "25vh"))
      )
          ),
          fluidRow(
            box(
          title = "Career Trajectory", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          height = "40vh",
          selectInput("person_select", "Select Person:",
            choices = c("Person 1", "Person 2", "Person 3"),
            selected = "Person 1"),
          withSpinner(plotlyOutput("career_trajectory", height = "25vh"))
            )
          )
        )
          )
        )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About CineMetrics Dashboard", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    h3("Welcome to CineMetrics"),
                    p("An interactive dashboard for exploring movie industry trends and analytics."),
                    
                    h4("Features:"),
                    tags$ul(
                      tags$li("Industry Overview: Analyze box office trends, genre performance, and market patterns"),
                      tags$li("Movie Explorer: Search and filter through thousands of movies with detailed information"),
                      tags$li("Director & Actor Analytics: Explore collaboration networks and career trajectories")
                    ),
                    
                    h4("Data Source:"),
                    p("This dashboard uses the TMDB 5000 Movie Dataset, containing comprehensive information about movies, cast, crew, and financial performance."),
                    p("Keep in mind that movies available in this dashboard are only up to 2017."),

                    h4("How to Use:"),
                    tags$ol(
                      tags$li("Use the global filters in the sidebar to narrow down your analysis"),
                      tags$li("Navigate between tabs to explore different aspects of the data"),
                      tags$li("Click on interactive elements to trigger cross-component updates"),
                      tags$li("Hover over charts for detailed information")
                    ),
                    
                    hr(),
                    p("Built with R Shiny, ggplot2, plotly, and networkD3"),
                    p("Developed by Marek O. and Filip P."),
                  )
                )
              )
      )
    )
  )
)










# Server
server <- function(input, output, session) {

  # Genre "Select All" logic
  observeEvent(input$genre_filter, {
    genres_vec <- unlist(strsplit(movies_data$genres, ","))
    genres_vec <- trimws(genres_vec)
    genres_unique <- sort(unique(genres_vec[genres_vec != ""]))
    if ("ALL" %in% input$genre_filter) {
      updateCheckboxGroupInput(session, "genre_filter",
        selected = genres_unique
      )
    } else if (length(input$genre_filter) == 0) {
      updateCheckboxGroupInput(session, "genre_filter",
        selected = character(0)
      )
    }
  }, ignoreInit = TRUE)

  # Reactive data filtering
  filtered_data <- reactive({
    movies_data %>%
      filter(
        release_year >= input$year_range[1],
        release_year <= input$year_range[2],
        vote_average >= input$rating_range[1],
        vote_average <= input$rating_range[2],
        sapply(strsplit(genres, ","), function(gs) {
          any(trimws(gs) %in% input$genre_filter)
        })
      )
  })

  # Total movies
  output$total_movies <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Movies",
      icon = icon("film"),
      color = "blue"
    )
  })

  # Average rating
  output$avg_rating <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$vote_average, na.rm = TRUE), 1),
      subtitle = "Average Rating",
      icon = icon("star"),
      color = "yellow"
    )
  })

  # Total revenue  
  output$total_revenue <- renderValueBox({
    valueBox(
      value = paste0("$", round(sum(filtered_data()$revenue, na.rm = TRUE) / 1e9, 1), "B"),
      subtitle = "Total Revenue",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # Timeline chart
  output$timeline_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(release_year) %>%
      summarise(
        total_revenue = sum(revenue, na.rm = TRUE),
        avg_rating = mean(vote_average, na.rm = TRUE),
        movie_count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(data, aes(x = release_year, y = total_revenue / 1e9)) +
      geom_line(color = "#3498db", size = 1.2) +
      geom_point(aes(text = paste("Year:", release_year, 
                                  "<br>Revenue: $", round(total_revenue/1e9, 2), "B",
                                  "<br>Movies:", movie_count,
                                  "<br>Avg Rating:", round(avg_rating, 2))),
                 color = "#e74c3c", size = 3) +
      labs(title = "", x = "Year", y = "Total Revenue (Billions $)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Genre heatmap
  output$genre_heatmap <- renderPlotly({
    data <- filtered_data() %>%
      mutate(genre = strsplit(genres, ",")) %>%
      tidyr::unnest(genre) %>%
      mutate(genre = trimws(genre)) %>%
      filter(genre != "") %>%
      group_by(genre) %>%
      summarise(
        avg_revenue = mean(revenue, na.rm = TRUE),
        avg_rating = mean(vote_average, na.rm = TRUE),
        .groups = 'drop'
      )

    p <- ggplot(data, aes(x = avg_rating, y = avg_revenue / 1e6, 
                          text = paste("Genre:", genre,
                                      "<br>Avg Rating:", round(avg_rating, 2),
                                      "<br>Avg Revenue:", round(avg_revenue/1e6, 1), "M"))) +
      geom_point(aes(color = genre), size = 4) +
      labs(x = "Average Rating", y = "Average Revenue (Millions $)") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Movies by year
  output$movies_by_year <- renderPlotly({
    data <- filtered_data() %>%
      mutate(genre = strsplit(genres, ",")) %>%
      tidyr::unnest(genre) %>%
      mutate(genre = trimws(genre)) %>%
      filter(genre != "")

    if (input$stacking_option == "Genre") {
      p <- ggplot(data, aes(x = release_year, fill = genre)) +
        geom_bar(position = "stack") +
        labs(x = "Year", y = "Number of Movies") +
        theme_minimal() +
        scale_fill_viridis_d(name = "Genre")
    } else {
      data_summary <- data %>%
        group_by(release_year) %>%
        summarise(count = n(), .groups = 'drop')

      p <- ggplot(data_summary, aes(x = release_year, y = count)) +
        geom_col(fill = "#3498db") +
        labs(x = "Year", y = "Number of Movies") +
        theme_minimal()
    }

    ggplotly(p) %>%
      config(displayModeBar = FALSE)
  })
  
  # Movies table
  output$movies_table <- DT::renderDataTable({
    filtered_data() %>%
      select(title, release_year, genres, vote_average, revenue, budget) %>%
      mutate(
        revenue = paste0("$", round(revenue / 1e6, 1), "M"),
        budget = paste0("$", round(budget / 1e6, 1), "M")
      )
  }, 
  selection = 'single',
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'ftip'
  ))

  # Movie detail panel (default)
  output$movie_detail_panel <- renderUI({
    div(class = "movie-card",
        h4("Select a movie from the table"),
        p("Click on any row in the movie table to see detailed information here.")
    )
  })
  
  # Movie detail panel (reactive to table selection)
  observeEvent(input$movies_table_rows_selected, {
    if (length(input$movies_table_rows_selected) > 0) {
      selected_movie <- filtered_data()[input$movies_table_rows_selected, ]
      
      output$movie_detail_panel <- renderUI({
        div(class = "movie-card",
            h2(selected_movie$title),
            if (!is.na(selected_movie$tagline) && selected_movie$tagline != "") {
              tags$h4(tags$em(paste0('"', selected_movie$tagline, '"')))
            },
            p(strong("Release Date:"), selected_movie$release_date),
            p(strong("Genres:"), selected_movie$genres),
            p(strong("Rating:"), selected_movie$vote_average, "/10"),
            p(strong("Runtime:"), selected_movie$runtime, "minutes"),
            p(strong("Director:"), selected_movie$director),
            p(strong("Cast:"), selected_movie$cast),
            p(strong("Production Companies:"), selected_movie$production_companies),
            p(strong("Budget:"), paste0("$", formatC(selected_movie$budget, format = "f", digits = 0, big.mark = ","))),
            p(strong("Revenue:"), paste0("$", formatC(selected_movie$revenue, format = "f", digits = 0, big.mark = ","))),
            p(strong("Profit:"), paste0("$", formatC(selected_movie$revenue - selected_movie$budget, format = "f", digits = 0, big.mark = ","))),
            tags$hr(),
            p(strong("Overview:"), selected_movie$overview)
        )
      })
    }
  })
  
  # Rating distribution
  output$rating_distribution <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = vote_average)) +
      geom_histogram(binwidth = input$hist_binwidth, fill = "#3498db", alpha = 0.7, color = "white") +
      labs(x = "Rating", y = "Number of Movies") +
      theme_minimal()
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE)
  })
  
  
output$actor_table <- DT::renderDataTable({
  data.frame(
    Name = get_unique_actors(filtered_data()),
    stringsAsFactors = FALSE
  )
}, 
selection = "single",
options = list(pageLength = 10, dom = 'ftip', searchHighlight = TRUE))

output$director_table <- DT::renderDataTable({
  data.frame(
    Name = get_unique_directors(filtered_data()),
    stringsAsFactors = FALSE
  )
}, 
selection = "single",
options = list(pageLength = 10, dom = 'ftip', searchHighlight = TRUE))

# Track selected person from actor or director table
selected_person <- reactiveVal(NULL)
selected_role <- reactiveVal(NULL)

# When user clicks on actor table
observeEvent(input$actor_table_rows_selected, {
  actors <- get_unique_actors(filtered_data())
  idx <- input$actor_table_rows_selected
  if (length(idx) > 0 && all(idx <= length(actors))) {
    selected_person(actors[idx])
    selected_role("Actor")
    updateSelectInput(session, "person_select", choices = actors, selected = actors[idx])
    DT::selectRows(dataTableProxy("director_table"), NULL)
  }
})

# When user clicks on director table
observeEvent(input$director_table_rows_selected, {
  directors <- get_unique_directors(filtered_data())
  idx <- input$director_table_rows_selected
  if (length(idx) > 0 && all(idx <= length(directors))) {
    selected_person(directors[idx])
    selected_role("Director")
    updateSelectInput(session, "person_select", choices = directors, selected = directors[idx])
    DT::selectRows(dataTableProxy("actor_table"), NULL)
  }
})

# Update person_select choices if filters change and nothing is selected
observe({
  if (is.null(selected_person())) {
    actors <- get_unique_actors(filtered_data())
    updateSelectInput(session, "person_select", choices = actors, selected = actors[1])
  }
})

# When person_select changes, update selected_person
observeEvent(input$person_select, {
  selected_person(input$person_select)
})

output$current_person <- renderText({
  req(selected_person())
  selected_person()
})



# Load collaborator network data
collaborator_network <- reactiveVal(NULL)
observe({
  if (is.null(collaborator_network())) {
    json_data <- jsonlite::fromJSON("actor_collaborators.json", simplifyVector = FALSE)
    collaborator_network(json_data)
  }
})


get_collaboration_subgraph <- function(person, depth, network) {
  if (is.null(network) || is.null(person) || !(person %in% names(network))) return(list(nodes = list(), links = list()))
  visited <- setNames(rep(FALSE, length(network)), names(network))
  nodes <- list()
  links <- list()
  queue <- list(list(name = person, level = 0))
  visited[person] <- TRUE

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    name <- current$name
    level <- current$level

    # Add node
    nodes[[name]] <- list(
      name = name,
      role = network[[name]]$roles
    )

    if (level < depth) {
      for (collab in names(network[[name]]$collaborators)) {
        # Add link
        links[[length(links) + 1]] <- list(
          source = name,
          target = collab,
          target_role = network[[collab]]$roles %||% network[[name]]$collaborators[[collab]]
        )
        # Add node if not visited
        if (!visited[collab] && !is.null(network[[collab]])) {
          visited[collab] <- TRUE
          queue[[length(queue) + 1]] <- list(name = collab, level = level + 1)
        }
      }
    }
  }

  # Convert nodes/links to data.frames for networkD3
  node_names <- names(nodes)
  nodes_df <- data.frame(
    name = node_names,
    role = sapply(nodes, function(x) x$role),
    stringsAsFactors = FALSE
  )
  nodes_df$id <- seq_len(nrow(nodes_df)) - 1

  links_df <- do.call(rbind, lapply(links, function(l) {
    data.frame(
      source = which(node_names == l$source) - 1,
      target = which(node_names == l$target) - 1,
      target_role = l$target_role,
      stringsAsFactors = FALSE
    )
  }))
  list(nodes = nodes_df, links = links_df)
}

# Render collaboration network
output$collaboration_network <- networkD3::renderForceNetwork({
  req(selected_person(), collaborator_network())
  subgraph <- get_collaboration_subgraph(selected_person(), input$network_depth, collaborator_network())
  if (nrow(subgraph$nodes) == 0 || nrow(subgraph$links) == 0) return(NULL)

  role_colors <- c(
    "actor" = "#3498db",
    "director" = "#e67e22"
  )
  node_color <- role_colors[match(tolower(subgraph$nodes$role), names(role_colors))]
  node_color[is.na(node_color)] <- "#888888"
  node_color[subgraph$nodes$name == selected_person()] <- "#d62728"
  subgraph$nodes$color <- node_color

  networkD3::forceNetwork(
    Links = subgraph$links,
    Nodes = subgraph$nodes,
    Source = "source",
    Target = "target",
    NodeID = "name",
    Group = "role",
    opacity = 0.9,
    fontSize = 16,
    zoom = TRUE,
    linkDistance = 100,
    charge = -300,
    colourScale = networkD3::JS(sprintf(
      "d3.scaleOrdinal().domain(['actor','director']).range(['%s','%s'])",
      role_colors["actor"], role_colors["director"]
    )),
    linkColour = "#aaa"
  )
})



# Career trajectory plot
output$career_trajectory <- renderPlotly({
  req(selected_person())
  data <- filtered_data() %>%
    filter(grepl(selected_person(), cast, ignore.case = TRUE) | grepl(selected_person(), director, ignore.case = TRUE)) %>%
    select(title, release_year, revenue, vote_average) %>%
    filter(!is.na(release_year), !is.na(revenue), !is.na(vote_average))

  p <- ggplot(data, aes(x = release_year, y = revenue / 1e6, color = vote_average, text = paste0(
    "Title: ", title,
    "<br>Year: ", release_year,
    "<br>Revenue: $", round(revenue / 1e6, 1), "M",
    "<br>Rating: ", vote_average
  ))) +
    geom_point(size = 4, alpha = 0.8) +
    scale_color_viridis_c(name = "Rating", option = "C") +
    labs(x = "Year", y = "Revenue (Millions $)", title = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggplotly(p, tooltip = "text") %>%
    config(displayModeBar = FALSE)
})

}

shinyApp(ui = ui, server = server)