#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(spotifyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinythemes)

tabs_ui <- tabsetPanel(
    tags$style(HTML("
    body {
      background-color: #100000; /* Dark background */
      color: #FFFFFF; /* White text */
    }
    .nav-tabs > li > a, .nav-tabs > li > a:focus, .nav-tabs > li > a:hover {
      color: #FFFFFF; /* White text for tab titles */
    }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
      color: #FFFFFF; /* White text for active tab title */
      background-color: #444444; /* Optional: dark background for active tab */
    }
  ")),
    tabPanel("Metrics Exploration", 
             textOutput("metricDescription"),
             plotOutput("metricPlot"),
             tableOutput("filteredSongs")),
    tabPanel("Summary", textOutput("summary")),
    tabPanel("Genres", plotOutput("genrePlot")),
    tabPanel("Top Tracks", tableOutput("topTracks")),
    tabPanel("Listening Trends",
             plotOutput("listeningTrendsPlot"),
             p("This chart shows how your listening habits have changed over time."))
)


main_ui <- navbarPage(
    title = tags$img(src = "spotify_logo.png", height = "30px"),  # Add Spotify logo
    theme = shinythemes::shinytheme("cosmo"),  # Apply a dark theme
    tags$style(HTML("
    body {
      background-color: #100000; /* Dark gray background */
      color: #FFFFFF; /* White text */
    }
    .sidebar {
      background-color: #100000; /* Dark blue-gray background */
      color: #ffffff; /* White text */
    }
    /* Spotify Green checkbox background when checked */
    .form-check-input:checked {
      background-color: #1DB954 !important; /* Spotify Green */
      border-color: #1DB954 !important;
    }
    /* Focus state when clicking on the checkbox */
    .form-check-input:focus {
      border-color: #1DB954 !important; /* Focus border color */
    }
    /* General checkbox styles */
    .form-check-input {
      color: #1DB954 !important; /* Checkbox border */
      background-color: #1DB954; /* Unchecked background */
    }
    /* Label for checkboxes - set color to white */
    .form-check-label {
      color: #FFFFFF !important;
    }
    /* Custom checkbox styles for when the user hovers over the checkbox */
    .form-check-input:hover {
      background-color: #1DB954 !important; /* Hover state */
      border-color: #1DB954 !important;
    }
")),
    tabPanel("Dashboard",
             icon = icon("fa-solid fa-user"),
             sidebarLayout(
                 sidebarPanel(
                     class = "sidebar",
                     textInput("token", "Enter your Spotify Token:", ""),
                     actionButton("analyze", "Lock In"),
                     hr(),
                     h3("Analysis Options"),
                     checkboxInput("show_genres", "Show Genre Analysis", TRUE),
                     checkboxInput("show_tracks", "Show Most Listened Tracks", TRUE),
                     checkboxInput("show_time", "Show Listening Time Trends", TRUE),
                     selectInput("metric", "Choose a Metric:", 
                                 choices = c("Speechiness", "Danceability", "Energy", "Valence")),
                     sliderInput("metricRange", "Select Metric Range:", 
                                 min = 0, max = 1, value = c(0.4, 0.6)),
                     actionButton("filter", "Filter Songs")
                 ),
                 mainPanel(tabs_ui)  # Use `tabs_ui` here
             )
    ),
    tabPanel("Top Pick 2023",
             icon = icon("arrow-down-wide-short"),
             
             # Put the 2023 top streamed songs analysis here.
             
             ),
    tabPanel("Listening Trends", 
             icon = icon("arrow-trend-up"),
             plotOutput("listeningTrendsPlot")),
    tabPanel("Playlist Generate",
             icon = icon("list"))
)


ui <- fluidPage(
    main_ui
)

server <- function(input, output, session) {
    user_data <- reactiveVal()
    observeEvent(input$analyze, {
        req(input$token)
        
        access_token <- input$token
        
        # Fetch top tracks
        tracks <- get_my_top(type = "tracks", time_range = "long_term", limit = 50, authorization = access_token)
        artists <- get_my_top(type = "artists", time_range = "long_term", limit = 50, authorization = access_token)
        
        # Process data
        genres <- unlist(artists$genres)
        genre_count <- as.data.frame(table(genres))
        genre_count <- genre_count[order(-genre_count$Freq), ]
        
        total_time <- sum(tracks$duration_ms) / (1000 * 60)  # Convert ms to minutes
        
        # Store results
        user_data(list(
            tracks = tracks,
            artists = artists,
            genres = genre_count,
            total_time = total_time
        ))
    })
    
    observeEvent(input$analyze, {
        req(input$token)
        access_token <- input$token
        
        # Fetch recently played tracks or user's top tracks
        tracks <- get_my_top(type = "tracks", time_range = "long_term", limit = 50, authorization = access_token)
        
        # Add a timestamp approximation
        tracks$played_at <- tracks$added_at  # Replace 'added_at' with actual timestamps if available
        
        # Store data for reactive use
        user_data(tracks)
    })
    trends_data <- reactive({
        req(user_data())
        
        user_data() %>%
            mutate(month = floor_date(as.Date(played_at), "month")) %>%  # Group by month
            group_by(month) %>%
            summarize(
                total_songs = n(),                           # Total songs listened
                total_duration = sum(duration_ms, na.rm = TRUE) / 60000   # Total duration in minutes
            )
    })
    output$listeningTrendsPlot <- renderPlot({
        req(trends_data())
        
        ggplot(trends_data(), aes(x = month)) +
            geom_line(aes(y = total_songs), color = "blue", size = 1) +  # Line for total songs
            geom_bar(aes(y = total_duration), stat = "identity", fill = "lightblue", alpha = 0.5) +  # Bars for duration
            labs(
                title = "User's Listening Trends Over Time",
                x = "Month",
                y = "Total Songs / Listening Time (min)"
            ) +
            theme_minimal()
    })
    
    # Output Summary
    output$summary <- renderText({
        data <- user_data()
        req(data)
        paste0("Total time spent listening: ", round(data$total_time, 2), " minutes")
    })
    
    # Genre Analysis Plot
    output$genrePlot <- renderPlot({
        req(input$show_genres)
        data <- user_data()
        req(data)
        ggplot(data$genres, aes(x = reorder(genres, Freq), y = Freq)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            labs(title = "Top Genres", x = "Genres", y = "Count")
    })
    
    # Top Tracks Table
    output$topTracks <- renderTable({
        req(input$show_tracks)
        data <- user_data()
        req(data)
        data$tracks %>%
            select(name, artists = artists.name, popularity) %>%
            arrange(-popularity) %>%
            head(10)
    })
    
    # Listening Time Trends (Placeholder for now)
    output$timePlot <- renderPlot({
        req(input$show_time)
        ggplot() +
            geom_line() +
            labs(title = "Listening Time Trends", x = "Date", y = "Time Spent")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
