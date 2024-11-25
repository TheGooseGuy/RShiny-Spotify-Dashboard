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
      border-top-left-radius: 10px;
      border-top-right-radius: 10px;
    }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
      color: #FFFFFF; /* White text for active tab title */
      background-color: #444444; /* Optional: dark background for active tab */
      border-top-left-radius: 10px;
      border-top-right-radius: 10px;
    }
    input[type='checkbox'] {
      accent-color: #1DB954; /* Changes the check color */
      background-color: black; /* Changes the box color */
    }
    input[type='checkbox']:checked {
      background-color: #1DB954; /* Box color when checked */
    }
    .nav-tabs .nav-link {
      border-radius: 10px 10px 0 0; /* Round corners of tab buttons */
      margin-right: 5px;  /* Spacing between tabs */
      border: 1px solid #007bff; /* Tab border color */
      padding: 8px 15px; /* Padding inside tabs */
      color: #007bff; /* Text color */
    }
    
    .sidebar {
      border-radius: 10px
    }
    
  ")),
  tabPanel("Metrics Exploration", 
           textOutput("metricDescription"),
           plotOutput("metricPlot"),
           tableOutput("filteredSongs"),
           class="inner-tab"),
  tabPanel("Summary", textOutput("summary"), class="inner-tab"),
  tabPanel("Genres", plotOutput("genrePlot"), class="inner-tab"),
  tabPanel("Top Tracks", tableOutput("topTracks"), class="inner-tab"),
  tabPanel("Listening Trends",
           plotOutput("listeningTrendsPlot"),
           p("This chart shows how your listening habits have changed over time."), class="inner-tab")
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
    /* Spotify Green slider track and handle */
    .irs-bar {
      background: #1DB954 !important; /* Spotify Green for the slider bar */
      border-color: #1DB954 !important; /* Spotify Green for the border */
    }
    .irs-bar-edge {
      background: #1DB954 !important; /* Green color for the edges */
      border-color: #1DB954 !important;
    }
    .irs-single {
      background: #1DB954 !important; /* Green for the selection tooltip */
      color: #ffffff !important; /* White text inside tooltip */
    }
    .irs-slider {
      background: #1DB954 !important; /* Green for the draggable handle */
      border-color: #1DB954 !important;
    }
    .irs-grid-pol {
      background: #FFFFFF !important; /* Change grid lines to white */
    }
    .irs-grid-text {
      color: #FFFFFF !important; /* Change grid text to white */
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
           tabsetPanel(
             tabPanel("Introduction",
                   HTML("")   
                       
             ),
           tabPanel("Top 10 Streams",
                   h3("202x Top 10 Tracks by Streams"),
                   htmlOutput("topstreamsummary"),
                   plotOutput("topstreamplot")),
           
           tabPanel("Top 10 playlists",
                    h3("202x Top 10 Tracks by Spotify Playlists"),
                    htmlOutput('topplaylistssummary'),
                    plotOutput("topplaylistsplot")))
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
    # Reactive value to store user data
    user_data <- reactiveVal()
    
    # Fetch and process Spotify data on 'Analyze' button click
    observeEvent(input$analyze, {
        req(input$token)  # Ensure token is provided
        
        access_token <- input$token
        
        tryCatch({
            # Fetch data from Spotify API
            tracks <- get_my_top_artists_or_tracks(type = "tracks", time_range = "long_term", limit = 50, authorization = access_token)
            artists <- get_my_top_artists_or_tracks(type = "artists", time_range = "long_term", limit = 50, authorization = access_token)
            
            # Process genres
            genres <- unlist(artists$genres)
            genre_count <- as.data.frame(table(genres))
            genre_count <- genre_count[order(-genre_count$Freq), ]
            
            # Calculate total listening time
            total_time <- sum(tracks$duration_ms, na.rm = TRUE) / (1000 * 60)  # Convert ms to minutes
            
            # Store results in a reactive value
            user_data(list(
                tracks = tracks,
                artists = artists,
                genres = genre_count,
                total_time = total_time
            ))
            
            # Notify success
            showNotification("Data successfully fetched!", type = "message")
        }, error = function(e) {
            # Handle errors
            showNotification(paste("Error:", e$message), type = "error")
        })
    })
    
    # Reactive data for trends
    trends_data <- reactive({
        req(user_data())
        
        user_data()$tracks %>%
            mutate(month = floor_date(as.Date(added_at), "month")) %>%  # Adjust for available timestamp field
            group_by(month) %>%
            summarize(
                total_songs = n(),
                total_duration = sum(duration_ms, na.rm = TRUE) / 60000  # Duration in minutes
            )
    })
    
    # Listening Trends Plot
    output$listeningTrendsPlot <- renderPlot({
        req(trends_data())
        
        ggplot(trends_data(), aes(x = month)) +
            geom_line(aes(y = total_songs), color = "blue", size = 1) +
            geom_bar(aes(y = total_duration), stat = "identity", fill = "lightblue", alpha = 0.5) +
            labs(
                title = "User's Listening Trends Over Time",
                x = "Month",
                y = "Total Songs / Listening Time (min)"
            ) +
            theme_minimal()
    })
    
    # Output for total listening time summary
    output$summary <- renderText({
        req(user_data())
        paste0("Total time spent listening: ", round(user_data()$total_time, 2), " minutes")
    })
    
    # Genre Analysis Plot
    output$genrePlot <- renderPlot({
        req(input$show_genres, user_data())
        ggplot(user_data()$genres, aes(x = reorder(genres, Freq), y = Freq)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            labs(title = "Top Genres", x = "Genres", y = "Count")
    })
    
    # Top Tracks Table
    output$topTracks <- renderTable({
        req(input$show_tracks, user_data())
        user_data()$tracks %>%
            select(name, artists = artists.name, popularity) %>%
            arrange(-popularity) %>%
            head(10)
    })
    
    # Placeholder for additional trends
    output$timePlot <- renderPlot({
        req(input$show_time)
        ggplot() +
            geom_line() +
            labs(title = "Listening Time Trends", x = "Date", y = "Time Spent")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)