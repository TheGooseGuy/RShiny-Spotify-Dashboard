#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(Rspotify)
library(spotifyr)
library(ggplot2)
library(dplyr)
library(lubridate)

# Fetch audio features for a user's top tracks
#tracks_audio_features <- get_audio_features(track_ids = tracks$id, authorization = access_token)

# Merge audio features with track data
#track_data <- tracks %>%
#    left_join(tracks_audio_features, by = c("id" = "id"))


ui <- fluidPage(
  titlePanel("Spotify User Behavior Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("token", "Enter your Spotify Token:", ""),
      actionButton("analyze", "Analyze My Data"),
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
    
    mainPanel(
      tabsetPanel(
        tabPanel("Metrics Exploration", 
                 textOutput("metricDescription"),
                 plotOutput("metricPlot"),
                 tableOutput("filteredSongs")),
        tabPanel("Data Reference", uiOutput("reference")),
        tabPanel("Genres", plotOutput("genrePlot")),
        tabPanel("Top Tracks", tableOutput("topTracks")),
        tabPanel("Listening Trends", plotOutput("timePlot")),
        tabPanel(
          "Listening Trends",
          plotOutput("listeningTrendsPlot"),
          p("This chart shows how your listening habits have changed over time, including the number of songs listened and the total listening duration.")
        ),
        tabPanel("Summary", textOutput("summary")),
      )
    )
  )
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
  
  # Introduce the data set
  output$reference <- renderText({HTML("<div style='font-size: 12px; line-height: 1.5;'>
     This dataset contains a comprehensive list of the most famous songs of 2023 as listed on Spotify.<br>
     The dataset offers a wealth of features beyond what is typically available in similar datasets.<br> 
     It provides insights into each song's attributes, popularity, and presence on various music platforms.<br> 
     The dataset includes information such as <strong> track name, artist(s) name, release date, Spotify playlists and charts, streaming statistics</strong> and so on.<br>
     However, we may only use some of features for our project to analysis the user behavior.<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
      <span style='font-size: 10px;'>Sources link: https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023/data </span></div>"
  )})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
