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
library(shinythemes)
library(bslib)
library(plotly)

# Function to get track features based on user preferences
get_audio_features <- function(tracks) {
    audio_features <- spotifyr::get_audio_features(tracks, access_token = access_token)
    return(audio_features)
}


dashboard_ui <- tabsetPanel(
  tabPanel("Summary",
           icon = icon("fa-regular fa-file"),
           textOutput("summary"), class="inner-tab"),
  
  tabPanel("Top Tracks",
           icon = icon("music"),
           fluidPage(
               card(
                   h3("Metrics Exploration"),
                   fluidRow(
                       column(6, 
                              sliderInput("danceability", "Danceability:", 
                                          min = 0, max = 1, value = c(0.4, 0.8))),
                       column(6, 
                              sliderInput("energy", "Energy:", 
                                          min = 0, max = 1, value = c(0.4, 0.8)))
                   ),
                   fluidRow(
                       column(6, 
                              sliderInput("valence", "Valence:", 
                                          min = 0, max = 1, value = c(0.4, 0.8))),
                       column(6, 
                              sliderInput("speechiness", "Speechiness:", 
                                          min = 0, max = 1, value = c(0.2, 0.6)))
                   ),
                   actionButton("apply_filters", "Apply Filters")
               ),
               card(
                   DT::dataTableOutput("filtered_songs_table")
                   )
               )
           ),
  
  tabPanel("Genres", 
           icon = icon("bars-staggered"),
           plotOutput("genrePlot"), class="inner-tab"),
  tabPanel("Listening Trends",
           icon = icon("arrow-trend-up"),
           plotOutput("listeningTrendsPlot"),
           p("This chart shows how your listening habits have changed over time."), class="inner-tab")
)
    

main_ui <- navbarPage(
  title = tags$img(src = "spotify_logo.png", height = "32px"),  # Add Spotify logo
  theme = shinythemes::shinytheme("cosmo"),  # Apply a dark theme
          tabPanel("Dashboard",
           icon = icon("fa-solid fa-user"),
           fluidRow(
             column(3, class = "sidebar",
                    #style = "border: 2px solid white; padding: 15px; border-radius: 10px;", # White frame style
                    textInput("CID", "Enter your Client ID:", ""),
                    textInput("CST", "Enter your Client Secret:", ""),
                    textInput("RURL", "Enter your Redirect URL:", "http://localhost:1410/"),
                    textInput("token", "Enter your Spotify Token:", ""),
                    actionButton("analyze", "Lock In"),
                  hr(),
                  h3("Analysis Options"),
                  checkboxInput("show_genres", "Show Genre Analysis", TRUE),
                  checkboxInput("show_tracks", "Show Most Listelned Tracks", TRUE),
                  checkboxInput("show_time", "Show Listening Time Trends", TRUE),
                  #selectInput("metric", "Choose a Metric:", 
                  #            choices = c("Speechiness", "Danceability", "Energy", "Valence")),
                  #sliderInput("metricRange", "Select Metric Range:", 
                  #            min = 0, max = 1, value = c(0.4, 0.6)),
                  #actionButton("filter", "Filter Songs")
                  ),
             column(9, 
                    dashboard_ui),
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

  tabPanel("Audio Feature",
           icon = icon('music'),
           sidebarLayout(
               sidebarPanel(textInput("spotify_token", "Enter your Spotify Token:"),
                            selectInput("artist3", "Choose Artist for Tracks:", choices = c("Speechiness", "Danceability", "Energy", "Valence")),
                            actionButton("generate_plot", "Generate Plot")
                            ),
               mainPanel(h3("Audio Feature Scatter Plot"),
                         plotlyOutput("scatter_plot"))
           )),
  tabPanel(
     "Playlist Generate",
      icon = icon("list"),
      fluidPage(
          column(4, 
                 h3("Customize Your Playlist"),
                 selectInput("artist2", "Choose Similar Artist:", 
                             choices = NULL, # Will be populated dynamically
                             multiple = FALSE),
                 selectInput("mood", "Select Mood:", 
                             choices = c("Happy", "Sad", "Energetic", "Chill")),
                 sliderInput("danceability", "Danceability:", 
                             min = 0, max = 1, value = c(0.4, 0.8)),
                 sliderInput("energy", "Energy:", 
                             min = 0, max = 1, value = c(0.4, 0.8)),
                 sliderInput("valence", "Valence (Mood):", 
                             min = 0, max = 1, value = c(0.4, 0.8)),
                 sliderInput("popularity", "Track Popularity:", 
                             min = 0, max = 100, value = c(50, 100)),
                 actionButton("generate_playlist", "Generate Playlist", class = "btn-success")
          ),
          column(8,
                 h3("Generated Playlist"),
                 tableOutput("playlist_table"), # To display playlist details
                 actionButton("save_playlist", "Save Playlist", class = "btn-primary")
          )
      )
  ),
  tabPanel("Share this App",
           icon = icon("arrow-up-right-from-square"),
           ),
)



ui <- fluidPage(
    tags$style(HTML("
 /* Global Styles */
    body {
      background-color: #100000; /* Dark background */
      color: #FFFFFF; /* White text */
      font-family: Arial, sans-serif; /* Consistent font */
    }

    /* Navbar Styles */
    .navbar {
      background-color: #1f1f1f !important; /* Black background */
      border: none; /* Remove border */
    }
    .navbar-nav > li > a {
      color: white !important; /* White text */
      font-weight: bold; /* Bold tabs */
      padding: 15px 30px; /* Spacing for tabs */
      text-align: center; /* Center-align */
    }
    .navbar-nav > li > a:hover {
      background-color: #444 !important; /* Dark gray hover */
    }
    .navbar-nav > li.active > a {
      background-color: #1DB954 !important; /* Spotify green */
      color: white !important; /* White text for active */
    }
    .navbar-header .navbar-brand {
      color: white !important; /* White logo text */
      font-weight: bold;
    }

    /* Tab Styles */
    .nav-tabs > li > a {
      color: #FFFFFF; /* White text */
      background-color: transparent; /* Transparent tabs */
      border: 1px solid #444; /* Subtle border */
      border-radius: 10px 10px 0 0; /* Rounded corners */
      padding: 8px 15px; /* Padding inside tabs */
      margin-right: 5px; /* Space between tabs */
    }
    .nav-tabs > li.active > a {
      background-color: #1DB954 !important; /* Active Spotify green */
      border-color: #1DB954; /* Match border */
    }

    /* Checkbox Styles */
    input[type='checkbox'] {
      accent-color: #1DB954; /* Spotify green checkmark */
      background-color: black; /* Dark box */
      border: 1px solid #444; /* Border */
    }
    input[type='checkbox']:hover {
      background-color: #1DB954; /* Green hover */
    }
    input[type='checkbox']:checked {
      background-color: #1DB954; /* Checked color */
    }

    /* Sidebar Styles */
    .sidebar {
      border-radius: 10px; /* Rounded corners */
      padding: 10px; /* Inner padding */
      background-color: #1f1f1f; /* Dark sidebar background */
      color: #FFFFFF; /* White text */
    }

    /* Slider Styles */
    
    .irs-bar, .irs-bar-edge, .irs-single, .irs-slider {
      background: #1DB954 !important; /* Spotify green */
      border-color: #1DB954 !important;
    }
    .irs-grid-pol {
      background: #FFFFFF !important; /* White grid lines */
    }
    .irs-grid-text {
      color: #FFFFFF !important; /* White grid text */
    } 
                    /* Change the background and text color for active tabs inside a tabPanel */
.nav-tabs > li > a.active,
.nav-tabs > li > a.active:focus,
.nav-tabs > li > a.active:hover {
  background-color: #1DB954 !important; /* Spotify green for active tab */
  color: #000000 !important; /* Black text for active tab */
  border-color: #1DB954; /* Match border with background */
}

/* Optional: Adjust inactive tabs for better contrast */
.nav-tabs > li > a {
  background-color: transparent; /* Transparent background for unselected tabs */
  color: #FFFFFF !important; /* White text for unselected tabs */
  border: 1px solid #444; /* Subtle border */
  border-radius: 10px 10px 0 0; /* Rounded corners */
}

/* Adjust hover effect for inactive tabs */
.nav-tabs > li > a:hover {
  background-color: #444 !important; /* Darker gray for hover */
  color: #FFFFFF !important; /* Keep text white */
}
")
    ),
    main_ui
)


server <- function(input, output, session) {
    # Reactive value to store user data
    user_data <- reactiveVal()
    
    # Fetch and process Spotify data on 'Analyze' button click
    observeEvent(input$analyze, {
        req(input$token)  # Ensure token is provided

        access_token <- input$token
        auth_header <- paste("Bearer", access_token)
        
        tryCatch({
            # Fetch data from Spotify API
            tracks <- get_my_top_artists_or_tracks(
                type = "tracks",
                time_range = "long_term",
                limit = 50,
                authorization = auth_header  # Pass the formatted token here
            )
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
    
    # Sample Spotify track data
    all_tracks <- reactive({
        req(user_data()) # Assuming `user_data` holds your tracks data
        user_data()$tracks %>%
            mutate(
                album_image = paste0("<img src='", album.images[[1]]$url, "' height='50'>") # Add album image as HTML
            )
    })
    
    # Reactive filter based on user inputs
    filtered_tracks <- reactive({
        req(all_tracks())
        all_tracks() %>%
            filter(
                danceability >= input$danceability[1],
                danceability <= input$danceability[2],
                energy >= input$energy[1],
                energy <= input$energy[2],
                valence >= input$valence[1],
                valence <= input$valence[2]
            )
    })
    
    # Render dynamic DataTable
    output$filtered_songs_table <- DT::renderDataTable({
        req(filtered_tracks())
        DT::datatable(
            filtered_tracks() %>%
                select(
                    "Song Name" = name,
                    "Artist" = artists.name,
                    "Genre" = genres,   # Assuming `genres` is available
                    "Album Image" = album_image
                ),
            escape = FALSE,  # Allow HTML rendering for album images
            options = list(pageLength = 10, autoWidth = TRUE)
        )
    })
    
    # Generate Playlist
    observeEvent(input$artist, {
        related_artists <- spotifyr::get_related_artists(input$artist_id) # Replace with actual ID
        updateSelectInput(session, "artist", choices = related_artists$name)
    })
    
    observeEvent(input$generate_playlist, {
        playlist <- spotifyr::get_recommendations(
            seed_artists = selected_artist_id, # Obtain from input
            target_danceability = mean(input$danceability),
            target_energy = mean(input$energy),
            target_valence = mean(input$valence),
            min_popularity = input$popularity[1],
            max_popularity = input$popularity[2]
        )
        
        output$playlist_table <- renderTable({
            playlist %>% 
                select(track_name, artist_name, album_name, popularity) # Customize displayed columns
        })
    })
    
    observeEvent(input$save_playlist, {
        playlist_id <- spotifyr::create_playlist(
            user_id = spotifyr::get_my_profile()$id,
            name = paste("Custom Playlist -", Sys.Date()),
            description = "Generated using the Spotify R App"
        )
        
        spotifyr::add_tracks_to_playlist(playlist_id, tracks = playlist$track_id)
        showNotification("Playlist saved successfully!", type = "message")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)