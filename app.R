library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(plotly)
library(shinycssloaders)
library(shinythemes)

client_id <- Sys.getenv("SPOTIFY_CLIENT_ID", "")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET", "")

if (client_id == "" || client_secret == "") {
  warning("Spotify API credentials not found. Set SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET environment variables.")
}

get_spotify_token <- function(client_id, client_secret) {
  response <- POST(
    url = "https://accounts.spotify.com/api/token",
    authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  content(response)$access_token
}

get_artist_albums <- function(artist_name, token) {
  search_url <- paste0("https://api.spotify.com/v1/search?q=", URLencode(artist_name), "&type=artist")
  search_response <- GET(search_url, add_headers(Authorization = paste("Bearer", token)))
  
  if (http_error(search_response)) {
    warning("Error in Spotify API search call: ", content(search_response, "text"))
    return(list(albums = list(), artist_image = NULL))
  }
  
  result <- content(search_response)
  if (length(result$artists$items) == 0) {
    warning("No artist found with name: ", artist_name)
    return(list(albums = list(), artist_image = NULL))
  }
  
  artist_info <- result$artists$items[[1]]
  artist_id <- artist_info$id
  artist_image <- ifelse(length(artist_info$images) > 0, artist_info$images[[1]]$url, NULL)
  
  albums_url <- paste0("https://api.spotify.com/v1/artists/", artist_id, "/albums?include_groups=album&market=US&limit=50")
  albums_response <- GET(albums_url, add_headers(Authorization = paste("Bearer", token)))
  
  # Error handling for albums request
  if (http_error(albums_response)) {
    warning("Error in Spotify API albums call: ", content(albums_response, "text"))
    return(list(albums = list(), artist_image = artist_image))
  }
  
  albums <- content(albums_response)$items
  
  list(albums = albums, artist_image = artist_image)
}

get_album_tracks <- function(album_id, token) {
  tracks_url <- paste0("https://api.spotify.com/v1/albums/", album_id, "/tracks?market=US&limit=50")
  tracks_response <- GET(tracks_url, add_headers(Authorization = paste("Bearer", token)))
  
  # Error handling
  if (http_error(tracks_response)) {
    warning("Error in Spotify API tracks call: ", content(tracks_response, "text"))
    return(list())
  }
  
  content(tracks_response)$items
}

# Initialize token
tryCatch({
  spotify_token <- get_spotify_token(client_id, client_secret)
}, error = function(e) {
  message("Failed to get Spotify token: ", e$message)
  spotify_token <- NULL
})

# Use a relative path to the dataset - make sure this file is included in your deployment
# For shinyapps.io, this will work as long as the file is in the same directory as this script
local_data <- tryCatch({
  read_csv("dataset_to_use_spotify.csv", show_col_types = FALSE)
}, error = function(e) {
  # Provide a user-friendly error
  stop("Error loading dataset: ", e$message, 
       "\nMake sure 'dataset_to_use_spotify.csv' is in the same directory as this app.")
})

total_tracks <- nrow(local_data)
total_artists <- length(unique(local_data$artists))
total_albums <- length(unique(local_data$album_name))

ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap');
      body {
        background-color: #181818;
        color: #FFFFFF;
        font-family: 'Montserrat', sans-serif;
      }
      .navbar, .navbar-default {
        background-color: #1DB954;
        color: #FFFFFF;
      }
      .form-control {
        background-color: #282828;
        color: #FFFFFF;
        border: 1px solid #1DB954;
      }
      .selectize-input {
        background-color: #1DB954 !important;
        color: #000000 !important;
        border: 1px solid #1DB954;
      }
      .selectize-dropdown {
        background-color: #282828 !important;
        color: #FFFFFF !important;
        border: 1px solid #1DB954;
      }
      .well, .panel {
        background-color: #282828 !important;
        border: none !important;
        box-shadow: none !important;
      }
      .panel-body {
        background-color: #282828 !important;
      }
      .panel-heading {
        background-color: #282828 !important;
        border: none !important;
      }
      .btn {
        background-color: #1DB954;
        color: #000000;
        border: none;
        font-weight: bold;
        padding: 10px 20px;
        text-transform: uppercase;
      }
      .btn:hover {
        background-color: #1ed760;
      }
      .album-cover {
        height: 150px;
        width: 150px;
        border-radius: 10px;
        margin: 10px;
      }
      .track-info {
        color: #FFFFFF;
        margin: 10px;
      }
      .main-panel {
        padding: 20px;
        background-color: #181818;
        border-radius: 10px;
      }
      .overview-panel {
        margin-bottom: 20px;
      }
      .header-logo {
        width: 50px;
        height: auto;
        margin-right: 10px;
      }
      .header-title {
        display: flex;
        align-items: center;
        
      }
      .overview-header {
        font-size: 1.5em;
        margin-bottom: 20px;
      }
      .stat-box {
        background-color: #282828;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        text-align: center;
      }
      .stat-box h3 {
        font-size: 1.5em;
        margin: 0;
      }
      .stat-box p {
        font-size: 1.2em;
        margin: 0;
      }
      .top-stat-panel {
        display: flex;
        justify-content: space-around;
        margin-bottom: 20px;
      }
      .top-stat-box {
        background-color: #282828;
        border-radius: 10px;
        padding: 20px;
        text-align: center;
        flex: 1;
        margin: 0 10px;
      }
      .top-stat-box h3 {
        font-size: 1.5em;
        margin: 0;
      }
      .top-stat-box p {
        font-size: 1.2em;
        margin: 0;
      }
      .tab-panel {
        background-color: #282828;
        border-radius: 10px;
        padding: 20px;
      }
      .slider-title {
        font-weight: bold;
      }
      .slider-description {
        font-size: 0.9em;
        color: #CCCCCC;
        margin-bottom: 10px;
      }
      .irs-bar, .irs-bar-edge, .irs-single, .irs-to, .irs-from {
        background-color: #1db95442 !important;
        border-color: #1DB954 !important;
      }
      .irs-line, .irs-grid-pol {
        background: #282828 !important;
      }
      .irs-grid-text, .irs-to, .irs-from, .irs-min, .irs-max {
        color: #FFFFFF !important;
      }
      .footer {
        text-align: center;
        padding: 10px;
        margin-top: 20px;
        color: #AAAAAA;
        font-size: 0.8em;
      }
    "))
  ),
  div(
    class = "header-title", 
    tags$img(src = "https://storage.googleapis.com/pr-newsroom-wp/1/2018/11/Spotify_Logo_RGB_White.png", class = "header-logo"),
    h1("Spotify Dashboard")
  ),
  div(
    class = "top-stat-panel",
    div(class = "top-stat-box",
        h3("Total Tracks"),
        p(total_tracks)
    ),
    div(class = "top-stat-box",
        h3("Total Artists"),
        p(total_artists)
    ),
    div(class = "top-stat-box",
        h3("Total Albums"),
        p(total_albums)
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("artist", "Select an Artist:", choices = c("All", unique(local_data$artists))),
      uiOutput("artist_image_ui"),
      uiOutput("album_ui"),
      div(class = "slider-title", "Popularity"),
      sliderInput("popularity_filter", "", min = 0, max = 100, value = c(0, 100)),
      div(class = "slider-title", "Danceability"),
      sliderInput("danceability_filter", "", min = 0, max = 1, value = c(0, 1)),
      div(class = "slider-title", "Energy"),
      sliderInput("energy_filter", "", min = 0, max = 1, value = c(0, 1)),
      div(class = "slider-title", "Tempo (BPM)"),
      sliderInput("tempo_filter", "", min = 0, max = 250, value = c(0, 250)),
      div(class = "slider-title", "Valence"),
      sliderInput("valence_filter", "", min = 0, max = 1, value = c(0, 1)),
      div(class = "slider-title", "Liveness"),
      sliderInput("liveness_filter", "", min = 0, max = 1, value = c(0, 1)),
      div(class = "slider-title", "Instrumentalness"),
      sliderInput("instrumentalness_filter", "", min = 0, max = 1, value = c(0, 1))
    ),
    mainPanel(
      div(
        class = "main-panel",
        tabsetPanel(
          tabPanel("Overview",
                   div(class = "tab-panel",
                       fluidRow(
                         column(6, plotlyOutput("barChartArtistTrackCount") %>% withSpinner()),
                         column(6, plotlyOutput("barChartGenrePopularity") %>% withSpinner())
                       ),
                       fluidRow(
                         column(6, plotlyOutput("linePlotPopularityKey") %>% withSpinner()),
                         column(6, plotlyOutput("scatterPlotPopularityValence") %>% withSpinner())
                       ),
                       fluidRow(
                         column(6, plotlyOutput("scatterPlotPopularityTempo") %>% withSpinner()),
                         column(6, plotlyOutput("linePlotAverageDanceability") %>% withSpinner())
                       ),
                       fluidRow(
                         column(6, plotlyOutput("scatterPlotPopularityLiveness") %>% withSpinner()),
                         column(6, plotlyOutput("linePlotAverageInstrumentalness") %>% withSpinner())
                       )
                   )
          ),
          tabPanel("Album Analysis",
                   div(class = "tab-panel",
                       fluidRow(
                         column(6, uiOutput("albums_ui")),
                         column(6, uiOutput("tracks_ui"))
                       )
                   )
          ),
          tabPanel("About",
                   div(class = "tab-panel",
                       h3("About This Dashboard"),
                       p("This interactive Spotify Dashboard allows you to explore music data across various artists, albums, and tracks."),
                       p("The data includes audio features like danceability, energy, tempo, and popularity ratings."),
                       h4("How to Use"),
                       tags$ul(
                         tags$li("Select an artist from the dropdown menu to filter the data"),
                         tags$li("Use the sliders to filter by audio features"),
                         tags$li("View album details and track listings in the Album Analysis tab"),
                         tags$li("Explore visualizations showing relationships between different audio features and popularity")
                       ),
                       h4("Data Source"),
                       p("The data used in this dashboard comes from the Spotify Web API and includes information about tracks, artists, albums, and audio features."),
                       h4("Created By"),
                       p("This dashboard was created using R Shiny and the Spotify Web API.")
                   )
          )
        )
      )
    )
  ),
  div(class = "footer",
      p("Data sourced from Spotify Web API. This is an educational project and not affiliated with Spotify."))
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- local_data
    if (input$artist != "All") {
      data <- data %>% filter(grepl(input$artist, artists))
    }
    data %>%
      filter(
        popularity >= input$popularity_filter[1] & popularity <= input$popularity_filter[2],
        danceability >= input$danceability_filter[1] & danceability <= input$danceability_filter[2],
        energy >= input$energy_filter[1] & energy <= input$energy_filter[2],
        tempo >= input$tempo_filter[1] & tempo <= input$tempo_filter[2],
        valence >= input$valence_filter[1] & valence <= input$valence_filter[2],
        liveness >= input$liveness_filter[1] & liveness <= input$liveness_filter[2],
        instrumentalness >= input$instrumentalness_filter[1] & instrumentalness <= input$instrumentalness_filter[2]
      )
  })
  
  artist_albums <- reactive({
    req(input$artist, spotify_token)
    if (input$artist != "All") {
      tryCatch({
        get_artist_albums(input$artist, spotify_token)
      }, error = function(e) {
        # Return empty list if API call fails
        warning("Error getting artist albums: ", e$message)
        list(albums = list(), artist_image = NULL)
      })
    } else {
      list(albums = list(), artist_image = NULL)
    }
  })
  
  output$artist_image_ui <- renderUI({
    if (input$artist != "All") {
      artist_image <- artist_albums()$artist_image
      if (!is.null(artist_image)) {
        tags$img(src = artist_image, height = "200px", style = "display: block; margin-left: auto; margin-right: auto; border-radius: 10px;")
      }
    }
  })
  
  output$album_ui <- renderUI({
    req(input$artist)
    if (input$artist != "All") {
      artist_data <- local_data %>% filter(grepl(input$artist, artists))
      dataset_albums <- unique(artist_data$album_name)
      album_choices <- c("All", dataset_albums)
      selectInput("album", "Select an Album:", choices = album_choices)
    }
  })
  
  output$albums_ui <- renderUI({
    req(artist_albums())
    albums <- artist_albums()$albums
    dataset_albums <- unique(local_data$album_name)
    album_ui_list <- lapply(albums, function(album) {
      if (album$name %in% dataset_albums) {
        album_id <- album$id
        album_image <- ifelse(length(album$images) > 0, album$images[[1]]$url, NULL)
        album_name <- album$name
        tags$div(
          if (!is.null(album_image)) {tags$img(src = album_image, class = "album-cover")},
          tags$div(class = "track-info", h4(album_name)),
          actionButton(inputId = paste0("album_", album_id), label = "View Tracks", class = "btn")
        )
      }
    })
    do.call(tagList, album_ui_list)
  })
  
  observe({
    req(artist_albums(), spotify_token)
    lapply(artist_albums()$albums, function(album) {
      if (album$name %in% unique(local_data$album_name)) {
        observeEvent(input[[paste0("album_", album$id)]], {
          tracks <- tryCatch({
            get_album_tracks(album$id, spotify_token)
          }, error = function(e) {
            # Return empty list if API call fails
            warning("Error getting album tracks: ", e$message)
            list()
          })
          
          output$tracks_ui <- renderUI({
            track_ui_list <- lapply(tracks, function(track) {
              tags$div(class = "track-info", 
                       tags$a(href = paste0("https://open.spotify.com/track/", track$id), target = "_blank", track$name)
              )
            })
            do.call(tagList, track_ui_list)
          })
        })
      }
    })
  })
  
  filtered_album_data <- reactive({
    data <- filtered_data()
    if (!is.null(input$album) && input$album != "All") {
      data <- data %>% filter(album_name == input$album)
    }
    data
  })
  
  output$barChartArtistTrackCount <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    artist_track_count <- filtered_data() %>%
      group_by(artists) %>%
      summarize(Track_Count = n()) %>%
      arrange(desc(Track_Count)) %>%
      head(10)
    
    plot_ly(artist_track_count, x = ~reorder(artists, -Track_Count), y = ~Track_Count, type = 'bar',
            marker = list(color = 'rgb(29,185,84)')) %>%
      layout(title = 'Top 10 Artists by Total Track Count',
             xaxis = list(title = 'Artist', tickangle = -45),
             yaxis = list(title = 'Track Count'),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$barChartGenrePopularity <- renderPlotly({
    req(input$artist != "All" && nrow(filtered_album_data()) > 0)
    avg_popularity_genre <- filtered_album_data() %>%
      group_by(track_genre) %>%
      summarize(Average_Popularity = mean(popularity, na.rm = TRUE)) %>%
      arrange(desc(Average_Popularity)) %>%
      head(10)
    
    plot_ly(avg_popularity_genre, x = ~reorder(track_genre, -Average_Popularity), y = ~Average_Popularity, type = 'bar',
            marker = list(color = 'rgb(29,185,84)')) %>%
      layout(title = 'Top 10 Genres by Average Popularity',
             xaxis = list(title = 'Genre', tickangle = -45),
             yaxis = list(title = 'Average Popularity'),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$linePlotPopularityKey <- renderPlotly({
    req(input$artist != "All" && nrow(filtered_album_data()) > 0)
    avg_popularity_key <- filtered_album_data() %>%
      group_by(key) %>%
      summarize(Average_Popularity = mean(popularity, na.rm = TRUE))
    
    all_keys <- as.character(0:11)  
    
    plot_ly(avg_popularity_key, x = ~key, y = ~Average_Popularity, type = 'scatter', mode = 'lines+markers',
            marker = list(color = 'rgb(29,185,84)')) %>%
      layout(title = 'Average Popularity by Key',
             xaxis = list(title = 'Key',
                          categoryorder = "array",
                          categoryarray = all_keys),
             yaxis = list(title = 'Average Popularity'),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$linePlotAverageDanceability <- renderPlotly({
    req(input$artist != "All" && nrow(filtered_album_data()) > 0)
    avg_danceability <- filtered_album_data() %>%
      group_by(track_name) %>%
      summarize(Average_Danceability = mean(danceability, na.rm = TRUE))
    plot_ly(avg_danceability, x = ~track_name, y = ~Average_Danceability, type = 'scatter', mode = 'lines+markers',
            marker = list(color = 'rgb(29,185,84)')) %>%
      layout(title = 'Average Danceability by Track',
             xaxis = list(title = 'Track', tickangle = -45),
             yaxis = list(title = 'Average Danceability'),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$scatterPlotPopularityValence <- renderPlotly({
    req(input$artist != "All" && nrow(filtered_album_data()) > 0)
    plot_ly(data = filtered_album_data(), x = ~valence, y = ~popularity, type = 'scatter', mode = 'markers',
            text = ~paste("Track:", track_name, "<br>Artist:", artists, "<br>Album:", album_name),
            marker = list(color = ~popularity, colorscale = 'Viridis', showscale = TRUE)) %>%
      layout(title = 'Popularity vs. Valence',
             xaxis = list(title = 'Valence'),
             yaxis = list(title = 'Popularity'),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$scatterPlotPopularityTempo <- renderPlotly({
    req(input$artist != "All" && nrow(filtered_album_data()) > 0)
    plot_ly(data = filtered_album_data(), x = ~tempo, y = ~popularity, type = 'scatter', mode = 'markers',
            text = ~paste("Track:", track_name, "<br>Artist:", artists, "<br>Album:", album_name),
            marker = list(color = ~popularity, colorscale = 'Viridis', showscale = TRUE)) %>%
      layout(title = 'Popularity vs. Tempo',
             xaxis = list(title = 'Tempo (BPM)'),
             yaxis = list(title = 'Popularity'),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$scatterPlotPopularityLiveness <- renderPlotly({
    req(input$artist != "All" && nrow(filtered_album_data()) > 0)
    plot_ly(data = filtered_album_data(), x = ~liveness, y = ~popularity, type = 'scatter', mode = 'markers',
            text = ~paste("Track:", track_name, "<br>Artist:", artists, "<br>Album:", album_name),
            marker = list(color = ~popularity, colorscale = 'Viridis', showscale = TRUE)) %>%
      layout(title = 'Popularity vs. Liveness',
             xaxis = list(title = 'Liveness'),
             yaxis = list(title = 'Popularity'),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$linePlotAverageInstrumentalness <- renderPlotly({
    req(input$artist != "All" && nrow(filtered_album_data()) > 0)
    avg_instrumentalness <- filtered_album_data() %>%
      group_by(track_name) %>%
      summarize(Average_Instrumentalness = mean(instrumentalness, na.rm = TRUE))
    plot_ly(avg_instrumentalness, x = ~track_name, y = ~Average_Instrumentalness, type = 'scatter', mode = 'lines+markers',
            marker = list(color = 'rgb(29,185,84)')) %>%
      layout(title = 'Average Instrumentalness by Track',
             xaxis = list(title = 'Track', tickangle = -45),
             yaxis = list(title = 'Average Instrumentalness'),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
}

shinyApp(ui = ui, server = server)