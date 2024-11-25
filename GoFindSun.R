install.packages(c("httr", "jsonlite", "dplyr", "tidyr", "leaflet", "lubridate", "shiny", "DT"))
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)

# Klucz API z OpenWeatherMap
api_key <- "de91afd2dd31621a9391bf27b720d080"

# Definicja funkcji pobierającej dane pogodowe
get_forecast_data <- function(lat, lon, api_key) {
  url <- paste0("http://api.openweathermap.org/data/2.5/forecast?lat=", lat, "&lon=", lon, "&appid=", api_key, "&units=metric")
  response <- httr::GET(url)
  if (httr::status_code(response) != 200) {
    stop("Nie udało się pobrać danych pogodowych.")
  }
  data <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
  return(data)
}

# Funkcja sprawdzająca, czy dane są aktualne
is_data_up_to_date <- function(file_path) {
  if (!file.exists(file_path)) {
    return(FALSE)  # Plik nie istnieje
  }
  file_date <- as.Date(file.info(file_path)$mtime)  # Data ostatniej modyfikacji pliku
  return(file_date == Sys.Date())  # Sprawdź, czy plik został zmodyfikowany dzisiaj
}

# Funkcja pobierająca prognozę pogody w partiach
get_forecast_data_in_batches <- function(cities, api_key, output_file) {
  batch_size <- 55  # Liczba zapytań na minutę
  num_batches <- ceiling(nrow(cities) / batch_size)  # Liczba partii
  
  all_data <- list()  # Lista na dane
  for (i in seq_len(num_batches)) {
    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, nrow(cities))
    
    batch <- cities[start_index:end_index, ]  # Pobierz partię miast
    
    batch_data <- lapply(seq_len(nrow(batch)), function(j) {
      city <- batch[j, ]
      tryCatch({
        data <- get_forecast_data(city$latitude, city$longitude, api_key)
        data_frame <- data.frame(
          city = city$city,
          lat = city$latitude,
          lon = city$longitude,
          date = as.Date(as.POSIXct(data$list$dt, origin = "1970-01-01", tz = "Europe/Warsaw")),
          temp = data$list$main.temp,
          clouds = data$list$clouds.all,
          wind_speed = data$list$wind.speed,
          humidity = data$list$main.humidity,
          rain = ifelse(!is.null(data$list$rain.3h) && !is.na(data$list$rain.3h), "Yes", "No"),
          snow = ifelse(!is.null(data$list$snow.3h) && !is.na(data$list$snow.3h), "Yes", "No")
        )
        return(data_frame)
      }, error = function(e) {
        message(paste("Error for city:", city$city, " - ", e$message))
        write(paste(Sys.time(), "Error for city:", city$city, "-", e$message), file = "error_log.txt", append = TRUE)
        return(NULL)
      })
    })
    
    all_data <- append(all_data, batch_data)
    
    if (i < num_batches) {
      Sys.sleep(70)  # Opóźnienie między partiami
    }
  }
  
  final_data <- do.call(rbind, all_data)
  
  # Obliczanie mediany temperatury i zachmurzenia dla każdego miasta i dnia
  final_data <- final_data %>%
    group_by(city, date) %>%
    mutate(
      median_temp = median(temp, na.rm = TRUE),
      median_clouds = median(clouds, na.rm = TRUE),
      median_humidity = median(humidity, na.rm = TRUE)
    ) %>%
    ungroup()
  
  write.csv(final_data, output_file, row.names = FALSE)  # Zapisz do pliku CSV
  
  return(final_data)
}

# Ścieżka do pliku CSV
output_file <- "forecast_poland.csv"

# Sprawdź, czy dane są aktualne
if (!is_data_up_to_date(output_file)) {
  message("Fetching new data...")
  
  # Załaduj listę miast z pliku CSV
  cities <- read.csv("C:/Users/Drukarnia/OneDrive - University of Gdansk (for Students)/Pulpit/IiE Analiza Danych - Big Data/Inzynieria oprogramowania/Cities - Poland.csv", stringsAsFactors = FALSE)
  
  # Pobierz dane w partiach
  forecast_data <- get_forecast_data_in_batches(cities, api_key, output_file)
  
  message("Data fetched and saved.")
} else {
  message("Data is already up to date.")
  forecast_data <- read.csv(output_file, stringsAsFactors = FALSE)
}


forecast_summary <- forecast_data %>%
  group_by(city, date) %>%
  summarise(
    temp_mediana = median(temp, na.rm = TRUE),
    clouds_mediana = median(clouds, na.rm = TRUE),
    humidity_mediana = median(humidity, na.rm = TRUE),
    wind_speed_max = max(wind_speed, na.rm = TRUE),
    rain = ifelse(any(rain == "Yes"), "Yes", "No"),
    snow = ifelse(any(snow == "Yes"), "Yes", "No"),
    lon = first(lon),  # Dodanie kolumny longitude
    lat = first(lat)   # Dodanie kolumny latitude
  ) %>%
  ungroup()

# Funkcje dla każdego rodzaju wyjazdu

calculate_romantic_score <- function(data) {
  data %>%
    mutate(
      temp_mediana_score = case_when(
        temp_mediana >= 15 & temp_mediana <= 25 ~ 1,
        temp_mediana >= 10 & temp_mediana < 15 ~ 0.8,
        temp_mediana > 25 & temp_mediana <= 30 ~ 0.8,
        TRUE ~ 0.4
      ),
      wind_score = case_when(
        wind_speed_max < 3.3 ~ 1,
        wind_speed_max >= 3.3 & wind_speed_max <= 5.4 ~ 0.9,
        wind_speed_max > 5.4 & wind_speed_max <= 7.9 ~ 0,
        wind_speed_max > 7.9 ~ -1
      ),
      clouds_mediana_score = case_when(
        clouds_mediana < 20 ~ 1,
        clouds_mediana >= 20 & clouds_mediana <= 50 ~ 0.8,
        TRUE ~ -0.2
      ),
      rain_score = ifelse(rain == "Yes", -1, 1),
      snow_score = ifelse(snow == "Yes", 0, 1),
      humidity_mediana_score = case_when(
        humidity_mediana >= 40 & humidity_mediana <= 60 ~ 1,
        humidity_mediana >= 0 & humidity_mediana < 40 ~ 0.8,
        humidity_mediana > 60 & humidity_mediana <= 70 ~ 0.7,
        TRUE ~ -0.2
      ),
      total_score = 0.2 * temp_mediana_score +
        0.1 * wind_score +
        0.1 * clouds_mediana_score +
        0.25 * rain_score +
        0.05 * snow_score +
        0.05 * humidity_mediana_score
    )
}

calculate_cultural_score <- function(data) {
  data %>%
    mutate(
      temp_mediana_score = case_when(
        temp_mediana >= 10 & temp_mediana <= 25 ~ 1,
        temp_mediana >= 5 & temp_mediana < 10 ~ 0.2,
        temp_mediana > 25 & temp_mediana <= 30 ~ 0.8,
        TRUE ~ -1
      ),
      wind_score = case_when(
        wind_speed_max < 3.3 ~ 1,
        wind_speed_max >= 3.3 & wind_speed_max <= 5.4 ~ 0.9,
        wind_speed_max > 5.4 & wind_speed_max <= 7.9 ~ 0,
        wind_speed_max > 7.9 ~ -1
      ),
      clouds_mediana_score = case_when(
        clouds_mediana < 20 ~ 1,
        clouds_mediana >= 20 & clouds_mediana <= 50 ~ 0.8,
        TRUE ~ -0.2
      ),
      rain_score = ifelse(rain == "Yes", -1, 1),
      snow_score = ifelse(snow == "Yes", -1, 1),
      humidity_mediana_score = case_when(
        humidity_mediana >= 40 & humidity_mediana <= 60 ~ 1,
        humidity_mediana >= 0 & humidity_mediana < 40 ~ 0.8,
        humidity_mediana > 60 & humidity_mediana <= 70 ~ 0.7,
        TRUE ~ -0.2
      ),
      total_score = 0.1 * temp_mediana_score +
        0.2 * wind_score +
        0.1 * clouds_mediana_score +
        0.25 * rain_score +
        0.05 * snow_score +
        0.05 * humidity_mediana_score
    )
}

calculate_photographic_score <- function(data) {
  data %>%
    mutate(
      temp_mediana_score = case_when(
        temp_mediana >= 15 & temp_mediana <= 25 ~ 1,
        temp_mediana >= 10 & temp_mediana < 15 ~ 0.8,
        temp_mediana > 25 & temp_mediana <= 30 ~ 0.8,
        TRUE ~ 0.4
      ),
      wind_score = case_when(
        wind_speed_max < 3.3 ~ 1,
        wind_speed_max >= 3.3 & wind_speed_max <= 5.4 ~ 0.9,
        wind_speed_max > 5.4 & wind_speed_max <= 7.9 ~ 0.5,
        wind_speed_max > 7.9 ~ -1
      ),
      clouds_mediana_score = case_when(
        clouds_mediana < 20 ~ 1,
        clouds_mediana >= 20 & clouds_mediana <= 50 ~ 0.8,
        TRUE ~ 0.4
      ),
      rain_score = ifelse(rain == "Yes", -1, 1),
      snow_score = ifelse(snow == "Yes", -1, 1),
      humidity_mediana_score = case_when(
        humidity_mediana >= 40 & humidity_mediana <= 60 ~ 1,
        humidity_mediana >= 0 & humidity_mediana < 40 ~ 0.8,
        humidity_mediana > 60 & humidity_mediana <= 70 ~ 0.7,
        TRUE ~ -1
      ),
      total_score = 0.1 * temp_mediana_score +
        0.2 * wind_score +
        0.1 * clouds_mediana_score +
        0.25 * rain_score +
        0.05 * snow_score +
        0.05 * humidity_mediana_score
    )
}

calculate_family_trip_score <- function(data) {
  data %>%
    mutate(
      temp_mediana_score = case_when(
        temp_mediana >= 10 & temp_mediana <= 25 ~ 1,
        temp_mediana >= 5 & temp_mediana < 10 ~ 0.3,
        temp_mediana > 25 & temp_mediana <= 30 ~ 0.5,
        TRUE ~ -1
      ),
      wind_score = case_when(
        wind_speed_max < 3.3 ~ 1,
        wind_speed_max >= 3.3 & wind_speed_max <= 5.4 ~ 0.7,
        wind_speed_max > 5.4 & wind_speed_max <= 7.9 ~ 0.2,
        wind_speed_max > 7.9 ~ -1
      ),
      clouds_mediana_score = case_when(
        clouds_mediana < 20 ~ 1,
        clouds_mediana >= 20 & clouds_mediana <= 50 ~ 0.8,
        TRUE ~ -0.2
      ),
      rain_score = ifelse(rain == "Yes", -1, 1),
      snow_score = ifelse(snow == "Yes", -1, 1),
      humidity_mediana_score = case_when(
        humidity_mediana >= 40 & humidity_mediana <= 60 ~ 1,
        humidity_mediana >= 0 & humidity_mediana < 40 ~ 0.8,
        humidity_mediana > 60 & humidity_mediana <= 70 ~ 0.7,
        TRUE ~ -0.2
      ),
      total_score = 0.15 * temp_mediana_score +
        0.1 * wind_score +
        0.1 * clouds_mediana_score +
        0.3 * rain_score +
        0.05 * snow_score +
        0.05 * humidity_mediana_score
    )
}

calculate_active_winter_score <- function(data) {
  data %>%
    mutate(
      wind_score = case_when(
        wind_speed_max <= 3.3 ~ 0.1,
        wind_speed_max > 3.3 & wind_speed_max <= 5.4 ~ 0.8,
        wind_speed_max > 5.4 & wind_speed_max <= 7.9 ~ 0.2,
        TRUE ~ -1.0
      ),
      temp_score = case_when(
        temp_mediana >= -10 & temp_mediana <= -2 ~ 1.0,
        temp_mediana >= -15 & temp_mediana < -10 ~ 0.5,
        temp_mediana >= -2 & temp_mediana <= 2 ~ 0.3,
        TRUE ~ -1.0
      ),
      cloud_score = case_when(
        clouds_mediana <= 20 ~ 1.0,
        clouds_mediana > 20 & clouds_mediana <= 50 ~ 0.8,
        clouds_mediana > 50 ~ 0.5
      ),
      rain_score = ifelse(rain == "Yes", -1, 1),
      snow_score = ifelse(snow == "Yes", 0.7, 1),
      humidity_score = case_when(
        humidity_mediana >= 40 & humidity_mediana <= 60 ~ 1.0,
        humidity_mediana > 60 & humidity_mediana <= 70 ~ 0.7,
        TRUE ~ -0.2
      ),
      total_score = 0.1 * wind_score +
        0.15 * temp_score +
        0.3 * rain_score +
        0.05 * snow_score +
        0.1 * cloud_score +
        0.05 * humidity_score
    )
}




#Logika wyboru funkcji
apply_scoring_function <- function(data, trip_type) {
  if (trip_type == "Romantic") {
    return(calculate_romantic_score(data))
  } else if (trip_type == "Cultural") {
    return(calculate_cultural_score(data))
  } else if (trip_type == "Photographic") {
    return(calculate_photographic_score(data))
  } else if (trip_type == "Family Trip") {
    return(calculate_family_trip_score(data))
  } else if (trip_type == "Active Winter") {
    return(calculate_active_winter_score(data))
  } else {
    stop("Nieznany typ wyjazdu.")
  }
}


# UI aplikacji
ui <- fluidPage(
  titlePanel("GoFindSun - Wybierz najlepszą pogodę!"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "selected_date",
        "Wybierz zakres dat:",
        start = min(forecast_summary$date),
        end = max(forecast_summary$date),
        min = min(forecast_summary$date),
        max = max(forecast_summary$date)
      ),
      selectInput(
        "trip_type",
        "Wybierz typ wyjazdu:",
        choices = c("Romantic", "Cultural", "Photographic", "Family Trip", "Active Winter"),
        selected = "Romantic"
      ),
      actionButton("find_weather", "Znajdź najlepsze miejsce")
    ),
    mainPanel(
      leafletOutput("map"),         # Mapa z wybraną lokalizacją
      textOutput("no_result"),      # Informacja o braku wyników
      DTOutput("score_table")       # Tabela z rankingiem lokalizacji
    )
  )
)



# Serwer aplikacji
server <- function(input, output, session) {
  observeEvent(input$find_weather, {
    # Filtrowanie danych według zakresu dat
    filtered_data <- forecast_summary %>%
      filter(date >= input$selected_date[1], date <= input$selected_date[2])
    
    if (nrow(filtered_data) == 0) {
      # Brak wyników
      output$no_result <- renderText("Brak wyników spełniających zadane kryteria.")
      output$map <- renderLeaflet({
        leaflet() %>% addTiles()
      })
      output$score_table <- renderDT(NULL)
    } else {
      # Zastosowanie funkcji obliczania punktacji na podstawie typu wyjazdu
      scored_data <- apply_scoring_function(filtered_data, input$trip_type)
      
      # Obliczanie średnich punktacji dla każdego miasta
      avg_scores <- scored_data %>%
        group_by(city) %>%
        summarise(mean_score = mean(total_score, na.rm = TRUE)) %>%
        arrange(desc(mean_score))
      
      # Wybór najlepszej lokalizacji
      best_match <- scored_data %>%
        filter(city == avg_scores$city[1]) %>%
        slice(1)  # Wybierz pierwszy wiersz z najwyższą punktacją
      
      # Wyświetlenie najlepszego miejsca na mapie
      output$no_result <- renderText(paste("Najlepsze miejsce to:", best_match$city))
      output$map <- renderLeaflet({
        leaflet(data = best_match) %>%
          addTiles() %>%
          addMarkers(
            lng = ~as.numeric(lon),
            lat = ~as.numeric(lat),
            popup = ~paste(
              city, ": ", 
              "Śr. temp.: ", temp_mediana, "°C, Zachmurzenie: ", clouds_mediana, "%, ",
              "Wiatr: ", wind_speed_max, "m/s, Deszcz: ", rain, ", Śnieg: ", snow
            )
          )
      })
      
      # Wyświetlenie tabeli z rankingiem
      output$score_table <- renderDT({
        avg_scores %>%
          mutate(Lp = row_number()) %>%
          select(Lp, City = city, Mean_Score = mean_score) %>%
          datatable(options = list(pageLength = 5, autoWidth = TRUE))
      })
    }
  })
}



# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)