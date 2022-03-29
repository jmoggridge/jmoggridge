
setHook("rstudio.sessionInit", function(newSession) {
  # any code included here will be run at the start of each RStudio session
  glue <- glue::glue
  # cat("\014") or cat('\f') will automatically clear the console and therefor the R startup message
  cat("\014")
  
  apikey <- Sys.getenv('WEATHER_API_KEY')
  postal_code <- Sys.getenv('MY_POSTAL_CODE')
  ## weather API
  url <-
    glue("http://api.weatherapi.com/v1/current.json?key={apikey}&q={postal_code}&aqi=yes")
  response <- httr::GET(url)
  content <- httr::content(response)
  
  em <- function(x) {
    emojifont::emoji(x)
  }
  
  condition_emoji <- function(c) {
    
    ## emojii
    w <- list(
      sun_behind_small_cloud = c("Partly cloudy"),
      cloud = c("Cloudy", "Overcast", "Mist", "Freezing fog"),
      cloud_with_rain = c("Light drizzle",
                          "Light rain",
                          "Light rain shower"),
      sun_behind_rain_cloud = c(
        "Patchy rain possible",
        "Patchy light drizzle",
        "Patchy light rain"
      ),
      cloud_with_lightning_and_rain = c("Patchy light rain with thunder"),
      rain2 = c(
        "Moderate rain at times",
        "Moderate rain",
        "Heavy rain at times",
        "Heavy rain",
        "Moderate or heavy rain shower",
        "Torrential rain shower",
        "Moderate or heavy rain with thunder"
      ),
      cloud_with_snow = c(
        "Patchy snow possible",
        "Patchy light snow",
        "Light snow",
        "Light snow showers",
        "Patchy light snow with thunder",
        "Patchy moderate snow",
        "Moderate snow"
      ),
      snow2 = c(
        "Patchy heavy snow",
        "Heavy snow",
        "Blowing snow",
        "Blizzard",
        "Moderate or heavy snow showers",
        "Moderate or heavy snow with thunder"
      ),
      sleet = c(
        "Patchy sleet possible",
        "Freezing drizzle",
        "Heavy freezing drizzle",
        "Light freezing rain",
        "Moderate or heavy freezing rain",
        "Light sleet",
        "Moderate or heavy sleet",
        "Light sleet showers",
        "Moderate or heavy sleet showers"
      ),
      ice_pellets = c(
        "Ice pellets",
        "Light showers of ice pellets",
        "Moderate or heavy showers of ice pellets"
      )
    )
    
    em <- function(x) {
      emojifont::emoji(x)
    }
    
    set_emoji <- dplyr::case_when(
      c %in% "Sunny" ~ 
        em('sunny'),
      c %in% "Clear" ~ 
        em("crescent_moon"),
      c %in% w$cloud ~ 
        em('cloud'),
      c %in% w$rain1 ~ 
        em('umbrella'),
      c %in% w$sun_behind_small_cloud ~ 
        em('sun_behind_small_cloud'),
      c %in% w$sun_behind_rain_cloud ~ 
        em('sun_behind_rain_cloud'),
      c %in% w$cloud_with_lightning_and_rain ~ 
        em('cloud_with_lightning_and_rain'),
      c %in% w$cloud_with_snow ~ 
        em('cloud_with_snow'),
      c %in% w$snow2 ~ 
        paste0(rep(em('snowman'), 3), collapse = ''),
      c %in% w$ice_pellets ~  
        paste0(em('snowflake'), em('basketball'), collapse = ''),
      TRUE ~ 
        em("sunrise_over_mountains")
    )
    return(glue::glue('{set_emoji} Condition: {c}'))
  }
  
  # create weather strings with emoji
  current <- content$current
  temp <- glue("|{em('thermometer')} Temperature: {current$temp_c}˚C ({current$temp_f}˚F), fl: {current$feelslike_c}˚C")
  condition <- condition_emoji(c = current$condition$text)
  
  precip_emoji <- ifelse(
    current$precip_mm > 0, 
    yes = em('open_umbrella'),
    no = em('closed_umbrella')
  )
  precip <- glue('{precip_emoji} Precip: {content$current$precip_mm} mm')
  wind <- glue("{em('wind_face')} Wind: {current$wind_kph} kph {current$wind_dir}")
  pressure <- glue("Pressure: {current$pressure_mb} millibar")
  # air quality
  air <- purrr::map(content$current$air_quality, ~round(.x, 4))
  air_qual1 <- glue(
    "CO: {air$co} \tNO2: {air$no2} \t03: {air$o3} \tSO2: {air$so2}")
  air_qual2 <- glue(
    "pm2.5: {air$pm2_5} \tpm10: {air$pm10} \t EPA index: 1"
  )
  humidity <- glue("Humidity: {current$humidity}")
  ## display
  cat('------------------------------------------------------\n')
  # weather and date
  cat(date(), '\n')
  cat(crayon::bold(crayon::blurred("Weather")), temp, '\n')
  cat('\t|', condition, '\t', precip,'\n')
  cat('\t|', wind, '\t', crayon::italic('p '), pressure, '\n')
  cat('\t|', humidity, '\n')
  cat(crayon::bold(crayon::blurred("Air quality \n")))
  cat('\t|', air_qual1, '\n')
  cat('\t|', air_qual2, '\n')
  cat('------------------------------------------------------\n')
  # create joke/comic
  sample(mitchhedberg::jokes, 1) |> 
    stringr::str_trim() |>
    stringr::str_wrap(60) |>
    # add to art
    cowsay::say(
      by = "signbunny",
      what_color = randomcoloR::randomColor(),
      by_color = randomcoloR::randomColor()
    )
  cat('---------------------------------------------\n')
  
  
}, action = "append")

## this is the command line prompt
# if (interactive()) prompt::set_prompt(prompt::prompt_fancy)

