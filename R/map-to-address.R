# Load required libraries
library(tidyverse)
library(fs)
library(sf)
library(httr)
library(jsonlite)
library(knitr)

# List all .osm files in the current directory and subdirectories
osm_file_paths <- dir_ls(".", recurse = TRUE, regexp = ".*[.]osm")

# Calculate the centroid of a map from an OSM file
calculate_map_centroid <- function(file_path) {
  map_data <- suppressWarnings(st_read(file_path, quiet = TRUE))

  # Aggregate geometries and calculate the centroid
  map_data$geometry |>
    st_union() |>
    st_centroid() |>
    st_coordinates()
}

# Perform reverse geocoding using OpenStreetMap's Nominatim API
perform_reverse_geocoding <- function(longitude, latitude, language = "en") {
  # Define the API endpoint
  api_url <- "https://nominatim.openstreetmap.org/reverse"

  # Send GET request to the API
  response <- GET(
      api_url,
      query = list(
        lon               = longitude,
        lat               = latitude,
        format            = "json",
        addressdetails    = 1,
        `accept-language` = language
      )
    ) |>
    content("text", encoding = "UTF-8") |>
    fromJSON()

  # Extract relevant address components
  list(
    city    = response$address$city,
    county  = response$address$county,
    state   = response$address$state,
    country = response$address$country
  )
}

# Extract address information from an OSM file
extract_address_information <- function(file_path) {
  # Calculate the center point of the map
  center_coordinates <- calculate_map_centroid(file_path)

  # Perform reverse geocoding for English and Japanese
  address_english <- perform_reverse_geocoding(
    longitude = center_coordinates[1],
    latitude  = center_coordinates[2],
    language  = "en"
  )
  address_japanese <- perform_reverse_geocoding(
    longitude = center_coordinates[1],
    latitude  = center_coordinates[2],
    language  =  "ja"
  )

  tibble(
    file_path  = file_path,
    center_lon = center_coordinates[1],
    center_lat = center_coordinates[2],
    city_en    = address_english$city,
    county_en  = address_english$county,
    state_en   = address_english$state,
    country_en = address_english$country,
    city_ja    = address_japanese$city,
    county_ja  = address_japanese$county,
    state_ja   = address_japanese$state,
    country_ja = address_japanese$country,
    google_map = str_c(
      "[Map](https://www.google.co.jp/maps/@",
      center_coordinates[2], ",", center_coordinates[1], ",12z)"
    )
  )
}

# Process OSM files
processed_addresses <- osm_file_paths |>
  map_dfr(extract_address_information)
processed_addresses <- processed_addresses |>
  relocate(
    file_path,
    center_lon, center_lat,
    city_en, county_en, state_en, country_en,
    city_ja, county_ja, state_ja, country_ja,
    google_map,
  )

# Save the result as a markdown file
processed_addresses |>
  kable() |>
  as.character() |>
  paste(collapse = "\n") |>
  write_file(file = "map-address.md")
