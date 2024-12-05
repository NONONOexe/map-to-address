library(tidyverse)
library(knitr)

lines <- read_lines("map-addresses.md")
table_lines <- lines[str_detect(lines, "\\|")]
table_df <- table_lines |>
  str_remove_all("^\\|\\s*|\\s*\\|$") |>
  str_split("\\s*\\|\\s*", simplify = TRUE) |>
  as_tibble()
names(table_df) <- table_df[1,]
table_df <- table_df[-(1:2),]

table_df |>
  select(file_path, center_lon, center_lat) |>
  mutate(
    osm        = str_glue(
      "[OSM](https://www.openstreetmap.org/?mlat={center_lon}&mlon={center_lat})"
    ),
    google_map = str_glue(
      "[Google Map](https://www.google.com/maps?q={center_lon},{center_lat})"
    )
  ) |>
  kable() |>
  as.character() |>
  paste(collapse = "\n") |>
  write_file(file = "map-address-coodinates.md")
