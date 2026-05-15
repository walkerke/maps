# Split the ACS-backed place GeoJSON into state-level static assets.
#
# Run from this directory:
# Rscript split-acs-data.R

library(dplyr)
library(sf)

infile <- "us_places_2024_acs.geojson"
out_dir <- "data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

state_meta <- data.frame(
  state = c(state.name, "District of Columbia"),
  abbr = c(state.abb, "DC"),
  fips = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
    "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
    "46", "47", "48", "49", "50", "51", "53", "54", "55", "56",
    "11"
  ),
  stringsAsFactors = FALSE
)

places <- st_read(infile, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326) |>
  mutate(
    population = as.numeric(.data$estimate),
    moe = as.numeric(.data$moe),
    place_type = if_else(.data$LSAD == "57", "CDP", "Incorporated place")
  ) |>
  transmute(
    GEOID = .data$GEOID,
    NAME = .data$NAME.x,
    NAMELSAD = .data$NAMELSAD,
    STUSPS = .data$STUSPS,
    STATE_NAME = .data$STATE_NAME,
    LSAD = .data$LSAD,
    place_type = .data$place_type,
    population = .data$population,
    moe = .data$moe,
    geometry = .data$geometry
  )

for (i in seq_len(nrow(state_meta))) {
  state_places <- places |>
    filter(substr(.data$GEOID, 1, 2) == state_meta$fips[[i]]) |>
    arrange(desc(.data$population), .data$NAME, .data$GEOID)

  message("Writing ", state_meta$abbr[[i]], " (", nrow(state_places), " places)")

  st_write(
    state_places,
    file.path(out_dir, sprintf("places-%s.geojson", state_meta$abbr[[i]])),
    driver = "GeoJSON",
    delete_dsn = TRUE,
    quiet = TRUE
  )
}

jsonlite::write_json(
  state_meta,
  file.path(out_dir, "states.json"),
  dataframe = "rows",
  auto_unbox = TRUE,
  pretty = TRUE
)
