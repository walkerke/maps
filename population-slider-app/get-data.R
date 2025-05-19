library(tidycensus)
library(tidyverse)

pop_data <- get_estimates(
    geography = "county",
    variables = "POPESTIMATE",
    vintage = 2024,
    geometry = TRUE,
    resolution = "5m"
) |>
    shift_geometry(position = "outside") |>
    mutate(NAME = iconv(NAME, to = "UTF-8", sub = ""))

ctys <- pop_data |>
    arrange(desc(value)) |>
    mutate(
        cume_pct = (cumsum(value) / sum(value)) * 100
    ) |>
    select(-variable, -year)

write_rds(ctys, "population-slider-app/county_data.rds")
