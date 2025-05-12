library(tidycensus)
library(mapgl)

# load_variables(2023, "acs1/profile") |> View()

births_per_1000 <- get_acs(
  geography = "puma",
  year = 2023,
  variables = "DP02_0040",
  survey = "acs5",
  geometry = TRUE
)

popup_content <- glue::glue(
  "<strong>{births_per_1000$NAME}</strong><br>",
  "Rate per 1,000 women age 15-50: {births_per_1000$estimate}"
)

births_per_1000$popup <- popup_content

r <- range(births_per_1000$estimate, na.rm = TRUE)

birth_rate_map <- mapboxgl(
  style = mapbox_style("standard"),
  center = c(-98.5795, 39.8283),
  zoom = 4,
  config = list(
    basemap = list(
      theme = "monochrome",
      font = "Montserrat"
    )
  )
) |>
  add_fill_layer(
    id = "pumas",
    source = births_per_1000,
    fill_color = interpolate(
      column = "estimate",
      values = c(0, 25, 50, 75, 100),
      stops = RColorBrewer::brewer.pal(5, "PuRd"),
      na_color = "lightgrey"
    ),
    fill_opacity = 0.7,
    popup = "popup",
    hover_options = list(
      fill_color = "cyan",
      fill_opacity = 1
    )
  ) |>
  add_legend(
    "<span style='font-family: Arial, sans-serif; display: block;'><span style='font-weight: bold; font-size: 14px; color: #333; display: block; margin-bottom: 4px;'>Number of women age 15-50<br>who gave birth in past year</span><span style='font-size: 12px; color: #666;'>Per 1,000 women • PUMAs • 2023 5-year ACS</span></span>",
    values = c(0, 25, 50, 75, "100+"),
    colors = RColorBrewer::brewer.pal(5, "PuRd"),
    width = 400
  )

htmlwidgets::saveWidget(birth_rate_map, "happy-mothers-day/index.html", selfcontained = FALSE, title = "Happy Mother's Day")
