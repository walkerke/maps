library(shiny)
library(mapgl)
library(sf)
library(dplyr)

ui <- fluidPage(
  # Remove default padding/margin for full screen map
  tags$head(
    tags$style(HTML(
      "
      body, .container-fluid {
        padding: 0;
        margin: 0;
      }
      #map {
        position: absolute;
        top: 0;
        bottom: 0;
        width: 100%;
      }
    "
    ))
  ),

  # Full screen map
  mapboxglOutput("map", height = "100vh"),

  # Floating panel in top-left
  absolutePanel(
    top = 20,
    left = 20,
    width = 250,
    style = "background-color: rgba(255, 255, 255, 0.85); padding: 15px; border-radius: 5px;",

    h4("Map Projection Demo"),
    selectInput(
      "projection",
      "Choose Projection:",
      choices = c(
        "Mercator" = "mercator",
        "Natural Earth" = "naturalEarth",
        "Winkel Tripel" = "winkelTripel",
        "Equal Earth" = "equalEarth",
        "Equirectangular" = "equirectangular",
        "Globe" = "globe"
      ),
      selected = "mercator"
    ),
    p(
      "Hover to see how a 500km circle distorts across different projections.",
      style = "font-size: 12px; color: #666;"
    )
  )
)

server <- function(input, output, session) {
  # Create initial placeholder circle (tiny, won't be visible)
  placeholder_circle <- st_point(c(0, 0)) |>
    st_sfc(crs = 4326) |>
    st_sf() |>
    st_buffer(0.0001) |>
    mutate(id = 1)

  # Render initial map
  output$map <- renderMapboxgl({
    mapboxgl(
      zoom = 1.5,
      center = c(0, 33),
      projection = "mercator",
      hash = TRUE
    ) |>
      add_fill_layer(
        id = "hover_circle",
        source = placeholder_circle,
        fill_color = "magenta",
        fill_opacity = 0.4
      ) |>
      enable_shiny_hover(coordinates = TRUE, features = FALSE)
  })

  # Update projection when changed
  observeEvent(input$projection, {
    mapboxgl_proxy("map") |>
      set_projection(input$projection)
  })

  # Update circle on hover
  observeEvent(input$map_hover, {
    hover_data <- input$map_hover

    if (!is.null(hover_data$lng) && !is.null(hover_data$lat)) {
      # Create 500km buffer circle
      tryCatch(
        {
          hover_circle <- st_point(c(hover_data$lng, hover_data$lat)) |>
            st_sfc(crs = 4326) |>
            st_sf() |>
            st_buffer(500000) |> # 500km in meters
            mutate(id = 1)

          # Update the circle layer
          mapboxgl_proxy("map") |>
            set_source(
              layer_id = "hover_circle",
              source = hover_circle
            )
        },
        error = function(e) {
          # Silently handle errors (e.g., at extreme latitudes)
        }
      )
    }
  })
}

shinyApp(ui, server)
