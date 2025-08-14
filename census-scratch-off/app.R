library(shiny)
library(mapgl)
library(sf)
library(keys)
library(tidyverse)
library(tigris)
library(tidycensus)
library(shinybusy)
options(tigris_use_cache = TRUE)

# Format census labels for HTML map legends
format_census_label <- function(label) {
  # Remove the Estimate/Percent prefix and split by !!
  parts <- label %>%
    str_split("!!", simplify = TRUE) %>%
    str_squish()

  # Identify the type (Estimate or Percent)
  type <- parts[1]
  is_percent <- str_detect(type, "^Percent")

  # Remove the type from parts
  parts <- parts[-1]
  n_parts <- length(parts)

  if (n_parts == 0) {
    return(label)
  } # Safety check

  # The last part is the main title
  main_title <- parts[n_parts]

  # Build subtitle from remaining parts (if any)
  subtitle_parts <- character()

  # Add the hierarchical context (skip the broadest category if it's redundant)
  if (n_parts >= 2) {
    # Take the second-to-last part as the immediate parent
    subtitle_parts <- c(subtitle_parts, parts[n_parts - 1])
  }

  # Add the measure type
  subtitle_parts <- c(subtitle_parts, ifelse(is_percent, "Percent", "Count"))

  # Combine subtitle with bullet separator
  subtitle <- paste(subtitle_parts, collapse = " • ")

  # Create HTML formatted string
  html_label <- paste0(
    "<b style='font-size: 1.1em;'>",
    main_title,
    "</b>",
    "<br>",
    "<span style='font-size: 0.85em; color: #666;'>",
    subtitle,
    "</span>"
  )

  return(html_label)
}

state <- states(cb = TRUE, resolution = "20m") |>
  filter(STUSPS != "PR")

var_choices <- load_variables(2023, "acs5/profile") |>
  filter(!str_detect(name, "PR"))

# Function to get random row
var_choice <- function() {
  sample_n(var_choices, 1)
}

ui <- fluidPage(
  useKeys(),
  keysInput("hotkeys", "s"),

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

  maplibreOutput("map", height = "100vh"),

  absolutePanel(
    top = 20,
    left = 20,
    width = 200,
    draggable = TRUE,
    style = "background-color: rgba(255, 255, 255, 0.9); padding: 15px; border-radius: 5px;",

    h4("Census Scratch-Off"),
    p("'Scratch off' the top layer to reveal a surprise map of ACS data!"),
    actionButton("start_scratching", "Start Scratching!"),
    br(),
    br(),
    p(HTML("<strong>Press 'S' on your keyboard to toggle scratch mode</strong>")),
    selectInput(
      "state",
      label = "Switch state to: ",
      choices = sort(state$NAME),
      selected = "Oregon"
    )
  )
)

server <- function(input, output, session) {
  # Track scratching state
  scratching <- reactiveVal(FALSE)
  # Track if legend has been added
  legend_added <- reactiveVal(FALSE)

  state_mask <- reactive({
    state |> dplyr::filter(NAME == input$state)
  })

  tract_data <- reactive({
    # Show spinner during data fetch
    show_modal_spinner(
      text = "Fetching surprise data with tidycensus...",
      spin = "breeding-rhombus"
    )

    # Get a random variable (will change when state changes)
    var <- var_choice()

    data <- get_acs(
      geography = "tract",
      variables = var$name,
      state = input$state,
      geometry = TRUE,
      key = Sys.getenv("CENSUS_API_KEY")
    )

    label <- format_census_label(var$label)

    expr <- interpolate_palette(
      data = data,
      column = "estimate"
    )

    # Hide spinner when done
    remove_modal_spinner()

    list(
      data = data,
      label = label,
      expr = expr
    )
  })

  output$map <- renderMaplibre({
    # We want to redraw the map on state change
    dat <- tract_data()

    # Reset states when map re-renders
    scratching(FALSE)
    legend_added(FALSE)
    updateActionButton(session, "start_scratching", label = "Start Scratching")

    maplibre(
      style = carto_style("positron"),
      bounds = state_mask()
    ) |>
      add_fill_layer(
        id = "tracts",
        source = dat$data,
        fill_color = dat$expr$expression,
        fill_opacity = 0.5
      ) |>
      add_fill_layer(
        id = "top_layer",
        source = state_mask(),
        fill_color = "darkgray",
        fill_opacity = 1
      ) |>
      # Initialize empty scratch buffer
      turf_buffer(
        coordinates = c(0, 0),
        radius = 0,
        units = "kilometers",
        source_id = "scratch_buffer"
      ) |>
      # Visualize the scratch
      add_fill_layer(
        id = "scratch_visual",
        source = "scratch_buffer",
        fill_color = "white",
        fill_opacity = 0.5,
        fill_outline_color = "black"
      ) |>
      # Enable hover tracking
      enable_shiny_hover(coordinates = TRUE, features = FALSE)
  })

  # Function to toggle scratching mode
  toggle_scratching <- function() {
    current <- scratching()
    scratching(!current)

    # Update button text
    if (!current) {
      updateActionButton(session, "start_scratching", label = "Stop Scratching")

      # Add legend on first scratch
      if (!legend_added()) {
        dat <- tract_data()
        legend_colors <- get_legend_colors(dat$expr)
        legend_labels <- get_legend_labels(dat$expr)

        maplibre_proxy("map") |>
          add_legend(
            legend_title = dat$label,
            values = legend_labels,
            colors = legend_colors,
            width = "300px",
            position = "bottom-left"
          )

        legend_added(TRUE)
      }
    } else {
      updateActionButton(
        session,
        "start_scratching",
        label = "Start Scratching"
      )
    }
  }

  # Toggle scratching mode (button)
  observeEvent(input$start_scratching, {
    toggle_scratching()
  })

  # Toggle scratching mode (hotkey)
  observeEvent(input$hotkeys, {
    if (input$hotkeys == "s") {
      toggle_scratching()
    }
  })

  # Update scratch position on hover
  observeEvent(input$map_hover, {
    if (!is.null(input$map_hover$lng)) {
      # Always update scratch position
      proxy <- maplibre_proxy("map") |>
        turf_buffer(
          coordinates = c(input$map_hover$lng, input$map_hover$lat),
          radius = 10,
          units = "miles",
          source_id = "scratch_buffer"
        )

      # Only perform scratching when in scratching mode
      if (scratching()) {
        proxy |>
          turf_difference(
            layer_id = "top_layer",
            layer_id_2 = "scratch_buffer",
            source_id = "top_layer" # Same as top layer source - should update it!
          )
      }
    }
  })
}

shinyApp(ui, server)
