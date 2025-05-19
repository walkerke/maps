# app.R - Simple US Counties Population Visualization with Mobile Support
# Load required packages
library(shiny)
library(mapgl)
library(readr)
library(sf)

# Load county data
ctys <- read_rds("county_data.rds")

# Define map style
style <- list(
  version = 8,
  sources = structure(list(), .Names = character(0)),
  layers = list(
    list(
      id = "background",
      type = "background",
      paint = list(
        `background-color` = "lightgrey"
      )
    )
  )
)

# Define UI
ui <- fluidPage(
  # Add viewport meta tag for responsive design
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$style(HTML("
      body, html {
        margin: 0;
        padding: 0;
        height: 100vh;
        overflow: hidden;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      }

      /* Make container fill entire screen */
      .container-fluid {
        padding: 0 !important;
        height: 100vh;
      }

      /* Full-screen map */
      #map, .mapboxgl-map {
        position: absolute !important;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 1;
      }

      /* Overlay panel for controls */
      .overlay-panel {
        position: absolute;
        top: 20px;
        left: 0;
        right: 0;
        z-index: 10;
        text-align: center;
      }

      /* Panel styling with fixed dimensions */
      .panel-content {
        display: inline-block;
        background-color: rgba(255, 255, 255, 0.85);
        padding: 20px 30px;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        width: 600px;
        margin: 0 auto;
      }

      /* Title container with relative positioning */
      .title-container {
        position: relative;
        height: 70px;
        margin-bottom: 15px;
      }

      /* Special layout trick: two fixed position divs */
      .left-text {
        position: absolute;
        top: 50%;
        left: 0;
        transform: translateY(-50%);
        width: 40%;
        text-align: right;
        font-size: 1.6rem;
        font-weight: 600;
        padding-right: 8px;
      }

      .right-text {
        position: absolute;
        top: 50%;
        right: 0;
        transform: translateY(-50%);
        width: 40%;
        text-align: left;
        font-size: 1.6rem;
        font-weight: 600;
        padding-left: 15px;
        transition: padding-left 0.2s ease;
      }

      /* Center percentage with absolute positioning */
      .percentage-container {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 20%;
      }

      .percentage-value {
        color: #f97316;
        font-weight: bold;
        font-size: 3.5rem;
        line-height: 1;
        text-align: center;
      }

      /* Slider container */
      .slider-container {
        width: 500px;
        margin: 0 auto;
      }

      /* Slider styling */
      .irs--shiny .irs-bar {
        background-color: #f97316;
        border-top: 1px solid #f97316;
        border-bottom: 1px solid #f97316;
        height: 10px;
        top: 25px;
      }

      .irs--shiny .irs-line {
        height: 10px;
        top: 25px;
      }

      .irs--shiny .irs-handle {
        border: 2px solid #f97316;
        background-color: white;
        width: 22px;
        height: 22px;
        top: 19px;
      }

      .irs--shiny .irs-single {
        background-color: #f97316;
      }

      /* Hide grid ticks and values */
      .irs-grid, .irs-min, .irs-max {
        display: none !important;
      }

      /* Style the play button */
      .slider-animate-container {
        text-align: center;
        margin-top: 15px;
      }

      .slider-animate-button {
        transform: scale(1.3);
      }

      .play-pause-button {
        color: white !important;
        background-color: #f97316 !important;
        border-color: #ea580c !important;
        font-weight: bold !important;
        padding: 6px 12px !important;
      }

      .play-pause-button:hover {
        background-color: #ea580c !important;
        border-color: #c2410c !important;
        opacity: 1 !important;
      }

      /* County counter panel in bottom right */
      .county-counter {
        position: absolute;
        bottom: 25px;
        right: 25px;
        background-color: rgba(255, 255, 255, 0.9);
        padding: 15px 20px;
        border-radius: 8px;
        box-shadow: 0 3px 10px rgba(0, 0, 0, 0.15);
        font-size: 1.4rem;
        z-index: 10;
      }

      .counter-number {
        font-weight: bold;
        color: #f97316;
        font-size: 1.5rem;
      }

      /* Data source attribution in bottom left */
      .data-source {
        position: absolute;
        bottom: 40px;
        left: 10px;
        background-color: rgba(255, 255, 255, 0.9);
        padding: 10px 15px;
        border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        font-size: 0.9rem;
        z-index: 10;
        color: #555;
      }

      .data-source a {
        color: #f97316;
        text-decoration: none;
        font-weight: 500;
        transition: color 0.2s ease;
      }

      .data-source a:hover {
        color: #ea580c;
        text-decoration: underline;
      }

      /* Mobile Responsive Design */
      @media (max-width: 768px) {
        /* Adjust panel width for mobile */
        .panel-content {
          width: 90%;
          padding: 15px;
        }

        /* Adjust slider width for mobile */
        .slider-container {
          width: 100%;
        }

        /* Adjust title for mobile - stack vertically */
        .title-container {
          height: auto;
          margin-bottom: 10px;
          display: flex;
          flex-direction: column;
          align-items: center;
        }

        /* Reset absolute positioning for mobile */
        .left-text, .right-text, .percentage-container {
          position: static;
          transform: none;
          width: 100%;
          text-align: center;
          padding: 0;
          margin-bottom: 5px;
        }

        .percentage-value {
          font-size: 3rem;
          margin: 5px 0;
        }

        .left-text, .right-text {
          font-size: 1.2rem;
        }

        /* Adjust info panels for mobile */
        .county-counter, .data-source {
          padding: 8px 12px;
          font-size: 0.9rem;
        }

        .counter-number {
          font-size: 1.1rem;
        }

        /* Adjust positions for mobile */
        .county-counter {
          bottom: 15px;
          right: 15px;
        }

        .data-source {
          bottom: 15px;
          left: 15px;
          max-width: 45%;
        }
      }

      /* Extra small devices */
      @media (max-width: 480px) {
        .percentage-value {
          font-size: 2.5rem;
        }

        .left-text, .right-text {
          font-size: 1rem;
        }

        /* Further adjust info panels for very small screens */
        .county-counter, .data-source {
          padding: 5px 8px;
          font-size: 0.8rem;
        }

        .counter-number {
          font-size: 0.9rem;
        }
      }
    "))
  ),

  # Additional JS to style the play button and adjust text spacing (with mobile detection)
  tags$script(HTML("
    $(document).ready(function() {
      // Style the animation button
      $('.slider-animate-button').addClass('play-pause-button');

      // Monitor for button recreation (when state changes)
      const observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (mutation.addedNodes.length) {
            $('.slider-animate-button').addClass('play-pause-button');
          }
        });
      });

      // Start observing
      observer.observe(document.querySelector('.slider-animate-container'), {
        childList: true,
        subtree: true
      });

      // Function to check if device is mobile
      function isMobile() {
        return window.innerWidth <= 768;
      }

      // Function to adjust right text position based on percentage value
      function adjustRightTextPosition(percentValue) {
        // Skip adjustment on mobile - we use stacked layout there
        if (isMobile()) return;

        const rightText = $('.right-text');
        percentValue = parseFloat(percentValue);

        if (percentValue < 10) {
          // Single digit - move text closer
          rightText.css('padding-left', '5px');
        } else if (percentValue < 100) {
          // Double digit - standard spacing
          rightText.css('padding-left', '8px');
        } else {
          // Triple digit - move text further
          rightText.css('padding-left', '20px');
        }
      }

      // Initial adjustment
      adjustRightTextPosition($('.percentage-value').text());

      // Monitor for changes in the percentage value
      const percentObserver = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (mutation.type === 'childList') {
            const percentText = $('.percentage-value').text();
            adjustRightTextPosition(percentText);
          }
        });
      });

      // Start observing percentage changes
      percentObserver.observe(document.querySelector('.percentage-value'), {
        childList: true,
        subtree: true
      });

      // Handle window resize events to adjust layout
      $(window).resize(function() {
        adjustRightTextPosition($('.percentage-value').text());
      });
    })")
  ),

  # Map container
  mapboxglOutput("map", height = "100%"),

  # Overlay panel with title and slider
  div(
    class = "overlay-panel",
    div(
      class = "panel-content",
      # Title container with 3-column absolute layout
      div(
        class = "title-container",
        # Left text (fixed position on desktop, stacked on mobile)
        div(class = "left-text", "These Counties Represent"),

        # Center percentage (fixed position on desktop, stacked on mobile)
        div(
          class = "percentage-container",
          div(class = "percentage-value", textOutput("percentageValue"))
        ),

        # Right text (position will be adjusted by JavaScript on desktop, stacked on mobile)
        div(class = "right-text", "of the US Population")
      ),

      # Slider with fixed width
      div(
        class = "slider-container",
        sliderInput(
          "percentSlider",
          label = NULL,
          min = 2.9,
          max = 100,
          value = 2.9,
          step = 0.1,
          width = "100%",
          animate = animationOptions(
            interval = 60
          )
        )
      )
    )
  ),

  # County counter in bottom right
  div(
    class = "county-counter",
    "Showing ",
    span(textOutput("countiesCount", inline = TRUE), class = "counter-number"),
    " of 3,144 counties"
  ),

  # Data source attribution in bottom left
  div(
    class = "data-source",
    "Data source: ",
    a(
      "2024 US Census Bureau Population Estimates",
      href = "https://www.census.gov/data/datasets/time-series/demo/popest/2020s-counties-total.html",
      target = "_blank"
    )
  ),

  # Add JavaScript for real-time slider updates
  tags$script(HTML("
    $(document).ready(function() {
      // Add change event listener for real-time updates during dragging
      var sliderEl = $('#percentSlider');
      var ionSlider = sliderEl.data('ionRangeSlider');

      if (ionSlider) {
        ionSlider.update({
          onChange: function(data) {
            // Update during slider drag
            Shiny.setInputValue('percentSlider', data.from);
          }
        });
      }
    });"))
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the counties count
  counties_shown <- reactiveVal(0)

  # Format percentage for display
  output$percentageValue <- renderText({
    paste0(format(round(input$percentSlider, 1), nsmall = 1), "%")
  })

  # Render map
  output$map <- renderMapboxgl({
    mapboxgl(
      style = style,
      projection = "albers",
      center = c(-96.78, 42.32),
      zoom = 3.72,
      hash = TRUE
    ) |>
      add_fill_layer(
        id = "counties",
        source = ctys,
        fill_opacity = 1,
        fill_color = "white"
      ) |>
      add_fill_layer(
        id = "counties_shown",
        source = ctys,
        fill_color = "orange",
        fill_outline_color = "black",
        tooltip = "NAME",
        filter = list("<=", get_column("cume_pct"), 2.9),
        hover_options = list(
          fill_color = "cyan"
        )
      )
  })

  # Calculate counties count efficiently when slider changes
  observe({
    # Get current percentage
    current_pct <- input$percentSlider

    # Calculate counties count
    count <- sum(ctys$cume_pct <= current_pct)

    # Update reactive value
    counties_shown(count)
  }) |>
    bindEvent(input$percentSlider)

  # Display counties count
  output$countiesCount <- renderText({
    format(counties_shown(), big.mark = ",")
  })

  # Update map when slider changes
  observe({
    mapboxgl_proxy("map") |>
      set_filter(
        "counties_shown",
        list("<=", get_column("cume_pct"), input$percentSlider)
      )
  }) |>
    bindEvent(input$percentSlider)
}

# Run the application
shinyApp(ui = ui, server = server)
