library(shiny)
library(bslib)
library(mapgl)
library(sf)
library(dplyr)

# Directory for storing submissions
submissions_dir <- "dfw_submissions"

# Create directory if it doesn't exist
if (!dir.exists(submissions_dir)) {
  dir.create(submissions_dir)
}

# UI
ui <- page_fluid(
  # Custom CSS for floating sidebar
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$style(HTML("
      body { margin: 0; padding: 0; }
      .map-container {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        width: 100%;
        height: 100vh;
      }
      .floating-sidebar {
        position: absolute;
        top: 10px;
        left: 10px;
        right: 10px;
        width: auto;
        max-width: 320px;
        background: rgba(255, 255, 255, 0.35);
        backdrop-filter: blur(10px);
        -webkit-backdrop-filter: blur(10px);
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.15);
        z-index: 1000;
        max-height: calc(100vh - 20px);
        overflow-y: auto;
      }

      @media (min-width: 768px) {
        .floating-sidebar {
          top: 20px;
          left: 20px;
          right: auto;
          width: 280px;
          padding: 20px;
        }
      }

      /* Mobile header always visible on desktop */
      .mobile-header {
        display: none;
      }

      .toggle-btn {
        background: transparent;
        border: none;
        padding: 5px 10px;
        cursor: pointer;
        color: #333;
      }

      @media (max-width: 767px) {
        .floating-sidebar {
          position: fixed !important;
          bottom: 20px !important;
          top: auto !important;
          left: 10px !important;
          right: 10px !important;
          width: auto !important;
          max-width: none !important;
          max-height: 70vh !important;
          transition: all 0.3s ease;
        }

        .mobile-header {
          display: flex !important;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 0;
        }

        .mobile-title {
          margin: 0;
          font-size: 1.1rem;
        }

        /* Collapsed state */
        .floating-sidebar:not(.expanded) {
          max-height: 50px !important;
          overflow: hidden;
        }

        .floating-sidebar:not(.expanded) .sidebar-content {
          display: none;
        }

        /* Expanded state */
        .floating-sidebar.expanded {
          max-height: 70vh !important;
        }

        .floating-sidebar.expanded .sidebar-content {
          display: block;
          margin-top: 15px;
        }

        .floating-sidebar h5 {
          font-size: 0.95rem;
        }
        .floating-sidebar p, .floating-sidebar ol {
          font-size: 0.9rem;
        }
        .floating-sidebar ol {
          padding-left: 20px;
        }
        .btn {
          font-size: 0.9rem;
          padding: 8px 12px;
        }
        hr {
          margin: 10px 0;
        }
      }
      .btn-primary {
        background-color: #0066cc;
        border-color: #0066cc;
        color: white;
      }
      .btn-success {
        background-color: #28a745;
        border-color: #28a745;
        color: white;
      }
      .btn-warning {
        background-color: #ffc107;
        border-color: #ffc107;
        color: #212529;
      }
      h4 {
        margin-top: 0;
        color: #333;
      }
      .stats {
        background: rgba(0, 0, 0, 0.05);
        padding: 10px;
        border-radius: 4px;
        margin-top: 15px;
      }
    "))
  ),

  div(class = "map-container",
      maplibreOutput("map", width = "100%", height = "100%")
  ),

  div(class = "floating-sidebar", id = "sidebar",
      div(class = "mobile-header",
          h4(class = "mobile-title", "DFW Metro Boundary"),
          actionButton("toggle_panel", "",
                       icon = icon("chevron-down", class = "toggle-icon"),
                       class = "toggle-btn")
      ),

      div(class = "sidebar-content", id = "sidebar-content",
          p("Draw what YOU think is the Dallas-Fort Worth metro area boundary!"),
          hr(),

          h5("Instructions:"),
          tags$ol(
            tags$li("Use the polygon tool on the map to draw your boundary"),
            tags$li("Click 'Save My Boundary' when done"),
            tags$li("View everyone's submissions with 'Show All Responses'")
          ),

          hr(),

          actionButton("save_boundary",
                       "Save My Boundary",
                       class = "btn-primary",
                       width = "100%",
                       icon = icon("save")),
          br(), br(),

          actionButton("show_responses",
                       "Show All Responses",
                       class = "btn-success",
                       width = "100%",
                       icon = icon("layer-group")),
          br(), br(),

          actionButton("clear_responses",
                       "Hide All Responses",
                       class = "btn-warning",
                       width = "100%",
                       icon = icon("eye-slash")),

          div(class = "stats",
              h6("Statistics"),
              textOutput("submission_count")
          )
      ),

      tags$script(HTML("
        // Toggle panel on mobile
        $(document).on('click', '#toggle_panel', function(e) {
          e.preventDefault();
          $('#sidebar').toggleClass('expanded');
          $('.toggle-icon').toggleClass('fa-chevron-down fa-chevron-up');
        });

        // Initialize panel state based on screen size
        $(document).ready(function() {
          if (window.innerWidth < 768) {
            $('#sidebar').removeClass('expanded');
          }
        });
      "))
  )
)

# Server
server <- function(input, output, session) {

  # Reactive value to track if responses are shown
  responses_shown <- reactiveVal(FALSE)

  # Reactive to get submission count
  submission_count <- reactive({
    input$save_boundary  # Trigger on save
    input$show_responses  # Also trigger on show

    length(list.files(submissions_dir, pattern = "\\.rds$"))
  })

  # Render submission count
  output$submission_count <- renderText({
    count <- submission_count()
    paste("Total submissions:", count)
  })

  # Render the map
  output$map <- renderMaplibre({
    maplibre(style = maptiler_style("streets"), center = c( -97.0, 32.8), zoom = 8.5) |>
      add_draw_control(
        position = "top-right",
        point_color = "#0066cc",
        line_color = "#0066cc",
        fill_color = "#0066cc",
        fill_opacity = 0.3,
        active_color = "#ff6b6b",
        vertex_radius = 6,
        line_width = 3,
        controls = list(
          point = FALSE,
          line_string = FALSE,
          combine_features = FALSE,
          uncombine_features = FALSE
        )
      )
  })

  # Save boundary
  observeEvent(input$save_boundary, {
    # Get drawn features
    drawn_features <- get_drawn_features(maplibre_proxy("map"))

    if (!is.null(drawn_features) && nrow(drawn_features) > 0) {
      # Add timestamp to the features
      drawn_features$timestamp <- Sys.time()
      drawn_features$session_id <- session$token

      # Create unique filename using timestamp and session token
      filename <- paste0(
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        "_",
        substr(session$token, 1, 8),
        ".rds"
      )

      # Save to individual file
      saveRDS(drawn_features, file.path(submissions_dir, filename))

      showNotification(
        "Your boundary has been saved!",
        type = "default",
        duration = 3
      )

      # Clear the drawn features from the map
      # Note: This would require extending the draw control API
      # For now, users can manually delete their drawing

    } else {
      showNotification(
        "Please draw a boundary first!",
        type = "warning",
        duration = 3
      )
    }
  })

  # Show all responses
  observeEvent(input$show_responses, {
    submission_files <- list.files(submissions_dir, pattern = "\\.rds$", full.names = TRUE)

    if (length(submission_files) > 0) {
      # Read all submission files
      submissions <- lapply(submission_files, function(f) {
        tryCatch(readRDS(f), error = function(e) NULL)
      })

      # Remove any NULL entries (corrupted files)
      submissions <- submissions[!sapply(submissions, is.null)]

      if (length(submissions) > 0) {
        # Combine all submissions into one sf object
        all_boundaries <- dplyr::bind_rows(submissions)

        # Add to map as semi-transparent layer
        maplibre_proxy("map") |>
          # add_source(id = "all_submissions", data = all_boundaries) |>
          add_fill_layer(
            id = "submission_fills",
            source = all_boundaries,
            fill_color = "#28a745",
            fill_opacity = 0.08
          ) |>
          add_line_layer(
            id = "submission_lines",
            source = all_boundaries,
            line_color = "#28a745",
            line_opacity = 0.3,
            line_width = 1.5
          )

        responses_shown(TRUE)

        showNotification(
          paste("Showing", length(submissions), "submissions"),
          type = "default",
          duration = 3
        )
      } else {
        showNotification(
          "No submissions yet. Be the first!",
          type = "info",
          duration = 3
        )
      }
    } else {
      showNotification(
        "No submissions yet. Be the first!",
        type = "info",
        duration = 3
      )
    }
  })

  # Clear/hide responses from map
  observeEvent(input$clear_responses, {
    if (responses_shown()) {
      maplibre_proxy("map") |>
        set_layout_property("submission_fills", "visibility", "none") |>
        set_layout_property("submission_lines", "visibility", "none")

      responses_shown(FALSE)

      showNotification(
        "Responses hidden",
        type = "message",
        duration = 2
      )
    }
  })

}

# Run the app
shinyApp(ui, server)
