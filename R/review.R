library(shiny)
library(miniUI)
library(assertthat)
library(dplyr)

review <- function(dir = '.') {

  # ---- Setup ----
  # Check arguments
  assert_that(is.dir(dir))
  dir <- gsub('/$', '', dir)
  crop_path <- file.path(dir, 'screenmill-calibration-crop.csv', fsep = '/')
  grid_path <- file.path(dir, 'screenmill-calibration-grid.csv', fsep = '/')
  revi_path <- file.path(dir, 'screenmill-review.csv', fsep = '/')

  # All necessary details to display 1 plate
  if (!(file.exists(crop_path) & file.exists(grid_path))) {
    stop('Please annotate and calibrate before reviewing.')
  } else {
    crop <- readr::read_csv(crop_path)
    grid <- readr::read_csv(grid_path)
  }

  if (!file.exists(revi_path)) {
    revi <-
      grid %>%
      select(template, position, group, colony_row, colony_col, x, y) %>%
      mutate(excluded = FALSE)
  } else {
    revi <- readr::read_csv(revi_path)
  }
  plate <- 1:nrow(crop)

  # ---- Server ----
  server <- function(input, output) {

    # For storing which rows have been excluded
    vals <- reactiveValues(
      revi = revi,
      grid = grid,
      keeprows = NULL,
      plate = 1,
      fine = NULL,
      this_grid = NULL,
      this_revi = NULL,
      image = NULL,
      session = list()
    )

    observeEvent(vals$plate, {
      withProgress(message = 'Reading image ...', value = 0, {
        # Get data for this plate
        this_plate <- vals$plate
        last_plate <- max(this_plate - 1, min(plate))
        last_crop <- crop[last_plate, ]
        this_crop <- crop[this_plate, ]
        vals$this_grid <- vals$grid[vals$grid$template == this_crop$template & vals$grid$position == this_crop$position, ]
        vals$this_revi <- vals$revi[vals$revi$template == this_crop$template & vals$revi$position == this_crop$position, ]
        if (is.null(vals$keeprows)) {
          vals$keeprows <- !vals$this_revi$excluded
        }

        # Process image
        if (vals$plate == 1 || this_crop$template != last_crop$template) {
          vals$image <- EBImage::readImage(file.path(dir, this_crop$template, fsep = '/'))
        }
        rough   <- with(this_crop, vals$image[ rough_l:rough_r, rough_t:rough_b ])
        rotated <- EBImage::rotate(rough, this_crop$rotate)
        fine    <- with(this_crop, rotated[ fine_l:fine_r, fine_t:fine_b ])
        if (this_crop$invert) vals$fine <- 1 - fine else vals$fine <- fine
      })
    })

    output$plot1 <- renderPlot({
      # Plot the kept and excluded points as two separate data sets
      keep    <- vals$this_grid[ vals$keeprows, , drop = FALSE]
      exclude <- vals$this_grid[!vals$keeprows, , drop = FALSE]
      EBImage::display(vals$fine, method = 'raster')
      with(keep, segments(l, t, r, t, col = 'blue'))
      with(keep, segments(l, b, r, b, col = 'blue'))
      with(keep, segments(l, t, l, b, col = 'blue'))
      with(keep, segments(r, t, r, b, col = 'blue'))
      with(exclude, points(x, y, pch = 4, cex = 1.5, col = 'red'))
    })

    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
      result <- brushedPoints(vals$this_revi, input$brush1, xvar = 'x', yvar = 'y', allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, result$selected_)
    })

    observeEvent(input$click1, {
      result <- nearPoints(vals$this_revi, input$click1, xvar = 'x', yvar = 'y', allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, result$selected_)
    })

    # Reset all points
    observeEvent(input$exclude_reset, {
      vals$keeprows <- !vals$this_revi$excluded
    })

    observeEvent(input$exclude_all, {
      vals$keeprows <- rep(FALSE, length(vals$this_revi$excluded))
    })

    observeEvent(input$keep_all, {
      vals$keeprows <- rep(TRUE, length(vals$this_revi$excluded))
    })

    # On Click Next/Previous --------------------------------------------------
    observeEvent(input$next_plate, {
      vals$session[[vals$plate]] <- vals$keeprows
      n <- vals$plate + 1
      vals$plate <- n
      if (length(vals$session) >= n) {
        vals$keeprows <- vals$session[[n]]
      } else {
        vals$keeprows <- NULL
      }
    })

    observeEvent(input$back_plate, {
      vals$session[[vals$plate]] <- vals$keeprows
      n <- max(vals$plate - 1, min(plate))
      vals$plate <- n
      if (length(vals$session) >= n) {
        vals$keeprows <- vals$session[[n]]
      } else {
        vals$keeprows <- NULL
      }
    })
  }

  # ---- User Interface ----
  ui <- miniPage(
    gadgetTitleBar(
      'Review Colony Grids',
      left  = miniTitleBarButton('back_plate', 'Previous Plate'),
      right = miniTitleBarButton('next_plate', 'Next Plate', primary = TRUE)
    ),
    miniContentPanel(
      plotOutput(
        'plot1',
        height = '450px',
        brush = 'brush1',
        dblclick = 'click1'
      ),
      p(),
      fluidRow(
        column(
          width = 12, align = 'center',
          actionButton('exclude_toggle', 'Toggle selection', class = 'btn btn-success action-button'),
          actionButton('exclude_reset', 'Reset'),
          actionButton('exclude_all', 'Exclude all'),
          actionButton('keep_all', 'Keep all')
        )
      )
    )
  )

  # ---- Run ----
  runGadget(ui, server, viewer = dialogViewer('Screenmill Review', width = 1000, height = 1000))
}

review()
