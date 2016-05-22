#' @export

review <- function(dir = '.') {

  # ---- Setup ----
  # Check arguments
  assert_that(is.dir(dir))
  dir <- gsub('/$', '', dir)
  crop_path <- file.path(dir, 'screenmill-calibration-crop.csv', fsep = '/')
  grid_path <- file.path(dir, 'screenmill-calibration-grid.csv', fsep = '/')

  # All necessary details to display 1 plate
  if (!(file.exists(crop_path) & file.exists(grid_path))) {
    stop('Please annotate and calibrate before reviewing.')
  }
  crop <- readr::read_csv(crop_path)
  init <-
    readr::read_csv(grid_path) %>%
    left_join(crop, by = c('template', 'position'))

  if (!has_name(init, 'excluded')) {
    init <- mutate(init, excluded = FALSE)
  }

  grouping <- paste(init$template, init$position)
  exit <- length(unique(grouping))
  init_list <- init %>% split(grouping)

  # ---- Server ----
  server <- function(input, output, session) {

    # Reactive values shared between observers --------------------------------
    react <- reactiveValues(
      init  = init_list,
      final = init_list,
      image = NULL,
      last = NULL,
      fine = NULL,
      exclude_border = 0,
      replot = 1
    )

    # On Click Save, write to CSV and quit ------------------------------------
    observeEvent(input$save, {
      bind_rows(react$final) %>%
        select(template:b, excluded) %>%
        readr::write_csv(grid_path)
      stopApp(invisible(dir))
    })

    # On next, increment plate, write to CSV and quit if end is reached -------
    observeEvent(input$next_plate, {
      n <- input$plate + 1
      if (n > exit) {
        bind_rows(react$final) %>%
          select(template:b, excluded) %>%
          readr::write_csv(grid_path)
        stopApp(invisible(dir))
      }
      updateSliderInput(session, 'plate', value = n)
    })

    # On back, decrement plate, but don't go below 1 --------------------------
    observeEvent(input$back_plate, {
      updateSliderInput(session, 'plate', value = max(input$plate - 1, 1))
    })

    # Update variables for this plate -----------------------------------------
    grid <- reactive({
      react$final[[input$plate]]
    })

    observeEvent(input$plate, {
      withProgress(message = 'Reading image ...', value = 0, {
        react$exclude_border <- 0
        if (!is.null(react$last)) {
          previous <- unique(react$final[[react$last]]$template)
        } else {
          previous <- NULL
        }
        react$last <- input$plate
        crop <- distinct(grid()[, names(crop)])

        # Read raw image if different from previous plate
        if (is.null(previous) || crop$template != previous) {
          react$image   <- EBImage::readImage(file.path(dir, crop$template, fsep = '/'))
        }
        # Generate plate image from raw image
        rough   <- with(crop, react$image[ rough_l:rough_r, rough_t:rough_b ])
        rotated <- EBImage::rotate(rough, crop$rotate)
        fine    <- with(crop, rotated[ fine_l:fine_r, fine_t:fine_b ])
        if (crop$invert) react$fine <- 1 - fine else react$fine <- fine

        # Trigger re-plot
        react$replot <- react$replot + 1
      })
    })

    # Plot only when triggered by change in value to replot -------------------
    observeEvent(react$replot, {
      output$plot1 <- renderPlot({
        react$replot

        # Plot the kept and excluded points as two separate data sets
        keep    <- grid()[!grid()$excluded, , drop = FALSE]
        exclude <- grid()[ grid()$excluded, , drop = FALSE]
        EBImage::display(react$fine, method = 'raster')
        with(keep, segments(l, t, r, t, col = 'blue'))
        with(keep, segments(l, b, r, b, col = 'blue'))
        with(keep, segments(l, t, l, b, col = 'blue'))
        with(keep, segments(r, t, r, b, col = 'blue'))
        with(exclude, points(x, y, pch = 4, cex = 1.5, col = 'red'))
      })
    })

    # Toggle points that are brushed, when button is clicked ------------------
    observeEvent(input$exclude_toggle, {
      result <- brushedPoints(grid(), input$brush1, xvar = 'x', yvar = 'y', allRows = TRUE)
      react$final[[input$plate]]$excluded <- !xor(!grid()$excluded, result$selected_)
      react$replot <- react$replot + 1  # Trigger re-plot
    })

    # Toggle points that are double clicked -----------------------------------
    observeEvent(input$click1, {
      result <- nearPoints(grid(), input$click1, xvar = 'x', yvar = 'y', allRows = TRUE)
      react$final[[input$plate]]$excluded <- !xor(!grid()$excluded, result$selected_)
      react$replot <- react$replot + 1  # Trigger re-plot
    })

    # Reset all points to original state when app was started -----------------
    observeEvent(input$exclude_reset, {
      react$final[[input$plate]]$excluded <- react$init[[input$plate]]$excluded
      react$replot <- react$replot + 1  # Trigger re-plot
    })

    # Exclude all points ------------------------------------------------------
    observeEvent(input$exclude_all, {
      react$final[[input$plate]]$excluded <- TRUE
      react$replot <- react$replot + 1  # Trigger re-plot
    })

    # Keep all points ---------------------------------------------------------
    observeEvent(input$keep_all, {
      react$final[[input$plate]]$excluded <- FALSE
      react$replot <- react$replot + 1  # Trigger re-plot
    })

    # Exclude Border ---------------------------------------------------------
    observeEvent(input$exclude_border, {
      react$exclude_border <- react$exclude_border + 1
      n <- react$exclude_border
      colony_row <- grid()$colony_row
      colony_col <- grid()$colony_col
      rows <- sort(unique(colony_row))
      cols <- sort(unique(colony_col))
      rows <- c(head(rows, n), tail(rows, n))
      cols <- c(head(cols, n), tail(cols, n))
      react$final[[input$plate]]$excluded[colony_row %in% rows | colony_col %in% cols] <- TRUE
      react$replot <- react$replot + 1  # Trigger re-plot
    })
  }

  # ---- User Interface ----
  ui <- miniPage(
    gadgetTitleBar(
      'Review Colony Grids',
      right = miniTitleBarButton('save', 'Save', primary = TRUE)
    ),
    miniContentPanel(
      fluidRow(
        column(width = 1, align = 'right',
          actionButton('back_plate', '', icon = icon('angle-left'), style = 'height: 410px;')
        ),
        column(width = 10, align = 'center',
          plotOutput(
            'plot1',
            height = '410px',
            brush = 'brush1',
            dblclick = 'click1'
          )
        ),
        column(width = 1, align = 'left',
          actionButton('next_plate', '', icon = icon('angle-right'), style = 'height: 410px;')
        )
      ),
      p(),
      fluidRow(
        column(
          width = 12, align = 'center',
          actionButton('exclude_toggle', 'Toggle selection', class = 'btn btn-success action-button'),
          actionButton('exclude_reset', 'Reset'),
          actionButton('exclude_all', 'Exclude all'),
          actionButton('keep_all', 'Keep all'),
          actionButton('exclude_border', 'Exclude Border')
        )
      ),
      fluidRow(
        column(
          width = 12, align = 'center',
          sliderInput(
            'plate', '', min = 1, max = exit, value = 1,
            step = 1, round = TRUE, ticks = TRUE, width = '95%'
          )
        )
      )
    )
  )

  # ---- Run ----
  runGadget(ui, server, viewer = dialogViewer('Screenmill Review', width = 850, height = 1000))
}

review_addin <- function() {
  message('Choose a file in the directory of images you wish to process.')
  dir <- dirname(file.choose())
  review(dir)
}
