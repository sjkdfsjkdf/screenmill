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

  # ---- Server ----
  server <- function(input, output, session) {

    # For storing which rows have been excluded
    vals <- reactiveValues(
      init  = init %>% split(grouping),
      final = init %>% split(grouping),
      image = NULL,
      last = NULL,
      fine = NULL,
      replot = 1
    )

    # On Click Next/Previous --------------------------------------------------


    observeEvent(input$save, {
      bind_rows(vals$final) %>%
        select(template:b, excluded) %>%
        readr::write_csv(grid_path)
      stopApp(invisible(dir))
    })

    observeEvent(input$next_plate, {
      n <- input$plate + 1
      if (n > exit) {
        bind_rows(vals$final) %>%
          select(template:b, excluded) %>%
          readr::write_csv(grid_path)
        stopApp(invisible(dir))
      }
      updateSliderInput(session, 'plate', value = n)
    })

    observeEvent(input$back_plate, {
      updateSliderInput(session, 'plate', value = max(input$plate - 1, 1))
    })

    grid <- reactive({
      vals$final[[input$plate]]
    })

    observeEvent(input$plate, {
      withProgress(message = 'Reading image ...', value = 0, {
        if (!is.null(vals$last)) {
          previous <- unique(vals$final[[vals$last]]$template)
        } else {
          previous <- NULL
        }
        vals$last <- input$plate
        crop <- distinct(grid()[, names(crop)])

        # Process image
        if (is.null(previous) || crop$template != previous) {
          vals$image   <- EBImage::readImage(file.path(dir, crop$template, fsep = '/'))
        }
        rough   <- with(crop, vals$image[ rough_l:rough_r, rough_t:rough_b ])
        rotated <- EBImage::rotate(rough, crop$rotate)
        fine    <- with(crop, rotated[ fine_l:fine_r, fine_t:fine_b ])
        if (crop$invert) vals$fine <- 1 - fine else vals$fine <- fine
        vals$replot <- vals$replot + 1
      })
    })

    observeEvent(vals$replot, {
      output$plot1 <- renderPlot({
        vals$replot

        # Plot the kept and excluded points as two separate data sets
        keep    <- grid()[!grid()$excluded, , drop = FALSE]
        exclude <- grid()[ grid()$excluded, , drop = FALSE]
        EBImage::display(vals$fine, method = 'raster')
        with(keep, segments(l, t, r, t, col = 'blue'))
        with(keep, segments(l, b, r, b, col = 'blue'))
        with(keep, segments(l, t, l, b, col = 'blue'))
        with(keep, segments(r, t, r, b, col = 'blue'))
        with(exclude, points(x, y, pch = 4, cex = 1.5, col = 'red'))
      })
    })

    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
      result <- brushedPoints(grid(), input$brush1, xvar = 'x', yvar = 'y', allRows = TRUE)
      vals$final[[input$plate]]$excluded <- !xor(!grid()$excluded, result$selected_)
      vals$replot <- vals$replot + 1
    })

    observeEvent(input$click1, {
      result <- nearPoints(grid(), input$click1, xvar = 'x', yvar = 'y', allRows = TRUE)
      vals$final[[input$plate]]$excluded <- !xor(!grid()$excluded, result$selected_)
      vals$replot <- vals$replot + 1
    })

    # Reset all points
    observeEvent(input$exclude_reset, {
      vals$final[[input$plate]]$excluded <- vals$init[[input$plate]]$excluded
      vals$replot <- vals$replot + 1
    })

    observeEvent(input$exclude_all, {
      vals$final[[input$plate]]$excluded <- TRUE
      vals$replot <- vals$replot + 1
    })

    observeEvent(input$keep_all, {
      vals$final[[input$plate]]$excluded <- FALSE
      vals$replot <- vals$replot + 1
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
          actionButton('keep_all', 'Keep all')
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

review()
