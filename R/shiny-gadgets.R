#' @export
#' @importFrom ggplot2 ggplot aes_string geom_point

ggbrush <- function(data, xvar, yvar) {

  ui <- miniPage(
    gadgetTitleBar("Drag to select points"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      plotOutput("plot", height = "100%", brush = "brush")
    )
  )

  server <- function(input, output, session) {

    # Render the plot
    output$plot <- renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      ggplot(data, aes_string(xvar, yvar)) + geom_point()
    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(data, input$brush))
    })
  }

  runGadget(ui, server)
}

# library(shiny)
# library(miniUI)
# library(rhandsontable)
# library(dplyr)

#' @export

new_screen <- function(dir = '.') {

  files <- list.files(dir, pattern = '\\.tiff?$|\\.jpe?g$|\\.png$', full.names = T)
  files <- gsub('^\\./', '', files)
  mtime <- file.info(files)$mtime
  files <- files[order(mtime)]
  start <- format(min(mtime), usetz = T)

  table <-
    data_frame(
      file             = files,
      crop_template    = files[which.max(mtime)],
      incubation_start = start,
      incubation_end   = format(mtime, usetz = T),
      screen_type = 'SPA'
    )

  # Shiny Gadget UI
  ui <- miniPage(
    gadgetTitleBar('Input Screen Metadata'),
    miniContentPanel(
      h2('Screen'),
      fluidRow(
        column(
          4,
          h4('Collection ID'),
          textInput('collection', NULL, 'Strain collection'),
          p('The strain collection used in this screen')
        ),
        column(
          4,
          h4('Screen Type'),
          radioButtons('type', NULL, list('SPA', 'SGA', 'Drug', 'Other'), inline = T),
          p('SPA: Selective Ploidy Ablation\nSGA: Synthetic Genetic Array')
        )
      ),
      hr(),
      h2('Scans'),
      fluidRow(
        column(
          4,
          h4('Incubation Start Time'),
          textInput('start', NULL, start),
          p('Defaults to earliest scan in directory')
        ),
        column(
          4,
          h4('Crop Template Timepoint'),
          sliderInput('template', NULL, 0, length(files), length(files), step = 1, round = TRUE),
          p('Set to 0 to use each file as its own template. (For a time series, the last timepoint is recommended)')
        ),
        column(
          4,
          h4('Incubation Temperature'),
          numericInput('temp', NULL, 30, min = 0, step = 1),
          p('In Celcius')
        )
      ),
      hr(),
      h2('Positions'),
      numericInput('positions', 'Number of plate positions per scan', 9, min = 1, step = 1),
      hr(),
      rHandsontableOutput('hot'),
      tableOutput('table')
    )
  )

  # Shiny Gadget Server
  server <- function(input, output, session) {

    # Incubation Start
    observeEvent(input$start, {
      table$incubation_start <<- input$start
      output$table <- renderTable(table)
    })

    # Crop Template
    observeEvent(input$template, {
      table$crop_template <<- if (input$template == 0) files else files[input$template]
      output$table <- renderTable(table)
    })

    # Plate Positions
    observeEvent(input$positions, {
      table$position <<- input$positions
    })

    # Screen Type
    observeEvent(input$type, {
      table$screen_type <<- input$type
      output$table <- renderTable(table)
    })

    output$hot

    observeEvent(input$done, {
      stopApp(table)
    })
  }

  runGadget(ui, server, viewer = dialogViewer('New Screen', width = 1500, height = 1500))
}



