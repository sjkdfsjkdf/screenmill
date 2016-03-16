library(shiny)
library(miniUI)
library(dplyr)
library(rhandsontable)

# ---- Screenmill ----
screenmill <- function(dir = 'test-scans') {

  # ---- Setup ----
  images <-
    image_data(dir) %>%
    mutate(
      standard = 1:n(),
      time_series = guess_groups(time),
      time = as.character(time)
    )

  # ---- UI ----
  ui <-
    miniPage(
      gadgetTitleBar('New Screen'),
      miniContentPanel(
        textInput('user', label = NULL, placeholder = 'Name'),
        textInput('email', label = NULL, placeholder = 'Email'),
        h2('Group Images'),
        radioButtons(
          'time_series',
          h4('Time-series?', h3(tags$small(
            p(class = 'help-block', 'If yes, group numbers are guessed. Guessing
              assumes consistent time interval.'
          )))),
          c('No', 'Yes'),
          selected = 'No',
          inline = T
        ),
        h3(tags$small(
          'Please review the following fields for each image:',
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(code('group'),
              tags$dd('If multiple images were generated for the same set of plates
              (e.g. as a time-series), then they should be grouped. Edit this field
              to ensure all images within a group are assigned the same number.'))
          )))
        )),
        # Table 1: Group Images
        rHandsontableOutput('tbl1'),
        h2('Annotate Groups'),
        h3(tags$small(
          'Please review the following fields for each group:',
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(code('start'),
              tags$dd('Incubation start time is inferred from the earliest image
              within a group. If this is incorrect, please edit the start time using
              the', strong('YYYY-MM-DD hh:mm:ss'), 'format')),
            tags$dt(code('positions'), tags$dd('The number of plate positions in the group')),
            tags$dt(code('temperature'), tags$dd('The incubation temperature of the group'))
          )))
        )),
        # Table 2: Annotate Groups
        rHandsontableOutput('tbl2'),
        h2('Annotate Plates'),
        h3(tags$small(
          'Please review the following fields for each plate position in each group:',
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(code('query_id'), tags$dd('A unique ID for the query condition.')),
            tags$dt(code('strain_collection_id'), tags$dd('A unique ID for the strain collection used.')),
            tags$dt(code('plate'), tags$dd('The strain collection plate number.')),
            tags$dt(code('media'), tags$dd('The growth media.')),
            tags$dt(code('notes'), tags$dd('Miscellaneous notes regarding this plate.'))
          )))
        )),
        # Table 3: Annotate Plates
        rHandsontableOutput('tbl3'),
        h2('Annotate Queries'),
        h3(tags$small(
          'Please review the following fields for each query:',
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(code('query_id'), tags$dd('A unique ID for the query condition.')),
            tags$dt(code('query_name'), tags$dd('A short name for the query condition.')),
            tags$dt(code('query_type'), tags$dd('A unique ID for the strain collection used.'))
          )))
        )),
        # Table 4: Annotate Queries
        rHandsontableOutput('tbl4')
      )
    )

  # ---- Server ----
  server <- function(input, output, session) {

    # Track the status of table inputs to determine whether to reset tables
    reset <- reactiveValues(tbl1 = T, tbl2 = T, tbl3 = T, tbl4 = T)
    observeEvent(input$time_series, {
      reset$tbl1 <- T; reset$tbl2 <- T; reset$tbl3 <- T; reset$tbl4 <- T
    })
    observeEvent(input$tbl1, {
      reset$tbl1 <- F; reset$tbl2 <- T; reset$tbl3 <- T; reset$tbl4 <- T
    })
    observeEvent(input$tbl2, {
      reset$tbl2 <- F; reset$tbl3 <- T; reset$tbl4 <- T
    })
    observeEvent(input$tbl3, {
      reset$tbl3 <- F; reset$tbl4 <- T
    })
    observeEvent(input$tbl4, {
      reset$tbl4 <- F
    })


    # Table 1: Image groups -
    tbl1 <- reactive({
      if (reset$tbl1) {
        images %>%
          mutate(group = switch(input$time_series, Yes = time_series, No = standard)) %>%
          select(-path, -time_series, -standard)
      } else {
        hot_to_r(input$tbl1)
      }
    })

    output$tbl1 <- renderRHandsontable({
      tbl1() %>%
        rhandsontable(height = min(c(23 * nrow(.) + 50, 300))) %>%
        hot_col(c('name', 'time'), readOnly = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # Table 2: Group annotation -
    tbl2 <- reactive({
      if (reset$tbl2) {
        tbl1() %>%
          group_by(group) %>%
          mutate(
            Ptime = as.POSIXct(time),
            crop_template = name[which.max(Ptime)],
            start = min(Ptime) %>% as.character
          ) %>%
          ungroup %>%
          select(group, crop_template, start) %>%
          distinct %>%
          arrange(group) %>%
          mutate(positions = 1L, temperature = 30L)
      } else {
        hot_to_r(input$tbl2)
      }
    })

    output$tbl2 <- renderRHandsontable({
      tbl2() %>%
        rhandsontable(height = 23 * nrow(.) + 50) %>%
        hot_col(c('group', 'crop_template'), readOnly = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # Table 3: Plate annotation
    tbl3 <- reactive({
      if (reset$tbl3 && is.null(input$tbl3)) {
        tbl2() %>%
          select(group, crop_template, positions) %>%
          group_by(group, crop_template) %>%
          do(data_frame(position = 1:.$positions[1])) %>%
          ungroup %>%
          mutate(
            query_id = '',
            strain_collection_id = '',
            plate = '',
            media = ''
          )
      } else if (reset$tbl3) {
        right_join(
          hot_to_r(input$tbl3),
          tbl2() %>%
            select(group, crop_template, positions) %>%
            group_by(group, crop_template) %>%
            do(data_frame(position = 1:.$positions[1])) %>%
            ungroup,
          by = c('group', 'crop_template', 'position')
        )
      } else {
        hot_to_r(input$tbl3)
      }
    })

    output$tbl3 <- renderRHandsontable({
      tbl3() %>%
        rhandsontable(height = 23 * nrow(.) + 50) %>%
        hot_col(c('group', 'crop_template', 'position'), readOnly = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # Table 4: Annotate queries
    tbl4 <- reactive({
      if (reset$tbl4 && is.null(input$tbl4)) {
        tbl3() %>%
          select(query_id) %>%
          distinct %>%
          mutate(
            query_name = '',
            query_type = ''
          ) %>%
          filter(query_id != '')
      } else if (reset$tbl4) {
        right_join(
          hot_to_r(input$tbl4),
          tbl3() %>% select(query_id) %>% distinct %>% filter(query_id != ''),
          by = c('query_id')
        )
      } else {
        hot_to_r(input$tbl4)
      }
    })

    output$tbl4 <- renderRHandsontable({
      tbl4() %>%
        rhandsontable(height = 23 * nrow(.) + 50) %>%
        hot_col(c('query_id'), readOnly = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # On Click Done -----------------------------------------------------------
    observeEvent(input$done, stopApp(tbl1()))
  }

  # ---- Run ----
  runGadget(ui, server, viewer = dialogViewer('New Screen', width = 800, height = 800))
}



# ---- Utilities: screenmill ----
image_data <- function(dir, ext =  '\\.tiff?$|\\.jpe?g$|\\.png$') {
  data_frame(
    path  = list.files(dir, pattern = ext, full.names = T),
    name  = gsub(ext, '', basename(path)),
    time  = file.info(path)$mtime
  ) %>%
  arrange(desc(time))
}

guess_groups <- function(time) {
  data_frame(time = time) %>%
  mutate(major = guess_major(time)) %>%
  group_by(major) %>%
  mutate(
    # Difference in minutes from first time rounded to nearest 5 minutes
    diffmin   = 5 * round(as.integer(difftime(time[1], time, units = 'mins')) / 5),
    interval  = guess_interval(diffmin),
    remainder = diffmin %% interval,
    # Classify groups based on sets of 5 minutes
    minor = as.integer(as.factor(remainder))
  ) %>%
  ungroup %>%
  mutate(group = as.integer(as.factor(paste0(major, minor)))) %>%
  .$group
}

# Guess major groups based on large gaps in time.
guess_major <- function(time) {
  dt <- as.integer(difftime(lag(time), time, units = 'mins'))
  major <- c(1, which(dt > median(dt, na.rm = T) + 6 * mad(dt, na.rm = T) + 60))
  rep.int(1:length(major), diff(c(major, length(time) + 1)))
}

# This is insane but... seems to work?
guess_interval <- function(diffminutes, range = c(10, 500), step = 5) {
  intervals <- seq(range[1], range[2], by = step)
  n0 <- sapply(intervals, function(x) {
    inset <- diffminutes[which(diffminutes %% x < step)]
    if (length(inset) <= 1) return(NA)
    sum(abs((inset %/% x) - 0:(length(inset) - 1)))
  })
  intervals[which(n0 == 0)[1]]
}

images <- screenmill()
images