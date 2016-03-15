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
        radioButtons('time_series', 'Time Series?', c('No', 'Yes'), selected = 'No', inline = T),
        rHandsontableOutput('group_table'),
        rHandsontableOutput('group_annotate'),
        rHandsontableOutput('plate_annotate')
      )
    )

  # ---- Server ----
  server <- function(input, output, session) {

    # Table used to assign images to groups if time-series
    output$group_table <- renderRHandsontable({
      if (input$time_series == 'Yes')  df <- select(images, -path, -standard, group = time_series)
      if (input$time_series == 'No')   df <- select(images, -path, -time_series, group = standard)
      if (!is.null(input$group_table)) df <- hot_to_r(input$group_table)

      # Define crop template as most recent scan in group
      DF <<-
        df %>%
        group_by(group) %>%
        mutate(crop_template = name[which.max(as.POSIXct(time))]) %>%
        ungroup

      # Create a handsontable
      df %>%
        rhandsontable(height = 23 * nrow(.) + 50) %>%
        hot_col(c('name', 'time'), readOnly = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    # Table used to annotate groups with # plates, and temp
    output$group_annotate <- renderRHandsontable({

      # Respond to changes in time-series and group table
      input$time_series; input$group_table

      # Initialize df
      df <-
        DF %>%
        select(group, crop_template) %>%
        distinct %>%
        arrange(group) %>%
        mutate(positions = 1L, temperature = 30L)

      # Update df with user input
      if (!is.null(input$group_annotate)) df <- hot_to_r(input$group_annotate)

      # Export
      DF2 <<- left_join(DF, df, by = c('group', 'crop_template'))

      # Create a handsontable
      df %>%
        rhandsontable(height = 23 * nrow(.) + 50) %>%
        hot_col(c('group', 'crop_template'), readOnly = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    # Table used to annotate plates
    output$plate_annotate <- renderRHandsontable({

      # Respond to changes in other inputs
      input$time_series; input$group_table; input$group_annotate

      # Initialize table
      df <-
        DF2 %>%
        select(group, crop_template, positions) %>%
        group_by(group, crop_template) %>%
        do(data_frame(position = 1:.$positions[1])) %>%
        ungroup %>%
        mutate(
          query_id = '',
          query_name = '',
          strain_collection_id = '',
          plate = NA_integer_,
          media = ''
        )

      # Update df with user input
      if (!is.null(input$plate_annotate)) df <- hot_to_r(input$plate_annotate)

      DF3 <<- left_join(DF2, df, by = c('group', 'crop_template'))

      # Create a handsontable
      df %>%
        rhandsontable(height = 23 * nrow(.) + 50) %>%
        hot_col(c('group', 'crop_template', 'position'), readOnly = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    # On Click Done
    observeEvent(input$done, stopApp(DF3))
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