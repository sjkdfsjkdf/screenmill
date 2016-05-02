#' RStudio Addin for annotating plate images
#'
#' This function launches a Shiny gadget that allows a user to record information
#' about plate images for arrayed colony growth experiments.
#'
#' @param dir Directory of images to process. If \code{NULL} the
#' user will be prompted to choose a directory.
#' @param queries A vector, or dataframe of available queries. Uses
#' \code{getOption('screenmill.queries')} by default (see Details).
#' @param strain_collections A vector, or dataframe of available strain
#' collections. Uses \code{getOption('screenmill.strain_collections')}
#' (see Details).
#' @param media A vector, or dataframe of available media. Uses
#' \code{getOption('screenmill.media')} by default(see Details).
#' @param treatments A vector or dataframe of available treatments. Uses
#' \code{getOption('screenmill.treatments')} by default (see Details).
#' @param temperatures A vector of recommended temperatures.
#' @param overwrite Should existing annotations be overwritten?
#' Defaults to \code{FALSE}.
#'
#' @details This application is provided to make annotation of plates in
#' colony growth experiments easier and safer. It will try to warn, or prevent
#' the user from entering invalid data. If the \code{rothfreezer} package
#' is installed, arguments set to \code{NULL} will be pulled from this package's
#' database.
#'
#' The resulting annotation table is saved as \code{plate-annotations.csv}
#' in the directory containing the annotated images.
#'
#' To quit without saving, just press "Cancel".
#'
#' @importFrom readr write_csv read_csv cols_only
#' @importFrom stringr str_pad
#' @importFrom rhandsontable rhandsontable hot_col hot_cols hot_validate_numeric
#' hot_table renderRHandsontable rHandsontableOutput
#' @export

annotate <- function(dir = NULL,
                     queries = getOption('screenmill.queries'),
                     strain_collections = getOption('screenmill.strain_collections'),
                     media = getOption('screenmill.media'),
                     treatments = getOption('screenmill.treatments'),
                     temperatures = c(23, 27, 30, 33, 37),
                     overwrite = FALSE) {
  # ---- Setup ----
  if (is.null(dir)) {
    message('Choose a file in the directory of images you wish to process.')
    dir <- dirname(file.choose())
  }
  dir <- gsub('/$', '', dir)
  target <- paste(dir, 'screenmill-annotations.csv', sep = '/')

  # Get information about images - path, file, standard, time_series, time
  images <- image_data(dir)

  # Initialize variables
  vars <- list()
  if (!file.exists(target)) {
    # Set default variables
    vars$user  <- NULL
    vars$email <- NULL
    vars$ts    <- 'Yes'

    # Table 1: Image groups keys | file, time
    vars$tbl1 <- read_csv('file,time,group\n', col_types = 'cci')

    # Table 2: Group annotation keys | group, template
    vars$tbl2 <- read_csv('group,template,start,positions,temperature\n', col_types = 'iccin')

    # Table 3: Plate annotation keys | group, template, position
    vars$tbl3 <- read_csv('group,template,position,strain_collection_id,plate,query_id,treatment_id,media_id\n', col_types = 'iciciccc')
  } else {
    # Restore variables
    vars$tbl <-
      screenmill_annotations(target) %>%
      group_by(file, group) %>%
      mutate(positions = n(), time = end) %>%
      ungroup

    # If not overwrite and everything has been annotated then exit
    if (!overwrite && all(complete.cases(vars$tbl))) {
      message('Plates have already been annotated. Set "overwrite = TRUE" to re-annotate.')
      return(invisible(dir))
    }

    vars$user  <- vars$tbl$owner[1]
    vars$email <- vars$tbl$email[1]
    vars$ts    <- vars$tbl$time_series[1]
    vars$tbl1  <-
      (vars$tbl) %>%
      select(file, time, group) %>%
      distinct
    vars$tbl2  <-
      (vars$tbl) %>%
      select(group, template, start, positions, temperature) %>%
      distinct
    vars$tbl3 <-
      (vars$tbl) %>%
      select(
        group, template, position, strain_collection_id, plate, query_id,
        treatment_id, media_id
      ) %>%
      distinct
  }

  # Get known values from rothfreezer database if available
  if (requireNamespace('rothfreezer', quietly = T)) {
    db <- rothfreezer::src_rothfreezer()
    if (is.null(strain_collections)) {
      strain_collections <- collect(tbl(db, 'strain_collection_info'))
    }
    if (is.null(queries)) {
      queries <- collect(tbl(db, 'queries'))
    }
    if (is.null(treatments)) {
      treatments <- collect(tbl(db, 'treatments'))
    }
    if (is.null(media)) {
      media <- collect(tbl(db, 'media'))
    }
  }

  # Coerce vector input to dataframe input
  if (is.vector(strain_collections)) {
    strain_collections <- data_frame(strain_collection_id = strain_collections)
  }
  if (is.vector(queries)) {
    queries <- data_frame(query_id = queries)
  }
  if (is.vector(treatments)) {
    treatments <- data_frame(treatment_id = treatments)
  }
  if (is.vector(media)) {
    media <- data_frame(media_id = media)
  }

  # Determine column widths as ~10-13 pixels per character
  if (!is.null(strain_collections) && !is.null(queries) && !is.null(treatments) && !is.null(media)) {
    w_query <- max(65,  max(nchar(queries$query_id)) * 13)
    w_colle <- max(160, max(nchar(strain_collections$strain_collection_id)) * 10)
    w_media <- max(40,  max(nchar(media$media_id)) * 12)
    w_treat <- max(75,  max(nchar(treatments$treatment_id)) * 12)
    w_image <- max(130,  max(nchar(images$file)) * 10)
    w_tbl3  <- c(45, w_image, 60, w_colle, 40, w_query, w_treat, w_media)
  } else {
    w_tbl3 <- NULL
  }

  # ---- Server ----
  server <- function(input, output, session) {

    # The following is an ugly hack to ensure input table dependencies propagate.
    # The problem - handsontable inputs are only NULL if the table has
    # never been touched, otherwise it stores the last edited state. Which
    # meant that using a test for NULL resulted in a non-responsive table if
    # it was ever modified. The desired behavior is that if a table is
    # edited by the user then, those edits should be stored, and all downstream
    # tables should be updated.
    reset <- reactiveValues(tbl1 = T, tbl2 = T, tbl3 = T)
    observeEvent(input$ts, {
      reset$tbl1 <- T
      reset$tbl2 <- T
      reset$tbl3 <- T
    })
    observeEvent(input$tbl1, {
      reset$tbl1 <- F  # when edit tbl1, update tbl1() reactive to use edits
      reset$tbl2 <- T  # all dependencies will reset
      reset$tbl3 <- T
    })
    observeEvent(input$tbl2, {
      reset$tbl2 <- F
      reset$tbl3 <- T
    })
    observeEvent(input$tbl3, {
      reset$tbl3 <- F
    })


    # Table 1: Image groups keys | file, time ---------------------------------
    tbl1 <- reactive({
      if (reset$tbl1) {
        vars$tbl1 <-
          images %>%
          left_join(rename(vars$tbl1, old_group = group, old_time = time), by = 'file') %>%
          mutate(
            group = switch(input$ts, Yes = time_series, No = standard),
            group = ifelse(is.na(old_group), group, old_group), # restore saved groups
            time  = ifelse(is.na(old_time), time, old_time)     # restore saved time
          ) %>%
          select(file, time, group)
        vars$tbl1
      } else {
        vars$tbl1 <- hot_to_r(input$tbl1) %>% filter(complete.cases(.))
        vars$tbl1
      }
    })

    output$tbl1 <- renderRHandsontable({
      tbl1() %>%
        rhandsontable(height = min(c(23 * nrow(.) + 50, 300))) %>%
        hot_col(c('file', 'time'), readOnly = T) %>%
        hot_validate_numeric('group', allowInvalid = TRUE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # Table 2: Group annotation keys | group, template -------------------
    tbl2 <- reactive({
      if (reset$tbl2) {
        vars$tbl2 <-
          tbl1() %>%
          group_by(group) %>%
          mutate(
            Ptime = as.POSIXct(time),
            template = file[which.max(Ptime)],
            start = min(Ptime) %>% as.character
          ) %>%
          ungroup %>%
          select(group, template, start) %>%
          distinct %>%
          left_join(rename(vars$tbl2, r_start = start), by = c('group', 'template')) %>%
          mutate(
            start       = ifelse(is.na(r_start), start, r_start),
            positions   = ifelse(is.na(positions), 1L, positions),
            temperature = ifelse(is.na(temperature), 30, temperature)
          ) %>%
          arrange(group) %>%
          select(group, template, start, positions, temperature)
        vars$tbl2
      } else {
        vars$tbl2 <- hot_to_r(input$tbl2) %>% filter(complete.cases(.))
        vars$tbl2
      }
    })

    output$tbl2 <- renderRHandsontable({
      tbl2() %>%
        rhandsontable(height = 23 * nrow(.) + 200) %>%
        hot_col(c('group', 'template'), readOnly = T) %>%
        hot_col(
          'start',
          # Highlight cell in Red if value doesn't match ~YYYY-MM-DD hh:mm:ss
          renderer =
            'function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (!/^[1-9][0-9]{3}-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]$/.test(value)) {
                 td.style.background = "red";
               }
             }'
        ) %>%
        hot_col('temperature', type = 'autocomplete', source = temperatures) %>%
        hot_validate_numeric(c('positions', 'temperature')) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # Table 3: Plate annotation keys | group, template, position ---------
    tbl3 <- reactive({
      if (reset$tbl3) {
        vars$tbl3 <-
          tbl2() %>%
          select(group, template, positions) %>%
          group_by(group, template) %>%
          do(data_frame(position = 1:.$positions[1])) %>%
          ungroup %>%
          left_join(vars$tbl3, by = c('group', 'template', 'position')) %>%
          mutate_each(funs(ifelse(is.na(.), '', .)))
        vars$tbl3
      } else {
        vars$tbl3 <- hot_to_r(input$tbl3) %>% filter(complete.cases(.))
        vars$tbl3
      }
    })

    output$tbl3 <- renderRHandsontable({
      tbl3() %>%
        rhandsontable(height = 23 * nrow(.) + 200) %>%
        hot_validate_numeric('plate') %>%
        hot_col(c('group', 'template', 'position'), readOnly = T) %>%
        hot_col('query_id', type = 'autocomplete', source = queries$query_id) %>%
        hot_col('strain_collection_id', type = 'autocomplete', source = strain_collections$strain_collection_id) %>%

        hot_col('treatment_id', type = 'autocomplete', source = treatments$treatment_id) %>%
        hot_col('media_id', type = 'autocomplete', source = media$media_id) %>%
        hot_cols(colWidths = w_tbl3) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # Lookup tables -----------------------------------------------------------
    output$strain_collections <- renderDataTable(strain_collections, options = list(pageLength = 5))
    output$queries <- renderDataTable(queries, options = list(pageLength = 5))
    output$treatments <- renderDataTable(treatments, options = list(pageLength = 5))
    output$media <- renderDataTable(media, options = list(pageLength = 5))

    # On Click Save -----------------------------------------------------------
    observeEvent(input$save, {
      images %>%
        select(file) %>%
        left_join(tbl1(), by = 'file') %>%
        left_join(tbl2(), by = 'group') %>%
        left_join(tbl3(), by = c('group', 'template')) %>%
        mutate_each(funs(ifelse(. == '', NA, .))) %>% # replace empty string with NA
        mutate(
          date = as.Date(start),
          owner = ifelse(is.null(input$user) || (input$user) == '', NA, (input$user)),
          email = ifelse(is.null(input$email) || (input$user) == '', NA, (input$email)),
          time_series = (input$ts)
        ) %>%
        group_by(date, group, position) %>%
        mutate(
          timepoint = order(as.POSIXct(time)),
          grp3 = stringr::str_pad(group,     width = 3, side = 'left', pad = 0),
          pos3 = stringr::str_pad(position,  width = 3, side = 'left', pad = 0),
          tim3 = stringr::str_pad(timepoint, width = 3, side = 'left', pad = 0),
          plate_id = paste(date, grp3, pos3, tim3, sep = '-')
        ) %>%
        select(
          plate_id, date, group, position, timepoint, file, template,
          strain_collection_id, plate, query_id, treatment_id, media_id,
          temperature, time_series, start, end = time, owner, email
        ) %>%
        write_csv(target)

      stopApp(invisible(dir))
    })
  }


  # ---- UI ----
  ui <-
    miniPage(
      gadgetTitleBar(
        'Annotate Plates',
        right = miniTitleBarButton('save', 'Save', primary = TRUE)
      ),
      miniContentPanel(
        textInput('user', label = NULL, value = vars$user, placeholder = 'Name'),
        textInput('email', label = NULL, value = vars$email, placeholder = 'Email'),
        # Table 1: Group Images
        h2('Group Images'),
        radioButtons(
          'ts',
          h4('Time-series?', style = 'max-width: 800px'),
          c('Yes', 'No'),
          selected = vars$ts,
          inline = T
        ),
        h3(style = 'max-width: 800px', tags$small(
          'Please review the following fields for each image:',
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(
              code('group'),
              tags$dd(
                'Images that are part of a time-series (i.e. repeated scans
                of the same set of plates) need to be grouped. Edit this field
                to ensure all images within a series are assigned the same
                group number.'
            ))
          )))
        )),
        rHandsontableOutput('tbl1'),
        # Table 2: Annotate Groups
        h2('Annotate Groups'),
        h3(style = 'max-width: 800px', tags$small(
          'Please review the following fields for each group:',
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(code('start'),
                    tags$dd(
                      'Incubation start time is inferred from the earliest image
                      within a group. If this is incorrect, please edit the
                      start time using the', strong('YYYY-MM-DD hh:mm:ss'),
                      'format.')),
            tags$dt(code('positions'),
                    tags$dd('The number of plate positions in the group.')),
            tags$dt(code('temperature'),
                    tags$dd('The incubation temperature of the group.'))
          )))
        )),
        rHandsontableOutput('tbl2'),
        # Table 3: Annotate Plates
        h2('Annotate Plates'),
        h3(style = 'max-width: 800px', tags$small(
          'Please review the following fields for each plate position in each group:',
          h4(tags$small(strong(
            'Positions in raw images are numbered from left to right, top to bottom'))),
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(code('strain_collection_id'),
                    tags$dd('A unique ID for the strain collection used.')),
            tags$dt(code('plate'),
                    tags$dd('The strain collection plate number.')),
            tags$dt(code('query_id'),
                    tags$dd(
                      'A unique ID for the genetic query condition (e.g. a
                      plasmid, an allele or a strain).')),
            tags$dt(code('treatment_id'),
                    tags$dd(
                      'A unique ID for the treatment condition (e.g. drug
                      treatment or irradiation).')),
            tags$dt(code('media_id'),
                    tags$dd(
                      'A unique ID for the base growth media (should not
                      including treatment condition).'))
          )))
        )),
        rHandsontableOutput('tbl3'),
        # Lookup tables
        h2('Lookup tables'),
        h3(style = 'max-width: 800px', tags$small(
          'The following tables are provided for convenience, if all fields have
          been filled in the above tables, you may press save to exit this
          application:')),
        h2(tags$small('Strain collections')),
        dataTableOutput('strain_collections'),
        h2(tags$small('Queries')),
        dataTableOutput('queries'),
        h2(tags$small('Treatments')),
        dataTableOutput('treatments'),
        h2(tags$small('Media')),
        dataTableOutput('media')
      )
    )


  # ---- Run ----
  runGadget(ui, server, viewer = dialogViewer('New Screen', width = 1100, height = 1000))
}



# ---- Utilities: screenmill ----
image_data <- function(dir, ext =  '\\.tiff?$|\\.jpe?g$|\\.png$') {

  paths <- list.files(dir, pattern = ext, full.names = T)

  if (length(paths) < 1) stop('No images were found in this directory.')

  data_frame(
    path  = paths,
    file  = basename(path),
    time  = file.info(path)$mtime
  ) %>%
  arrange(desc(time)) %>%
  mutate(
    standard = 1:n(),
    time_series = guess_groups(time),
    time = as.character(time)
  )
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
