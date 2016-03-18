#' RStudio Addin for annotating plate images
#'
#' This function starts a Shiny gadget that allows a user to record information
#' about plate images for arrayed colony growth experiments.
#'
#' @param dir The directory containing images to annotate.
#' @param n_positions The default number of plate positions per image
#' (defaults to \code{9L}).
#' @param queries An optional vector of available query IDs.
#' @param strain_collections An optional vector of available strain collection IDs.
#' @param media An optional vector of available media IDs.
#' @param treatments An optional vector of available treatment IDs.
#' @param temperatures A vector of recommended temperatures.
#'
#' @details This application is provided to make annotation of plates in
#' colony growth experiments easier and safer. It will try to warn, or prevent
#' the user from entering invalid data. If the \code{rothfreezer} package
#' is installed, arguments set to \code{NULL} will be pulled from this package's
#' database.
#'
#' The resulting annotation table is saved to the current working directory as
#' \code{plate-annotations.csv}.
#'
#' To quit without saving, just press "Cancel".
#'
#' @importFrom readr write_csv
#' @export

annotate_plates <- function(dir = '.',
                            n_positions = 9L,
                            queries = NULL,
                            strain_collections = NULL,
                            media = NULL,
                            treatments = NULL,
                            temperatures = c(23, 27, 30, 33, 37)) {

  # ---- Setup ----
  images <-
    image_data(dir) %>%
    mutate(
      standard = 1:n(),
      time_series = guess_groups(time),
      time = as.character(time)
    )

  # Get known values from database rothfreezer database if available
  if (requireNamespace('rothfreezer', quietly = T)) {
    db <- rothfreezer::src_rothfreezer()
    if (is.null(strain_collections)) {
      strain_collections <-
        tbl(db, 'strain_collections') %>%
        select(strain_collection_id) %>%
        distinct %>%
        collect %>%
        .$strain_collection_id
    }
    if (is.null(queries)) {
      queries <- collect(select(tbl(db, 'queries'), query_id))$query_id
    }
    if (is.null(treatments)) {
      treatments <- collect(select(tbl(db, 'treatments'), treatment_id))$treatment_id
    }
    if (is.null(media)) {
      media <- collect(select(tbl(db, 'media'), media_id))$media_id
    }
  }

  # Determine column widths as ~8-13 pixels per character
  w_query <- max(65,  max(nchar(queries)) * 13)
  w_colle <- max(160, max(nchar(strain_collections)) * 8)
  w_media <- max(40,  max(nchar(media)) * 12)
  w_treat <- max(75,  max(nchar(treatments)) * 12)
  w_image <- max(30,  max(nchar(images$name)) * 8)
  w_tbl3  <- c(45, w_image, 60, w_colle, 40, w_query, w_treat, w_media)

  # ---- UI ----
  ui <-
    miniPage(
      gadgetTitleBar('New Screen', right = miniTitleBarButton('save', 'Save', primary = TRUE)),
      miniContentPanel(
        textInput('user', label = NULL, placeholder = 'Name'),
        textInput('email', label = NULL, placeholder = 'Email'),
        # Table 1: Group Images
        h2('Group Images'),
        radioButtons(
          'time_series',
          h4('Time-series?', style = 'max-width: 800px'),
          c('Yes', 'No'),
          selected = 'Yes',
          inline = T
        ),
        h3(style = 'max-width: 800px', tags$small(
          'Please review the following fields for each image:',
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(code('group'),
              tags$dd('Images groups will be cropped using the most recent image
                in the group as a template (e.g. the last timepoint in a
                time-series). Edit this field to ensure all images within a
                group are assigned the same number.'))
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
              tags$dd('Incubation start time is inferred from the earliest image
              within a group. If this is incorrect, please edit the start time using
              the', strong('YYYY-MM-DD hh:mm:ss'), 'format.')),
            tags$dt(code('positions'), tags$dd('The number of plate positions in the group.')),
            tags$dt(code('temperature'), tags$dd('The incubation temperature of the group.'))
          )))
        )),
        rHandsontableOutput('tbl2'),
        # Table 3: Annotate Plates
        h2('Annotate Plates'),
        h3(style = 'max-width: 800px', tags$small(
          'Please review the following fields for each plate position in each group:',
          h4(tags$small(strong('Positions in raw images are numbered from left to right, top to bottom'))),
          h4(tags$small(tags$dl(
            class = 'dl-horizontal',
            tags$dt(code('strain_collection_id'), tags$dd('A unique ID for the strain collection used.')),
            tags$dt(code('plate'), tags$dd('The strain collection plate number.')),
            tags$dt(code('query_id'), tags$dd('A unique ID for the genetic query condition (e.g. a plasmid, an allele or a strain).')),
            tags$dt(code('treatment_id'), tags$dd('A unique ID for the treatment condition (e.g. drug treatment or irradiation).')),
            tags$dt(code('media_id'), tags$dd('A unique ID for the base growth media (should not including treatment condition).'))
          )))
        )),
        rHandsontableOutput('tbl3')
      )
    )

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
    observeEvent(input$time_series, {
      reset$tbl1 <- T
      reset$tbl2 <- T
      reset$tbl3 <- T
    })
    observeEvent(input$tbl1, {
      reset$tbl1 <- F  # when user edits tbl1, update tbl1() reactive to use edits
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


    # Table 1: Image groups
    # keys | name, time
    # edit | group
    tbl1 <- reactive({
      if (reset$tbl1) {
        images %>%
          mutate(group = switch(input$time_series, Yes = time_series, No = standard)) %>%
          select(-path, -time_series, -standard)
      } else {
        hot_to_r(input$tbl1) %>% filter(complete.cases(.))
      }
    })

    output$tbl1 <- renderRHandsontable({
      tbl1() %>%
        rhandsontable(height = min(c(23 * nrow(.) + 50, 300))) %>%
        hot_col(c('name', 'time'), readOnly = T) %>%
        hot_validate_numeric('group') %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # Table 2: Group annotation
    # keys | group, crop_template
    # edit | start, positions, temperature
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
          mutate(positions = n_positions, temperature = 30L)
      } else {
        hot_to_r(input$tbl2) %>% filter(complete.cases(.))
      }
    })

    output$tbl2 <- renderRHandsontable({
      tbl2() %>%
        rhandsontable(height = 23 * nrow(.) + 50) %>%
        hot_col(c('group', 'crop_template'), readOnly = T) %>%
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



    # Table 3: Plate annotation
    # keys | group, crop_template, position
    # edit | strain_collection_id, plate, query_id, treatment_id, media_id
    tbl3 <- reactive({
      if (reset$tbl3 && is.null(input$tbl3)) {
        tbl2() %>%
          select(group, crop_template, positions) %>%
          group_by(group, crop_template) %>%
          do(data_frame(position = 1:.$positions[1])) %>%
          ungroup %>%
          mutate(
            strain_collection_id = '',
            plate = '',
            query_id = '',
            treatment_id = '',
            media_id = ''
          )
      } else if (reset$tbl3) {
        right_join(
          hot_to_r(input$tbl3) %>% filter(complete.cases(.)),
          tbl2() %>%
            select(group, crop_template, positions) %>%
            group_by(group, crop_template) %>%
            do(data_frame(position = 1:.$positions[1])) %>%
            ungroup,
          by = c('group', 'crop_template', 'position')
        )
      } else {
        hot_to_r(input$tbl3) %>% filter(complete.cases(.))
      }
    })

    output$tbl3 <- renderRHandsontable({
      tbl3() %>%
        rhandsontable(height = 23 * nrow(.) + 50) %>%
        hot_col(c('group', 'crop_template', 'position'), readOnly = T) %>%
        hot_col('query_id', type = 'autocomplete', source = queries) %>%
        hot_col('strain_collection_id', type = 'autocomplete', source = strain_collections) %>%
        hot_validate_numeric('plate') %>%
        hot_col('treatment_id', type = 'autocomplete', source = treatments) %>%
        hot_col('media_id', type = 'autocomplete', source = media) %>%
        hot_cols(colWidths = w_tbl3) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })


    # On Click Save -----------------------------------------------------------
    observeEvent(input$save, {
      annotation_path <-
        select(images, name, path) %>%
        left_join(tbl1(), by = 'name') %>%
        left_join(tbl2(), by = 'group') %>%
        left_join(tbl3(), by = c('group', 'crop_template')) %>%
        group_by(group) %>%
        mutate(Ptime = as.POSIXct(time)) %>%
        arrange(Ptime) %>%
        mutate(
          date = as.Date(start),
          timepoint = 1:n(),
          owner = input$user,
          email = input$email
        ) %>%
        select(
          date, name, path, group, position, strain_collection_id, plate, query_id,
          treatment_id, media_id, temperature, timepoint, start, end = time
        ) %>%
        write_csv('plate-annotations.csv')

      stopApp(annotation_path)
    })
  }

  # ---- Run ----
  runGadget(ui, server, viewer = dialogViewer('New Screen', width = 1100, height = 1000))
}



# ---- Utilities: screenmill ----
image_data <- function(dir, ext =  '\\.tiff?$|\\.jpe?g$|\\.png$') {

  paths <- list.files(dir, pattern = ext, full.names = T)

  if (length(paths) < 1) stop('No images were found in this directory.')

  data_frame(
    path  = paths,
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
