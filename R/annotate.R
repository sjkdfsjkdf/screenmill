#' RStudio Addin for annotating plate images
#'
#' This function launches a Shiny gadget that allows a user to record information
#' about plate images for arrayed colony growth experiments.
#'
#' @param dir Directory of images to process. If missing the
#'   user will be prompted to choose a directory.
#' @param queries A dataframe of available queries. Uses
#'   \code{getOption('screenmill.queries')} by default (see Details).
#' @param strain_collections A dataframe of available strain
#'   collections. Uses \code{getOption('screenmill.strain_collections')}
#'   by default (see Details).
#' @param strain_collection_keys A dataframe of strain collection keyfiles.
#'   Uses \code{getOption('screenmill.strain_collection_keys')}
#'   by default (see Details).
#' @param media A dataframe of available media. Uses
#'   \code{getOption('screenmill.media')} by default (see Details).
#' @param treatments A dataframe of available treatments. Uses
#'   \code{getOption('screenmill.treatments')} by default (see Details).
#' @param temperatures A vector of recommended temperatures.
#' @param overwrite Should existing annotations be overwritten?
#'   Defaults to \code{FALSE}.
#' @param update Should annotation tables be updated? Defaults to \code{TRUE}.
#'   Ignored if overwrite is \code{TRUE}, or annotation is incomplete.
#'
#' @details This application is provided to make annotation of plates in
#' colony growth experiments easier and safer. It will try to warn, or prevent
#' the user from entering invalid data. If the user does not supply annotation
#' tables (i.e. missing the arguments \code{queries}, \code{strain_collections},
#' \code{strain_collection_keys}, \code{media}, or \code{treatments}) then the
#' user defined options will be used (via \code{getOption('screenmill.<table>')})
#' The \code{rothfreezer} package database will be used as a fallback if available.
#'
#' The resulting annotation tables are saved as CSV files in the chosen directory.
#'
#' To quit without saving, just press "Cancel".
#'
#' Required fields for annotation tables are described in the following sections.
#'
#' @section Queries:
#'
#' \describe{
#'   \item{\bold{query_id} (chr)}{Query IDs (e.g. plasmid/strain IDs). Must be unique.}
#'   \item{\bold{query_name} (chr)}{Shorthand name of the query. Does not need to be unique.}
#'   \item{\bold{control_query_id} (chr)}{The query ID of the control (e.g. empty vector). Should also be present as a query ID.}
#' }
#'
#' @section Strain Collections:
#'
#' \describe{
#'   \item{\bold{strain_collection_id} (chr)}{Unique IDs for strain collections.}
#'   \item{\bold{description} (chr)}{A description of the strain collection.}
#' }
#'
#' @section Strain Collection Keys:
#'
#' \describe{
#'   \item{\bold{strain_collection_id} (chr)}{The strain's corresponding collection ID.}
#'   \item{\bold{strain_id} (chr)}{Unique identifiers for each strain.}
#'   \item{\bold{strain_name} (chr)}{Shorthand name of the strain. Does not need to be unique.}
#'   \item{\bold{plate} (int)}{The collection plate number.}
#'   \item{\bold{row} (int)}{The row number.}
#'   \item{\bold{column} (int)}{The column number.}
#'   \item{\bold{plate_control} (lgl)}{\code{TRUE}/\code{FALSE} is the strain a plate control? Strains
#'   marked \code{TRUE} will be used for plate level normalization.}
#' }
#'
#' @section Media:
#'
#' \describe{
#'   \item{\bold{media_id} (chr)}{A unique ID for the base media used in the final plate of the experiment.}
#'   \item{\bold{media_name} (chr)}{Shorthand name of the media.}
#' }
#'
#' @section Treatments:
#'
#' \describe{
#'   \item{\bold{treatment_id} (chr)}{A unique ID for the treatment condition.}
#'   \item{\bold{treatment_name} (chr)}{Shorthand name of the treatment condition.}
#'   \item{\bold{control_treatment_id} (chr)}{The treatment ID of the control condition (e.g. Untreated).}
#' }
#'
#' @importFrom readr write_csv read_csv cols_only
#' @importFrom stringr str_pad
#' @importFrom rhandsontable rhandsontable hot_col hot_cols hot_validate_numeric
#' hot_table renderRHandsontable rHandsontableOutput hot_to_r
#' @export

annotate <- function(dir, queries, strain_collections, strain_collection_keys,
                     media, treatments, temperatures = c(23, 27, 30, 33, 37),
                     overwrite = FALSE, update = TRUE) {
  # ---- Setup ----
  if (missing(dir) && interactive()) {
    message('Choose a file in the directory of images you wish to process.')
    dir <- dirname(file.choose())
  }

  # Check arguments
  assert_that(is.dir(dir), is.flag(overwrite), is.flag(update), is.numeric(temperatures))
  dir <- gsub('/$', '', dir)
  target <- file.path(dir, 'screenmill-annotations.csv', fsep = '/')

  # Get information about images - path, file, standard, time_series, time
  images <- image_data(dir)

  # Initialize lookup tables if missing
  use_db <- requireNamespace('rothfreezer', quietly = T)
  if (use_db) db <- rothfreezer::src_rothfreezer()
  if (missing(strain_collection_keys)) {
    strain_collection_keys <- getOption(
      'screenmill.strain_collection_keys',
      default = if (use_db) rothfreezer::view_strain_collection_keys(db) else NULL)
  }
  if (missing(strain_collections)) {
    strain_collections <- getOption(
      'screenmill.strain_collections',
      default = if (use_db) collect(tbl(db, 'strain_collection_info')) else NULL)
  }
  if (missing(queries)) {
    queries <- getOption(
      'screenmill.queries',
      default = if (use_db) collect(tbl(db, 'queries')) else NULL)
  }
  if (missing(treatments)) {
    treatments <- getOption(
      'screenmill.treatments',
      default = if (use_db) collect(tbl(db, 'treatments')) else NULL)
  }
  if (missing(media)) {
    media <- getOption(
      'screenmill.media',
      default = if (use_db) collect(tbl(db, 'media')) else NULL)
  }

  # Check Tables
  check_annotation_tables(strain_collections, strain_collection_keys, queries, treatments, media)

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

    # If not overwrite and everything has been annotated then exit
    if (!overwrite && all(complete.cases(vars$tbl))) {
      message('Plates have already been annotated. Set "overwrite = TRUE" to re-annotate.')

      # If desired, update CSVs with current annotation tables
      if (update) {
        strain_collections %>%
          filter(strain_collection_id %in% c('', vars$tbl$strain_collection_id)) %>%
          write_csv(file.path(dir, 'screenmill-collections.csv'))

        queries %>%
          filter(query_id %in% c('', vars$tbl$query_id)) %>%
          write_csv(file.path(dir, 'screenmill-queries.csv', fsep = '/'))

        treatments %>%
          filter(treatment_id %in% c('', vars$tbl$treatment_id)) %>%
          write_csv(file.path(dir, 'screenmill-treatments.csv', fsep = '/'))

        media %>%
          filter(media_id %in% c('', vars$tbl$media_id)) %>%
          write_csv(file.path(dir, 'screenmill-media.csv', fsep = '/'))

        strain_collection_keys %>%
          filter(strain_collection_id %in% c('', vars$tbl$strain_collection_id)) %>%
          collect %>%
          write_csv(file.path(dir, 'screenmill-collection-keys.csv'))
      }

      return(invisible(dir))
    }
  }

  # Determine column widths as ~10-13 pixels per character
  if (!is.null(strain_collections) && !is.null(queries) && !is.null(treatments) && !is.null(media)) {
    w_query <- max(65,  max(nchar(queries$query_id)) * 13)
    w_colle <- max(160, max(nchar(strain_collections$strain_collection_id)) * 10)
    w_media <- max(40,  max(nchar(media$media_id)) * 12)
    w_treat <- max(75,  max(nchar(treatments$treatment_id)) * 12)
    w_image <- max(130, max(nchar(images$file)) * 10)
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
      annotations <-
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
          time_series = (input$ts),
          hours_growth = difftime(end, start, units = 'hours') %>% as.numeric
        ) %>%
        group_by(date, group, position) %>%
        # Create plate_id column
        mutate(
          timepoint = order(as.POSIXct(time)),
          grp3 = stringr::str_pad(group,     width = 3, side = 'left', pad = 0),
          pos3 = stringr::str_pad(position,  width = 3, side = 'left', pad = 0),
          tim3 = stringr::str_pad(timepoint, width = 3, side = 'left', pad = 0),
          plate_id = paste(date, grp3, pos3, tim3, sep = '-')
        ) %>%
        select(
          plate_id, date, group, position, timepoint, hours_growth, file, template,
          strain_collection_id, plate, query_id, treatment_id, media_id,
          temperature, time_series, start, end = time, owner, email
        )

      # ---- Write tables to file ----
      annotations %>%
        write_csv(target)

      strain_collections %>%
        filter(strain_collection_id %in% c('', annotations$strain_collection_id)) %>%
        write_csv(file.path(dir, 'screenmill-collections.csv', fsep = '/'))

      queries %>%
        filter(query_id %in% c('', annotations$query_id)) %>%
        write_csv(file.path(dir, 'screenmill-queries.csv', fsep = '/'))

      treatments %>%
        filter(treatment_id %in% c('', annotations$treatment_id)) %>%
        write_csv(file.path(dir, 'screenmill-treatments.csv', fsep = '/'))

      media %>%
        filter(media_id %in% c('', annotations$media_id)) %>%
        write_csv(file.path(dir, 'screenmill-media.csv', fsep = '/'))

      keys <-
        strain_collection_keys %>%
        filter(strain_collection_id %in% c('', annotations$strain_collection_id)) %>%
        collect

      write_csv(keys, file.path(dir, 'screenmill-collection-keys.csv'))

      # ---- Issue warnings ----
      x <- annotations[, c('strain_collection_id', 'plate')]
      y <- keys[, c('strain_collection_id', 'plate')]
      crap <- as.data.frame(setdiff(x, y))
      if (nrow(crap)) {
        warning('The strain collection keys are missing the following plate ',
                'numbers.\nPlease resolve this issue before continuing your analysis.', call. = T)
        print(crap)
      }
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
        h2(tags$small('Queries')),
        dataTableOutput('queries'),
        h2(tags$small('Strain collections')),
        dataTableOutput('strain_collections'),
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


check_annotation_tables <- function(key_info, keys, queries, treatments, media) {
  # Check for missing columns
  req_cols_key   <- c('strain_collection_id', 'strain_id', 'strain_name', 'plate', 'row', 'column', 'plate_control')
  req_cols_info  <- c('strain_collection_id', 'description')
  req_cols_query <- c('query_id', 'query_name', 'control_query_id')
  req_cols_treat <- c('treatment_id', 'treatment_name', 'control_treatment_id')
  req_cols_media <- c('media_id', 'media_name')
  missing_key    <- req_cols_key[which(!req_cols_key   %in% colnames(keys))]
  missing_info   <- req_cols_info[which(!req_cols_info  %in% colnames(key_info))]
  missing_query  <- req_cols_query[which(!req_cols_query %in% colnames(queries))]
  missing_treat  <- req_cols_treat[which(!req_cols_treat %in% colnames(treatments))]
  missing_media  <- req_cols_media[which(!req_cols_media %in% colnames(media))]
  if (length(missing_key))   warning('Missing required columns (', paste(missing_key,   collapse = ', '), ') from strain_collection_keys table.', call. = F)
  if (length(missing_info))  warning('Missing required columns (', paste(missing_info,  collapse = ', '), ') from strain_collections table.', call. = F)
  if (length(missing_query)) warning('Missing required columns (', paste(missing_query, collapse = ', '), ') from queries table.', call. = F)
  if (length(missing_treat)) warning('Missing required columns (', paste(missing_treat, collapse = ', '), ') from treatments table.', call. = F)
  if (length(missing_media)) warning('Missing required columns (', paste(missing_media, collapse = ', '), ') from media table.', call. = F)
  # Check for duplicate entries
  # Duplicate strain collection keys are not checked here because this table needs to be filtered before checking
  dup_info  <- with(key_info, strain_collection_id[duplicated(strain_collection_id)])
  dup_query <- with(queries, query_id[duplicated(query_id)])
  dup_treat <- with(treatments, treatment_id[duplicated(treatment_id)])
  dup_media <- with(media, media_id[duplicated(media_id)])
  if (length(dup_info))  warning('Duplicate entries for (', paste(dup_info,  collapse = ', '), ') in strain_collections information table', call. = F)
  if (length(dup_query)) warning('Duplicate entries for (', paste(dup_query, collapse = ', '), ') in queries table', call. = F)
  if (length(dup_treat)) warning('Duplicate entries for (', paste(dup_treat, collapse = ', '), ') in treatments table', call. = F)
  if (length(dup_media)) warning('Duplicate entries for (', paste(dup_media, collapse = ', '), ') in media table', call. = F)
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
