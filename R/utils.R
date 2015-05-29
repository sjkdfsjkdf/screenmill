# Utils: miscelaneous ---------------------------------------------------------

# Assign column names in pipline
# @param x Data frame or matrix
# @param names Character; Column names to assign to x
assign_names <- function(x, names) { colnames(x) <- names; return(x) }

# Utils: read_cm --------------------------------------------------------------

# Parse names in colony measurement log file
# @param lines Character; Vector plate plate lines from CM engine log.
# @param by Delimiter used tp splite plate names, numbers, and conditions.
#' @importFrom dplyr %>% mutate_
parse_names <- function(lines, by = ',') {
  do.call(rbind, strsplit(lines, by)) %>%
    assign_names(c('name', 'numb', 'cond')) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    mutate_(
      numb = ~as.integer(numb),
      cond = ~gsub('\\.[^\\.]*$', '', cond)
    )
}

# Parse measurements in colony measurement log file
# @param lines Character; Vector of measurement lines from CM engine log.
# @param by Delimiter used to split measurements. Defaults to \code{\\\\t}.
#' @importFrom dplyr %>% mutate_
parse_measurements <- function(lines, by) {
  tbl <- do.call(rbind, strsplit(lines, by))

  if (ncol(tbl) == 2) {
    tbl %>%
      assign_names(c('size', 'circ')) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      mutate_(size = ~as.numeric(size), circ = ~as.numeric(circ))
  } else if (ncol(tbl) == 1) {
    tbl %>%
      assign_names('size') %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      mutate_(size = ~as.numeric(size), circ = NA)
  } else {
    stop('Too many measurement columns')
  }
}

# Map strain replicate to measurements
# @param librows Integer; number of rows in strain library.
# @param libcols Integer; number of columns in strain library.
# @param replicates Integer; number of strain replicates in screen.
# @param nobs Integer; number of observations in screen.
#' @importFrom dplyr %>%
map_replicate <- function(librows, libcols, replicates, nobs) {
  # map plate positions to replicate numbers
  (matrix(1, nrow = librows, ncol = libcols) %x%
     matrix(1:replicates, nrow = sqrt(replicates), byrow = TRUE)) %>%
    rep(length.out = nobs) %>%
    as.integer
}

# Map strain library column to measurements
# @param libcols Integer; number of columns in strain library.
# @param nrows Integer; number of rows on plate.
# @param ncols Integer; number of columns on plate.
# @param nobs Integer; number of observations in screen.
#' @importFrom dplyr %>%
map_column <- function(libcols, nrows, ncols, nobs) {
  # map plate positions to strain library columns
  matrix(rep(1:libcols, each = nrows * 2), nrow = nrows, ncol = ncols) %>%
    rep(length.out = nobs)
}

# Map strain library row to measurements
# @param librows Integer; number of rows in strain library.
# @param replicates Integer; number of strain replicates in screen.
# @param nobs Integer; number of observations in screen.
map_row <- function(librows, replicates, nobs) {
  # map plate positions to strain library rows
  rep(LETTERS[1:librows], each = sqrt(replicates), length.out = nobs)
}

# Utils: read_dr --------------------------------------------------------------

# Find table header in text file
# @param path Character; path to file.
# @param match Regular expression used to match header line.
# @param delim Delimiter used to split header
#' @importFrom R.utils countLines
find_header <- function(path, match, delim) {
  # Open file
  con <- file(path, open = "r")
  on.exit(close(con))
  # Find line corresponding to header
  line <- 1
  while (length(header <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if (grepl(match, header[[1]])) break
    else line <- line + 1; next
  }
  # Stop if the header was not found
  stopifnot(line < R.utils::countLines(path))
  return(list(line = line, header = unlist(strsplit(header, delim))))
}

# Utils: new_batch_metadata ---------------------------------------------------

# Write screens csv template
# @param cm Path to colony measurement log file.
#' @importFrom dplyr %>% select_ distinct mutate_ arrange_
write_screens_csv <- function(cm) {
  dirpath <- dirname(cm)
  readLines(cm) %>%
    grep(',', ., value = TRUE) %>%
    parse_names(by = ',') %>%
    assign_names(c('scan_name', 'plate', 'scan_condition')) %>%
    select_('scan_name', 'scan_condition') %>%
    distinct %>%
    mutate_(
      screen_name          = ~'Short descriptive name',
      screen_description   = ~'Long description',
      analysis_id          = ~'ID of analysis',
      strain_collection_id = ~'ID of strain collection used in screen',
      query_type           = ~'plasmid|drug|strain',
      query_id             = ~'Query ID (e.g. plasmid ID; strain ID; drug ID)',
      query_name           = ~'Brief description of query (e.g. GALp-CTF4)',
      control_screen_id    = ~'This screen_id of the control query',
      method_id            = ~'The screening method (e.g. SPA; SGA; drug)',
      media_id             = ~'Media used for final incubation',
      timepoint            = ~'Timepoint (e.g. 0, 1, 2, 3, 4...)',
      temperature          = ~'Temperature (C) of final incubation',
      screen_owner         = ~'Who performed the screen',
      screen_notes         = ~'Notes regarding this screen',
      scan_condition = ~ifelse(scan_condition == '', 'none', scan_condition),
      screen_id = ~paste(Sys.Date(), scan_name, scan_condition, sep = '-')) %>%
    arrange_(~scan_name, ~scan_condition) %>%
    select_(~screen_id, ~scan_name:screen_notes) %>%
    write.csv(file = paste0(dirpath, '/screens.csv'), row.names = FALSE)
}

# Write plates csv template
# @param cm Path to colony measurement log file.
#' @importFrom dplyr %>% select_ distinct mutate_
write_plates_csv <- function(cm) {
  dirpath <- dirname(cm)
  readLines(cm) %>%
    grep(',', ., value = TRUE) %>%
    parse_names(by = ',') %>%
    assign_names(c('scan_name', 'plate', 'scan_condition')) %>%
    select_('scan_name', 'scan_condition', 'plate') %>%
    distinct %>%
    mutate_(
      incubation_start = ~'Format: 2015-04-27 17:04:35 EDT',
      incubation_end   = ~'Format: 2015-04-27 17:04:35 EDT',
      scan_condition = ~ifelse(scan_condition == '', 'none', scan_condition)
    ) %>%
    arrange_(~scan_name, ~scan_condition, ~plate) %>%
    write.csv(file = paste0(dirpath, '/plates.csv'), row.names = FALSE)
}

# Utils for: new_strain_collection --------------------------------------------

# Expand vector of letters
# @param len Integer; desired length letter combinations
# @param letters Character; vector of letters.
expand_letters <- function(len, letters) {
  out <- character(len)
  out[1:length(letters)] <- letters
  i <- 1
  while (any(out == '')) {
    from <- (length(letters) * i) + 1
    to   <- from + length(letters) - 1
    out[from:to] <- paste0(out[i], letters)
    i <- i + 1
  }
  return(out)
}
