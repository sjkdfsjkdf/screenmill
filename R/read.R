#------------------------------------------------------------------------------
#' Read CM engine log file.
#'
#' Reads ScreenMill Colony Measurement engine log file.
#'
#' @param path Path to a CM engine log file.
#' @param replicates Integer. The number of replicates for each strain.
#' @param dim Numeric vector of length 2. The aspect ratio of the colonies on a
#' plate. Defaults to 2 rows for every 3 columns.
#'
#' @return A dataframe with columns:
#' \itemize{
#'   \item{\bold{scan_name:}} The name of the scanned plate.
#'   \item{\bold{scan_cond:}} The condition of the scanned plate.
#'   \item{\bold{plate:}} The strain library plate number.
#'   \item{\bold{row:}} The strain library row.
#'   \item{\bold{column:}} The strain library column.
#'   \item{\bold{replicate:}} The strain replicate number.
#'   \item{\bold{size:}} The size of the colony (unadjusted).
#'   \item{\bold{circ:}} The circularity of the colony.
#' }
#'
#' @export

read_cm <- function(path, replicates, dim = c(2, 3)) {

  lines <- readLines(path)

  # parse and clean plate attributes
  plates <- grep('[:alpha:]', lines, value = TRUE) %>% parse_names(by = ',')

  # drop plate names and parse measurements
  cm <- lines[-grep('[:alpha:]', lines)] %>% parse_measurements(by = '\t')

  # Detect density and dimensions based on dim argument
  nobs    <- nrow(cm)
  density <- nobs / nrow(plates)
  nrows   <- dim[1] * sqrt(density / (dim[1] * dim[2]))
  ncols   <- dim[2] * sqrt(density / (dim[1] * dim[2]))
  libdens <- density / replicates
  librows <- dim[1] * sqrt(libdens / (dim[1] * dim[2]))
  libcols <- dim[2] * sqrt(libdens / (dim[1] * dim[2]))

  test <- cm %>%
    mutate_(id = ~rep(plates$id, length.out = nobs, each = density)) %>%
    left_join(plates, by = 'id') %>%
    mutate_(
      row       = ~map_row(librows, replicates, nobs),
      column    = ~map_column(libcols, replicates, nrows, ncols, nobs),
      replicate = ~map_replicate(librows, libcols, replicates, nobs)
    ) %>%
    select_(~id, ~scan_name, ~scan_cond, ~plate, ~row, ~column, ~replicate, ~size, ~circ)
}

#------------------------------------------------------------------------------
#' Read DR stats file.
#'
#' Reads ScreenMill Data Review tables.
#'
#' @param path Path to a DR stats file. Defaults to interactive file selection.
#' @param match regex used to find the line containing the table header.
#'
#' @note For 16 colony replicates, data review engine numbers colonies in an
#' inconsistent way (rowwise for the upper-left quadrant and columnwise for
#' remaining colonies). `read_dr` translates these colony numbers to count
#' rowwise for a 4x4 matrix to match the rowwise counting for 4 colony
#' replicates.
#'
#' @return A dataframe with columns:
#' \itemize{
#'   \item{\bold{scan_name:}} The name of the scanned plate.
#'   \item{\bold{scan_cond:}} The condition of the scanned plate.
#'   \item{\bold{plate:}} The strain library plate number.
#'   \item{\bold{row:}} The strain library row.
#'   \item{\bold{column:}} The strain library column.
#'   \item{\bold{replicate:}} The strain replicate number.
#'   \item{\bold{size_dr:}} Size returned by DR engine.
#'   \item{\bold{excluded_query:}} Whether this observation was excluded in
#'   review.
#'   \item{\bold{excluded_control:}} Whether this observation's control was
#'   excluded in review.
#' }
#'
#' @importFrom tidyr gather
#' @export

read_dr <- function(path, match = 'Query\tCondition\tPlate #\tRow\tColumn') {

  # Somtimes colnames are missing so they have to be filled in after read
  header <- find_header(path, match, delim = '\t')
  if (any(grepl('ID Column', header$header))) {
    colnames <- header$header[1:grep('ID Column', header$header)]
  } else {
    colnames <- header$header
  }
  replicates <- length(grep('Colony Size', colnames))
  if (replicates == 16) {
    # 16 replicates get mapped strangely (Data review engine weirdness)
    replicate_map <- 1:16
    names(replicate_map) <- c(1,2,5,6,9,10,13,14,3,4,7,8,11,12,15,16)
  } else {
    replicate_map <- 1:replicates
    names(replicate_map) <- 1:replicates
  }

  dr <- (readLines(path)[-(1:header$line)]) %>%
    # Split lines by tab
    strsplit('\t') %>%
    # Keep only leading columns (there are often issues with last few cols)
    lapply(function(line, ncol = length(colnames)) line[1:ncol]) %>%
    # Build data frame
    do.call(rbind, .) %>%
    assign_names(colnames) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    # Select and rename important columns
    select_(
      scan_name = ~Query,
      scan_cond = ~Condition,
      plate     = ~`Plate #`,
      row       = ~Row,
      column    = ~Column,
      ~contains('Colony Size')
    ) %>%
    # Gather colony size into single column
    gather('colony_dr', 'size_dr', contains('Colony Size')) %>%
    # Fix columns
    mutate_(
      scan_name = ~as.character(scan_name),
      scan_cond = ~ifelse(is.na(scan_cond) | scan_cond == '', 'none', scan_cond),
      plate     = ~as.integer(gsub('[][]', '', plate)), # remove brackets
      column    = ~as.integer(column),
      colony_dr = ~as.integer(gsub('[^0-9]*', '', colony_dr)), # remove text
      replicate = ~as.integer(names(replicate_map[colony_dr])),
      excluded_query   = ~grepl('\\*', size_dr),
      excluded_control = ~grepl('\\^', size_dr),
      size_dr   = ~as.numeric(gsub('\\*|\\^', '', size_dr)) # remove *^ flags
    ) %>%
    # Sort the data
    arrange_(~scan_name, ~scan_cond, ~plate, ~row, ~column, ~replicate) %>%
    select_(~scan_name, ~scan_cond, ~plate, ~row, ~column, ~replicate,
            ~size_dr, ~excluded_query, ~excluded_control) %>%
    na.omit
}


#------------------------------------------------------------------------------
#' Read rothscreen metadata
#'
#' A convenience function to read and combine rothscreen metadata
#'
#' @param screens Path to screens metadata CSV
#' @param plates Path to plates metadata CSV
#'
#' @importFrom data.table fread
#' @export

read_metadata <- function(screens, plates) {
  scr <- fread(screens, data.table = FALSE)
  plt <- fread(plates, data.table = FALSE)

  left_join(plt, scr) %>%
    mutate_(
      incubation_end   = ~as.POSIXct(incubation_end),
      incubation_start = ~as.POSIXct(incubation_start),
      incubation = ~difftime(incubation_end, incubation_start, units = 'hours'))
}
