# ---- New Screen ----
# #' @export
#
# new_screen <- function(dir = '.', start = 'auto', template = c('auto', 'none')) {
#
#   files <- list.files(dir, pattern = '\\.tiff?$|\\.jpe?g$|\\.png$', full.names = T)
#   mtime <- file.info(files)$mtime
#   if (start == 'auto') start <- format(min(mtime), usetz = T)
#   if (template == 'auto') template <- files[which.max(mtime)]
#   if (template == 'none') template <- files
#
#   for (scan in unique(template)) {
#     n_positions <- readline(paste0('How many plate positions are in ', scan, '? '))
#     message('What is the query ID for position:')
#     for (n in n_positions) {
#       p_query <- readline(paste0(n, '. '))
#     }
#   }
#   type <- readline('What type of screen is this (e.g. SPA, SGA, Drug, etc.)? ')
#   temp <- readline('What was the incubation temperature (e.g. 30)? ')
#   data_frame(
#     file             = files,
#     crop_template    = template,
#     incubation_start = start,
#     incubation_end   = mtime
#   )
# }


# ---- New Strain Collection ----
#' Create new strain collection
#'
#' A convenience function to create a new strain collection keyfile
#'
#' @param id ID of new library. This ID needs to be unique in the database.
#' @param nplates Number of plates in the library.
#' @param format Size of plates. Defaults to 96.
#' @param dim Aspect ratio of rows to columns. Defaults to \code{c(2, 3)}.
#'
#' @export

new_strain_collection <- function(id, nplates, format = 96, dim = c(2, 3)) {
  nrow <- dim[1] * sqrt(format / prod(dim))
  ncol <- dim[2] * sqrt(format / prod(dim))
  LETTERS <- expand_letters(nrow, LETTERS)

  data_frame(
    strain_collection_id = id,
    strain_id = '',
    plate  = (1:nplates) %>% rep(each = format),
    row    = (LETTERS[1:(format / ncol)]) %>% rep(times = nplates, each = ncol),
    column = (1:(format / nrow)) %>% rep(length.out = format * nplates),
    plate_control = FALSE,
    strain_collection_notes  = '') %>%
    write.csv(file = paste0(id, '.csv'), row.names = FALSE)
}


# ---- New Batch Metadata ----
#' Write metadata templates
#'
#' A convenience function to create a templated 'screens.csv'
#' and 'plates.csv' metadata file for a batch of screens.
#'
#' @param cm Path to colony measurements (CM) file.
#'
#' @export

new_batch_metadata <- function(cm) {
  write_screens_csv(cm)
  write_plates_csv(cm)
}
