# ---- Crop -------------------------------------------------------------------
#' Crop plates
#'
#' Crop plates from images.
#'
#' @param screenmill_plates Path to a screenmill plates annotation CSV file
#' (the result of \link{annotate_plates}).
#'
#' @importFrom stringr str_pad
#' @export

crop <- function(screenmill_plates = 'test-scans/screenmill-plates.csv') {
  dir  <- dirname(screenmill_plates)

  data <-
    readr::read_csv(screenmill_plates) %>%
    mutate(
      group3     = stringr::str_pad(group,     width = 3, side = 'left', pad = 0),
      position3  = stringr::str_pad(position,  width = 3, side = 'left', pad = 0),
      timepoint3 = stringr::str_pad(timepoint, width = 3, side = 'left', pad = 0)
    )

  files <- unique(data$file)
  plate_names <- with(data, paste(date, group3, position3, timepoint3, sep = '-'))
}

#result <- crop()
#result
