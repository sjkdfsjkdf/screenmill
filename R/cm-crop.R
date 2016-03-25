# ---- Crop -------------------------------------------------------------------
#' Crop plates
#'
#' Crop plates from images.
#'
#' @param screenmill_plates Path to a screenmill plates annotation CSV file
#' (the result of \link{annotate_plates}).
#'
#' @export

#library(readr)

crop <- function(screenmill_plates = 'test-scans/screenmill-plates.csv') {
  data <- read_csv(screenmill_plates)
  data
}

#result <- crop()
#result
