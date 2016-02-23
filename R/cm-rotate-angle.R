# ---- plate_rotate_angle ----
#' Determine plate rotation angle
#'
#' Uses a Radon transform to determine the angle that aligns the colony grid
#'
#' @param img A rough cropped plate image
#' @param rough Rough angle in degrees clockwise with which to rotate plate
#' @param incr Degree increment used in fine rotation
#'
#' @details The implementation of this function was adapted from Gitter
#' (see \href{http://www.ncbi.nlm.nih.gov/pubmed/24474170}{PMID:24474170})
#'
#' @importFrom assertthat is.string
#' @importFrom EBImage readImage colorMode channel imageData resize rotate
#' @importFrom PET radon
#' @importFrom dplyr %>%
#' @export

plate_rotate_angle <- function(img, rough, incr = 0.2) {

  # Shrink image for efficiency
  if (is.string(img)) img <- readImage(img)
  if (colorMode(img)) img <- channel(img, 'luminance')
  m <- imageData(resize(img, h = 500))

  # Radon transform requires square matrix
  m_sq <- (m[1:min(dim(m)), 1:min(dim(m))]) %>% rotate(rough)
  rad  <- radon(m_sq, ThetaSamples = (180 / incr) + 1)$rData

  # Compute row-wise variance, the maximum variance is the desired sample angle
  v <- apply(rad, 1, var)
  v[(30 / incr):(150 / incr)] <- 0  # Exclude 30 to 150 degrees
  angle <- (which.max(v) - 1) * incr
  return(rough - angle)
}
