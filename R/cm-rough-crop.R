# ---- Rough Crop - CM Engine 2.0 ----
#' Rough crop a grid of plates
#'
#' Finds and returns the rough plate edges for a grid of plates.
#'
#' @param path Path to an image of a plate grid.
#' @param thresh Pixel intensity threshold used to find plate edges. Defaults
#' to \code{0.5}. Note: region between plates is assumed to be dark.
#' @param dim Grid dimensions. Defaults to \code{c(3, 3)} for three rows and
#' three columns of plates respectively.
#' @param pad Number of pixels to add (or remove) from detected plate edges.
#' Defaults to \code{c(-20, -20, -20, -20)} which removes 20 pixels from the
#' left, right, top, and bottom of each plate respectively.
#' @param display Should the resulting cropped images be displayed in a web
#' browser? Defaults to \code{FALSE}.
#'
#' @details Rough crops for a grid of plates are detected by thresholding by the
#' brightness of the image (can be adjusted using the \code{thresh} argument).
#' Rows and columns are then scanned to identify locations with a large number
#' of pixels that are more than the threshold pixel intensity. These transitions
#' are used to define the plate edges. For best results, the grid should be
#' approximately square to the edge of the image. Plates are numbered from
#' left to right, top to bottom.
#'
#' @return \code{rough_crop} returns a dataframe with the following columns:
#'
#' \item{plate}{Integer position of plate in grid (row-wise).}
#' \item{rough_top}{The rough top edge of the plate.}
#' \item{rough_left}{The rough left edge of the plate.}
#' \item{rough_right}{The rough right edge of the plate.}
#' \item{rough_bot}{The rough bottom edge of the plate.}
#'
#' @importFrom EBImage readImage display
#' @importFrom dplyr lag rename_ left_join data_frame %>% arrange mutate select_
#' @export

rough_crop <- function(path,
                       thresh = 0.5,
                       dim = c(3, 3),
                       pad = c(-20, -20, -20, -20),
                       display = FALSE) {

  # Read and threshold image
  img <- readImage(path)
  thr <- img > thresh

  # Find left (l), right (r), top (t) and bottom (b) plate edges
  lr <- apply(thr, 1, function(x) length(which(x)) > 200)
  tb <- apply(thr, 2, function(x) length(which(x)) > 200)
  lr_lag <- lr - lag(lr)
  tb_lag <- tb - lag(tb)
  l <- which(lr_lag > 0)
  t <- which(tb_lag > 0)
  r <- which(lr_lag < 0)
  b <- which(tb_lag < 0)

  # Make sure all plate edges were found, if not use dimensions of image
  if (length(l) < dim[1]) l <- c(1, l)
  if (length(t) < dim[2]) t <- c(1, t)
  if (length(r) < dim[1]) r <- c(r, dim(img)[1])
  if (length(b) < dim[2]) b <- c(b, dim(img)[2])

  # Construct the rough-crop dataframe
  result <- expand.grid(l, t) %>% # All combinations of left and top corners
    rename_(l = ~Var1, t = ~Var2) %>%
    left_join(data_frame(l, r), by = 'l') %>% # Pair right edges
    left_join(data_frame(t, b), by = 't') %>% # Pair bottom edges
    arrange(t, l) %>%  # Arrange plates to be numbered left-right top-bottom
    mutate(
      plate = 1:n(),
      # Add padding if desired
      l = l - pad[1],
      r = r + pad[2],
      t = t - pad[3],
      b = b + pad[4],
      # Limit edges if they excede dimensions of image after padding
      l = ifelse(l < 1, 1, l),
      t = ifelse(t < 1, 1, t),
      r = ifelse(r > dim(img)[1], dim(img)[1], r),
      b = ifelse(b > dim(img)[2], dim(img)[2], b)
    )

  # Display rough cropped images in browser if desired
  if (display) {
    lapply(result$plate, function(i) {
      crop <- with(result, img[(l[i]:r[i]), (t[i]:b[i])])
      display(crop, title = paste('Plate', i))
    })
  }

  # Rename columns of result to be more user friendly
  select_(result, ~plate, rough_top = ~t, rough_left = ~l, rough_right = ~r, rough_bot = ~b)
}
