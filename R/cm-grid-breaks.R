# ---- grid_breaks ----
#' Determine column or row breaks for grid
#'
#' Uses midpoints of low average pixel intensity to determine column breaks.
#'
#' @param img An Image object or a matrix
#' @param type Type of grid breaks ('column' or 'row') defaults to column.
#' @param thresh Threshold used to define local valleys in image.
#' @param edges How to handle edge breaks. Defaults to 'inner' which will use
#' the inner edge of flanking valleys. 'outer' will use outer edge of flanking
#' valleys. Otherwise the midpoint of flanking valleys will be returned.
#'
#' @importFrom dplyr mutate
#' @export

grid_breaks <- function(img, type = c('col', 'row'), thresh = 0.03, edges = c('inner', 'outer', 'mid')) {

  if (grepl('col', type[1], ignore.case = T)) {
    # Rows of matrix correspond to x axis (i.e. columns)
    valleys <- find_valleys(rowSums(img), thr = thresh * dim(img)[1])
  } else if (grepl('row', type[1], ignore.case = T)) {
    # Columns of matrix correspond to y axis (i.e. rows)
    valleys <- find_valleys(colSums(img), thr = thresh * dim(img)[2])
  }

  if (edges[1] == 'inner') {
    # Use right edge of first valley, left of last, otherwise use midpoint
    breaks <-
      mutate(
        valleys,
        breaks = ifelse(mid == first(mid), right,
                 ifelse(mid == last(mid), left, mid))
      )
  } else if (edges[1] == 'outer') {
    # Use left edge of first valley, right of last, otherwise use midpoint
    breaks <-
      mutate(
        valleys,
        breaks = ifelse(mid == first(mid), left,
                 ifelse(mid == last(mid), right, mid))
      )
  } else {
    breaks <- mutate(valleys, breaks = mid)
  }
  return(breaks$breaks)
}


# ---- Helpers for find_*_breaks ----

# Label continuous runs with an integer beginning with 1
label_runs <- function(x) {
  runs <- rle(x)$lengths
  rep.int(1:length(runs), runs)
}

# Find the midpoint, left, and right edge of regions of continuous low values
#' @importFrom dplyr data_frame bind_rows
find_valleys <- function(y, thr) {

  # Identify and label valleys
  is_peak <- y > thr
  regions <- label_runs(is_peak) # label consecutive low (F) or high (T) regions
  regions[is_peak] <- 0          # Label peak regions as 0
  valleys <- unique(regions)
  valleys <- valleys[which(valleys != 0)]

  # Find left, middle, and right edge of valley
  bind_rows(
    lapply(valleys, function(v) {
      x <- which(regions == v)
      data_frame(left = min(x), mid = mean(x), right = max(x))
    })
  )
}
