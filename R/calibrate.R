#' Calibrate grid
#'
#' Calibrates colony grid coordinates in for each unique grid template found
#' in \code{screenmill-plates.csv} (see \link{annotate_plates}).
#'
#' @param dir Path to screenmill project directory.
#' @param invert Should the image be inverted. (Recommended \code{TRUE} if
#' colonies are darker than the background).
#' @param display Should calibrated grid be displayed for review?
#' Defaults to \code{TRUE}.
#'
#' @importFrom readr write_csv
#' @export

calibrate_grid <- function(dir, invert = TRUE, display = TRUE, overwrite = FALSE) {

  # Determine working directory
  dir <- gsub('/$', '', dir)
  if (!is.dir(dir)) stop(dir, ' is not a directory')

  annot  <- paste(dir, 'screenmill-plates.csv', sep = '/')
  target <- paste(dir, 'screenmill-grid.csv', sep = '/')

  if (!file.exists(annot)) stop('Could not find ', annot, '. Please annotate and crop plates before calibrating grid.')
  if (file.exists(target) && !overwrite) {
    message('Grids have already been calibrated. Set "overwrite = TRUE" to re-calibrate.')
    return(invisible(dir))
  }

  # Get grid templates
  plate_annotation <- screenmill_plates(annot)

  if (is.null(plate_annotation$grid_template)) {
    plate_annotation <-
      plate_annotation %>%
      group_by(crop_template, position) %>%
      mutate(grid_template = img_crop[which(file == crop_template)[1]])
  }

  plates <- paste(dir, unique(plate_annotation$grid_template), sep = '/')

  # Progress information
  progress <- progress_estimated(length(plates), 3)
  time <- Sys.time()
  message('Calibrating ', length(plates), ' grid templates ...')

  # ---- initialize target ----
  result <- NULL

  for (plate in plates) {
    progress$tick()$print()

    # Read as greyscale image
    img <- EBImage::readImage(plate)
    if (EBImage::colorMode(img)) {
      img <- EBImage::channel(img, 'luminance')
    }
    if (invert) img <- 1 - img

    result <-
      locate_grid(img) %>%
      mutate(grid_template = gsub(paste0(dir, '/'), '', plate, fixed = T)) %>%
      bind_rows(result)
  }
  message('Finished in ', format(round(Sys.time() - time, 2)))

  result <- select(result, grid_template, everything())
  write_csv(plate_annotation, annot)
  write_csv(result, target)
  return(invisible(dir))
}


# Locate Colony Grid
#
# Locate grid and determine background pixel intensity for a single image
#
# @param img An Image object. See \link[EBImage]{Image}.
# @param radius Fraction of the average distance between row/column centers and
# edges. Affects the size of the selection box for each colony. Defaults to
# 0.9 (i.e. 90%).
#
#' @importFrom tidyr complete

locate_grid <- function(img, radius = 0.9) {

  # Scale image for rough object detection
  rescaled <- EBImage::normalize(img, inputRange = c(0.1, 0.8))

  # Blur image to combine spotted colonies into single objects for threshold
  blr <- EBImage::gblur(rescaled, sigma = 6)
  thr <- EBImage::thresh(blr, w = 15, h = 15, offset = 0.05)

  # label objects using watershed algorithm to be robust to connected objects
  wat <- EBImage::watershed(EBImage::distmap(thr))

  # Detect rough location of rows and columns
  cols <- grid_breaks(thr, 'col', thresh = 0.05, edges = 'mid')
  rows <- grid_breaks(thr, 'row', thresh = 0.05, edges = 'mid')
  col_centers <- ((cols + lag(cols)) / 2)[-1]
  row_centers <- ((rows + lag(rows)) / 2)[-1]

  # Characterize objects and bin them into rows/columns
  objs <-
    object_features(wat) %>%
    filter(eccen < 0.8) %>%   # remove weird objects
    mutate(
      colony_row = cut(y, rows, labels = FALSE),
      colony_col = cut(x, cols, labels = FALSE)
    )

  # If multiple objects are found in a grid location, choose largest object
  rough_grid <-
    objs %>%
    group_by(colony_row, colony_col) %>%
    summarise(x = x[which.max(area)], y = y[which.max(area)]) %>%
    ungroup

  # Determine x/y coordinates of each grid location
  fine_grid <-
    rough_grid %>%
    # Fill missing row/column combinations with NA
    complete(colony_row, colony_col) %>%
    # Determine row locations
    group_by(colony_row) %>%
    arrange(colony_col) %>%
    mutate(
      # If missing, use estimated center
      y = ifelse(is.na(y), row_centers[colony_row], y),
      y = round(predict(smooth.spline(colony_col, y), colony_col)[[2]])
    ) %>%
    # Determine column locations
    group_by(colony_col) %>%
    arrange(colony_row) %>%
    mutate(
      # If missing, use estimated center
      x = ifelse(is.na(x), col_centers[colony_col], x),
      x = round(predict(smooth.spline(colony_row, x), colony_row)[[2]])
    ) %>%
    ungroup

  # Add a selection box
  selection <-
    fine_grid %>%
    mutate(
      radius = round(((mean(diff(rows)) + mean(diff(cols))) / 4) * radius),
      l = x - radius,
      r = x + radius,
      t = y - radius,
      b = y + radius,
      # Fix edges
      l = as.integer(round(ifelse(l < 1, 1, l))),
      r = as.integer(round(ifelse(r > nrow(img), nrow(img), r))),
      t = as.integer(round(ifelse(t < 1, 1, t))),
      b = as.integer(round(ifelse(b > ncol(img), ncol(img), b))),
      # Identify corner intensities,
      tl = img[as.matrix(cbind(l, t))],
      tr = img[as.matrix(cbind(r, t))],
      bl = img[as.matrix(cbind(l, b))],
      br = img[as.matrix(cbind(r, b))],
      bg = apply(cbind(tl, tr, bl, br), 1, mean, trim = 0.5)
    )

  # Predict background intensity via loess smoothing
  selection$background <-
    loess(bg ~ colony_row + colony_col, data = selection, span = 0.3, normalize = F, degree = 2) %>%
    predict

  return(selection %>% select(colony_row, colony_col, x, y, l, r, t, b, background))
}
