# ---- Calibrate Cropping ----
#' Calibrate cropping and rotation parameters
#'
#' This function calibrates plate cropping and rotation parameters for an image
#' with an arbritrarily sized grid of plates.
#'
#' @param dir Directory of images to process.
#' @param rotate A rough angle in degrees clockwise to rotate each plate. The
#' rotation angle will be further calibrated after applying this rotation.
#' Defaults to \code{90}.
#' @param range Range to explore (in degrees) when calibrating rotation angle.
#' Defaults to \code{6}.
#' @param step Increment (in degrees) to step when calibrating rotation angle.
#' Defaults to \code{0.2}.
#' @param thresh Fraction of foreground pixels needed to identify plate
#' boundaries when rough cropping. Defaults to \code{0.03}.
#' @param invert Should the image be inverted? Defaults to \code{TRUE}.
#' Recommended \code{TRUE} if colonies are darker than the plate.
#' @param display Should cropped images be displayed for review?
#' Defaults to \code{TRUE}.
#' @param overwrite Should existing crop calibration be overwritten?
#' Defaults to \code{FALSE}.
#'
#' @details
#' Crop calibration procedes through the following 3 steps:
#'
#' \enumerate{
#'   \item Rough crop
#'   \item Rotate
#'   \item Fine crop
#' }
#'
#' Rough cropping relies on high contrast between plates. If
#' \code{invert = TRUE} plates should be light and the region between plates
#' should be dark, and vice versa if \code{invert = FALSE}.
#'
#' Rotation first applies the \code{rotate} argument, then iterates through
#' a range of degrees specified by the \code{range} argument with a step
#' specified by the \code{step} argument. The image is first thresholded to identify
#' objects (i.e. colonies) and the objects are expanded to get a rough shape of
#' their location on the plate. The final rotation angle is chosen
#' by minimizing the variance of rowwise sums for the range of rotation angles
#' explored, which effectively aligns a rectangular shape with the axes.
#'
#' Fine cropping finds the nearest object edge (problematic for plates without
#' any growth on the intended grid edges).
#'
#' @importFrom parallel mclapply detectCores
#' @export

calibrate_crop <- function(dir,
                           rotate = 90, range = 6, step = 0.2,
                           thresh = 0.03, invert = TRUE,
                           rough_pad = c(0, 0, 0, 0), fine_pad = c(5, 5, 5, 5),
                           display = TRUE, overwrite = FALSE) {

  # ---- Setup ----
  if (display) { old <- par(no.readonly = TRUE); on.exit(par(old)) }  # only necessary for bug in EBImage < 4.13.7

  # Validate input
  stopifnot(
    is.string(dir), is.number(rotate), is.number(range), is.number(step),
    is.number(thresh), is.flag(invert), is.flag(display), is.flag(overwrite),
    is.numeric(rough_pad) && length(rough_pad) == 4,
    is.numeric(fine_pad)  && length(fine_pad)  == 4
  )

  # Determine working directory
  dir <- gsub('/$', '', dir)
  if (is.dir(dir)) {
    path <- paste(dir, 'screenmill-plates.csv', sep = '/')
  } else {
    path <- dir
  }
  if (!file.exists(path)) {
    stop('Could not find ', path, '. Please annotate plates before cropping.
         See ?annotate_plates for more details.')
  }

  current <- screenmill_plates(path)

  if (attr(current, 'calibrated') && !overwrite) {
    message('Cropping has already been calibrated. Set "overwrite = TRUE" to re-calibrate.')
    return(invisible(dir))
  }

  # Get crop templates
  templates <- paste(dir, unique(current$crop_template), sep = '/')

  # Record start time
  time <- Sys.time()

  # ---- initialize targets ----
  rough_crops <- NULL
  fine_crops  <- NULL

  for (file in templates) {

    # ---- Rough Crop ----
    positions <- current$position[which(current$file == basename(file))]

    # Read as greyscale image
    message('Reading ', basename(file), ' ...')
    img <- EBImage::readImage(file)
    if (EBImage::colorMode(img)) {
      img <- EBImage::channel(img, 'luminance')
    }

    # Find crop coordinates
    message('Rough cropping ', basename(file), ' ...')
    rough <- rough_crop(img, thresh, invert, rough_pad)

    # Add to rough crops target
    rough_crops <- rough %>% mutate(file = basename(file)) %>% bind_rows(rough_crops)

    # Display rough cropped images in browser if desired
    if (display) {
      EBImage::display(img, method = 'raster')
      with(rough, segments(left,  top, right, top, col = 'red'))
      with(rough, segments(left,  bot, right, bot, col = 'red'))
      with(rough, segments(left,  top, left,  bot, col = 'red'))
      with(rough, segments(right, top, right, bot, col = 'red'))
      with(rough, text(x_plate, y_plate, position, col = 'red'))
    }

    # Error for too many annotated positions
    if (nrow(rough) < length(positions)) {
      stop(
        nrow(rough), ' plate positions were detected, but there were ',
        max(positions), ' annotated positions. See ?annotate_plates for more help.')
    }

    # Crop plate images for all annotated positions
    cores <- max(2L, detectCores(), na.rm = T)
    img <- imageData(img)
    plates <- mclapply(positions, function(p) {
      with(rough, img[ left[p]:right[p], top[p]:bot[p] ])
    }, mc.cores = cores)

    # Message for unannotated positions
    if (nrow(rough) > length(positions)) {
      message('Keeping positions (', paste(positions, collapse = ', '), ') of ',
              nrow(rough), ' available.')
    }

    # ---- Rotate/Fine Crop ----
    message('Fine cropping ', basename(file), ' ...')
    fine_crop_prog <- progress_estimated(length(plates))
    for (position in 1:length(plates)) {

      # Select Plate as Image
      plate <- plates[[position]]

      # Find crop and rotation coordinates
      fine <- fine_crop(plate, rotate, range, step, fine_pad, invert)
      fine_crop_prog$tick()$print()

      # Add to fine crops target
      fine_crops <-
        fine %>%
        mutate(file = basename(file), position = position) %>%
        bind_rows(fine_crops)

      # Display fine crop if desired
      if (display) {
        rotated <- EBImage::rotate(plate, fine$rotate)
        cropped <- with(fine, rotated[fine_left:fine_right, fine_top:fine_bot])
        EBImage::display(cropped, method = 'raster')
        x <- nrow(cropped) / 2
        y <- ncol(cropped) / 2
        text(x, y, labels = paste(file, position, sep = '\n'), col = 'red', cex = 1.5)
      }
    }
  }

  message('Cropping calibrated for ', length(templates), ' image(s) in ',
          format(round(Sys.time() - time, 2)))
  # Write and return
  current %>%
    select_(~one_of(attr(., 'annotations'))) %>%
    left_join(rough_crops, by = c('crop_template' = 'file', 'position')) %>%
    left_join(fine_crops,  by = c('crop_template' = 'file', 'position')) %>%
    select_(~date, ~file, ~position, ~everything()) %>%
    write_csv(path)
  return(invisible(dir))
}


# ---- Calibrate Grid --------------------------------------------------------
#' Calibrate Colony Grid
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


# ---- Display Calibration ----
#' Display crop calibration
#'
#' Convenience function for displaying crop calibrations. Usefull for viewing
#' the result of manually edited
#'
#' @param dir Directory of images
#' @param groups Cropping groups to display. Defaults to \code{NULL} which will
#' display all groups.
#' @param positions Positions to display. Defaults to \code{NULL} which will
#' display all positions.
#'
#' @export

display_calibration <- function(dir = '.', groups = NULL, positions = NULL) {
  # only necessary for bug in EBImage < 4.13.7
  old <- par(no.readonly = TRUE)
  on.exit(par(old))

  # Find screenmill-plates
  dir <- gsub('/$', '', dir)
  if (is.dir(dir)) {
    path <- paste(dir, 'screenmill-plates.csv', sep = '/')
  } else {
    path <- dir
  }
  if (!file.exists(path)) {
    stop('Could not find ', path, '. Please annotate plates before cropping.
         See ?annotate_plates for more details.')
  }

  calibration <- screenmill_plates(path)
  if (!is.null(groups)) {
    calibration <- filter(calibration, group %in% c(0, groups))
  }
  if (!is.null(positions)) {
    calibration <- filter(calibration, position %in% c(0, positions))
  }

  files <- paste0(dir, '/', unique(calibration$crop_template))
  for (file in files) {

    # Get data for file
    coords <- calibration[which(calibration$file == basename(file)), ]

    # Read as greyscale image
    img <- EBImage::readImage(file)
    if (EBImage::colorMode(img)) {
      img <- EBImage::channel(img, 'luminance')
    }

    # Apply Crop calibration
    lapply(1:nrow(coords), function(p) {
      rough   <- with(coords, img[ left[p]:right[p], top[p]:bot[p] ])
      rotated <- rotate(rough, coords$rotate[p])
      fine    <- with(coords, rotated[ fine_left[p]:fine_right[p], fine_top[p]:fine_bot[p] ])
      EBImage::display(fine, method = 'raster')
      x <- nrow(fine) / 2
      y <- ncol(fine) / 2
      text(x, y, labels = paste0('Group: ', coords$group[p], '\nPosition: ', coords$position[p]), col = 'red', cex = 1.5)
    })
  }
  return(invisible(dir))
}
