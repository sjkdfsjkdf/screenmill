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
#' @export

calibrate <- function(dir, rotate = 90, range = 6, step = 0.2, thresh = 0.03,
                      invert = TRUE, rough_pad = c(0, 0, 0, 0),
                      fine_pad = c(5, 5, 5, 5), display = TRUE,
                      overwrite = FALSE) {

  # Save plot parameter defaults. Only necessary for bug in EBImage < 4.13.7
  if (display) { old <- par(no.readonly = TRUE); on.exit(par(old)) }

  # Validate input
  stopifnot(
    is.string(dir), is.dir(dir), is.number(rotate), is.number(range),
    is.number(step), is.number(thresh), is.flag(invert), is.flag(display),
    is.flag(overwrite), is.numeric(rough_pad), length(rough_pad) == 4,
    is.numeric(fine_pad), length(fine_pad) == 4
  )

  # Clean trailing slash from directory input
  dir <- gsub('/$', '', dir)
  plt_path <- paste(dir, 'screenmill-annotations.csv', sep = '/')
  crp_path <- paste(dir, 'screenmill-calibration-crop.csv', sep = '/')
  grd_path <- paste(dir, 'screenmill-calibration-grid.csv', sep = '/')

  # Stop if plates have not yet been annotated
  if (!file.exists(plt_path)) stop('Could not find ', plt_path, '. Please annotate plates before cropping. See ?annotate for more details.')

  if (!overwrite && (file.exists(crp_path) || file.exists(grd_path))) {
    # Exit if already calibratd and no overwrite
    message('This batch has already been calibrated. Set "overwrite = TRUE" to re-calibrate.')
    return(invisible(dir))
  } else {
    # Remove pre-existing files
    if (file.exists(crp_path)) file.remove(crp_path)
    if (file.exists(grd_path)) file.remove(grd_path)
  }

  # Get paths to templates relative to dir, and corresponding plate positions
  annotation <- distinct(transmute(read_csv(plt_path), template = paste(dir, template, sep = '/'), position = position))
  templates <- unique(annotation$template)
  positions <- with(annotation, split(position, template))

  # Record start time
  time <- Sys.time()

  # Calibrate each template by iterating through templates and positions
  mapply(
    calibrate_template,
    templates,
    positions,
    MoreArgs = list(thresh, invert, rough_pad, fine_pad, rotate, range, step, display, crp_path, grd_path)
  )
  message('Finished calibration in ', format(round(Sys.time() - time, 2)))
  return(invisible(dir))
}



# ---- Utilities: calibrate ---------------------------------------------------
# Calibrate a single template image
#
# @param template path to template image
# @param positions integer vector of plate positions
# @param thresh ? TODO currently used to detect rough crop locations
# @param invert Should the image be inverted
# @param rough_pad Padding around rough crop
# @param fine_pad Padding to add around fine crop
# @param rotate Rough rotation angle in degrees
# @param range Range of angles to explore in degrees
# @param step Step interval to explore when optimizing rotation angle
# @param display Should calibration be displayed
# @param crp path to crop calibration output
# @param grd path to grid calibration output
#
#' @importFrom readr write_csv

calibrate_template <- function(template, positions, thresh, invert, rough_pad,
                               fine_pad, rotate, range, step, display, crp, grd) {

  # Read image in greyscale format
  #message('Calibrating ', basename(template), ' ...')
  cat('\r', basename(template), ': reading image', sep = '')
  img <- read_greyscale(template)

  # Determine rough crop coordinates
  rough <- rough_crop(img, thresh, invert, rough_pad) %>% mutate_(template = ~basename(template))

  # Message about unannotated positions
  if (nrow(rough) > length(positions)) warning('For ', basename(template), ', keeping positions (', paste(positions, collapse = ', '), ') of ', nrow(rough), ' available.')

  # Display rough cropped images in browser if desired
  if (display) display_rough_crop(img, rough, 'red')

  # Rough crop each plate
  plates <- lapply(positions, function(p) with(rough, img[ rough_l[p]:rough_r[p], rough_t[p]:rough_b[p] ]))

  # Determine fine crop coordinates
  #message('Cropping plates ...')
  clear_console()
  progress <- progress_estimated(length(positions))
  fine <-
    lapply(positions, function(p) {
      progress$tick()$print(); cat('\r', basename(template), ': cropping |', sep = '')
      fine_crop(plates[[p]], rotate, range, step, fine_pad, invert) %>%
        mutate(template = basename(template), position = p)
    }) %>%
    bind_rows

  # Determine grid coordinates
  #message('Locating colonies ...')
  clear_console()
  progress <- progress_estimated(length(positions))
  grid <-
    lapply(positions, function(p) {
      progress$tick()$print(); cat('\r', basename(template), ': gridding |', sep = '')
      plate <- plates[[p]]
      if (invert) plate <- 1 - plate
      rotated <- EBImage::rotate(plate, fine$rotate[p])
      cropped <- with(fine, rotated[fine_l[p]:fine_r[p], fine_t[p]:fine_b[p]])

      result <- locate_grid(cropped, radius = 0.9)

      if (is.null(result)) {
        warning(
          'Failed to locate colony grid for ', basename(template),
          ' at position ', p, '. This plate position has been skipped.')
      } else {
        result <- mutate_(result, template = ~basename(template), position = ~p)
      }

      # Display plate if desired
      if (display) display_plate(cropped, result, template, p, text.color = 'red', grid.color = 'blue')

      return(result)
    }) %>%
    bind_rows %>%
    select_(~template, ~position, ~everything())

  # Combine rough and fine crop coordinates
  crop <- left_join(rough, fine, by = c('template', 'position')) %>%
    select_(~template, ~position, ~everything())

  # Write results to file
  write_csv(crop, crp, append = file.exists(crp))
  write_csv(grid, grd, append = file.exists(grd))
  clear_console()
}


# ---- Display functions ------------------------------------------------------
display_rough_crop <- function(img, rough, color) {
  EBImage::display(img, method = 'raster')
  with(rough, segments(rough_l, rough_t, rough_r, rough_t, col = color))
  with(rough, segments(rough_l, rough_b, rough_r, rough_b, col = color))
  with(rough, segments(rough_l, rough_t, rough_l, rough_b, col = color))
  with(rough, segments(rough_r, rough_t, rough_r, rough_b, col = color))
  with(rough, text(plate_x, plate_y, position, col = color))
}

display_plate <- function(img, grid, template, position, text.color, grid.color) {
  EBImage::display(img, method = 'raster')

  if (!is.null(grid)) {
    with(grid, segments(l, t, r, t, col = grid.color))
    with(grid, segments(l, b, r, b, col = grid.color))
    with(grid, segments(l, t, l, b, col = grid.color))
    with(grid, segments(r, t, r, b, col = grid.color))
  }

  x <- nrow(img) / 2
  y <- ncol(img) / 2
  text(x, y, labels = paste(basename(template), position, sep = '\n'), col = text.color, cex = 1.5)
}

# ---- Locate Colony Grid -----------------------------------------------------
# Locate grid and determine background pixel intensity for a single image
#
# @param img An Image object or matrix. See \link[EBImage]{Image}.
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
  cols <- grid_breaks(thr, 'col', thresh = 0.07, edges = 'mid')
  rows <- grid_breaks(thr, 'row', thresh = 0.07, edges = 'mid')
  col_centers <- ((cols + lag(cols)) / 2)[-1]
  row_centers <- ((rows + lag(rows)) / 2)[-1]

  if (length(col_centers) < 1 || length(row_centers) < 1) return(NULL)

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
      # Fix edges if radius is out of bounds of image
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


clear_console <- function() {
  utils::flush.console()
  cat('\r', rep(' ', getOption('width') - 1), sep = '')
  utils::flush.console()
}

# ---- Display Calibration: TODO ----------------------------------------------
# Display crop calibration
#
# Convenience function for displaying crop calibrations. Usefull for viewing
# the result of manually edited
#
# @param dir Directory of images
# @param groups Cropping groups to display. Defaults to \code{NULL} which will
# display all groups.
# @param positions Positions to display. Defaults to \code{NULL} which will
# display all positions.
#
# @export

display_calibration <- function(dir = '.', groups = NULL, positions = NULL) {
  # only necessary for bug in EBImage < 4.13.7
  old <- par(no.readonly = TRUE)
  on.exit(par(old))

  # Find screenmill-annotations
  dir <- gsub('/$', '', dir)
  if (is.dir(dir)) {
    path <- paste(dir, 'screenmill-annotations.csv', sep = '/')
  } else {
    path <- dir
  }
  if (!file.exists(path)) {
    stop('Could not find ', path, '. Please annotate plates before cropping.
         See ?annotate for more details.')
  }

  calibration <- screenmill_annotations(path)
  if (!is.null(groups)) {
    calibration <- filter(calibration, group %in% c(0, groups))
  }
  if (!is.null(positions)) {
    calibration <- filter(calibration, position %in% c(0, positions))
  }

  files <- paste0(dir, '/', unique(calibration$template))
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
