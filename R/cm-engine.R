# ---- Calibrate Cropping ----
#' Calibrate cropping and rotation parameters
#'
#' This function calibrates plate cropping and rotation parameters for an image
#' with an arbritrarily sized grid of plates.
#'
#' @param annotations Path to a screenmill plate annotations CSV file
#' (the result of \link{annotate_plates}).
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
#' @param overwrite Should an existing screenmill crop calibration file be
#' overwritten? Defaults to \code{FALSE} to protect manually adjusted values.
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
#' @return Writes a file called \code{screenmill-plate-annotations.csv} to the image
#' directory and returns a path to this file.
#'
#' @export

calibrate_crop <- function(annotations,
                           rotate = 90, range = 6, step = 0.2,
                           thresh = 0.03, invert = TRUE,
                           rough_pad = c(0, 0, 0, 0), fine_pad = c(5, 5, 5, 5),
                           display = TRUE, overwrite = FALSE) {

  # ---- Setup ----
  # Save graphical parameters to reset on exit (bug fixed in EBImage >= 4.13.7)
  old <- par(no.readonly = TRUE)
  on.exit(par(old))

  # Determine working directory
  dir <- dirname(annotations)
  output_path <- paste0(dir, '/screenmill-crop-calibration.csv')

  if (file.exists(output_path) && !overwrite) {
    message('Cropping has already been calibrated. Set "overwrite = TRUE" to ',
            'overwrite existing crop calibration.')
    return(output_path)
  }

  # Read annotation data
  if (!file.exists(annotations)) {
    stop(paste('Annotations not found. Please annotate plates before cropping.',
               'See ?annotate_plates for more help.'))
  }
  anno <-
    read_csv(annotations) %>%
    mutate(path = gsub('^./', paste0(dir, '/'), path))
  templates <- with(anno, unique(path[which(crop_template == name)]))

  # Record start time
  time <- Sys.time()

  # ---- initialize targets ----
  rough_crops <- NULL
  fine_crops  <- NULL

  for (file in templates) {

    # ---- Rough Crop ----
    # Read as greyscale image
    message('Reading ', basename(file), ' ...')
    img <- EBImage::readImage(file)
    if (EBImage::colorMode(img)) {
      img <- EBImage::channel(img, 'luminance')
    }

    # Find crop coordinates
    message('Rough cropping ', basename(file), ' ...')
    rough <- rough_crop(img, thresh, invert, rough_pad)
    rough_crop_prog <- progress_estimated(nrow(rough), 3)

    # Add to rough crops target
    rough_crops <- rough %>% mutate(file = file) %>% bind_rows(rough_crops)

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
    positions <- filter(anno, path == file)$position
    if (nrow(rough) < length(positions)) {
      stop(
        nrow(rough), 'plate positions were detected, but there were ',
        max(positions), ' annotated positions. See ?annotate_plates for more help.')
    }

    # Crop plate images for all annotated positions

    plates <- lapply(positions, function(p) {
      rough_crop_prog$tick()$print()
      with(rough, img[ left[p]:right[p], top[p]:bot[p] ])
    })

    # Message for unannotated positions
    if (nrow(rough) > length(positions)) {
      message(
        nrow(rough), 'positions were detected, but only annotated positions (',
        paste(positions, collapse = ', '), ') were used.')
    }

    # ---- Rotate/Fine Crop ----
    message('Fine cropping ', basename(file), ' ...')
    fine_crop_prog <- progress_estimated(length(plates))
    for (position in 1:length(plates)) {

      # Select Plate
      plate <- plates[[position]]

      # Find crop and rotation coordinates
      fine <- fine_crop(plate, rotate, range, step, fine_pad, invert)
      fine_crop_prog$tick()$print()

      # Add to fine crops target
      fine_crops <-
        fine %>%
        mutate(file = file, position = position) %>%
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
  # Return
  left_join(rough_crops, fine_crops, by = c('file', 'position')) %>%
    select_(~file, ~position, ~everything()) %>%
    write_csv(output_path)
}



# ---- Rough Crop ----
#' Rough crop a grid of plates
#'
#' Finds the rough plate edges for a grid of plates.
#'
#' @param img Either an "Image" object (see \link[EBImage]{readImage}), or a
#' matrix.
#' @param thresh Fraction of foreground pixels needed to identify plate
#' boundaries. Defaults to \code{0.03}.
#' @param invert Should the image be inverted? Defaults to \code{TRUE}.
#' Recommended \code{FALSE} if region between plates is lighter than the
#' plates. This is faster than manual inversion of entire image.
#' @param pad Number of pixels to add (or remove) from detected plate edges.
#' Defaults to \code{c(0, 0, 0, 0)} which adds 0 pixels to the
#' left, right, top, and bottom coordinates of each plate respectively. Negative
#' values will remove pixels.
#' @param display Should the resulting cropped images be displayed?
#' Defaults to \code{FALSE}.
#'
#' @details Rough crops for a grid of plates are detected by thresholding by the
#' brightness of the image (can be adjusted using the \code{thresh} argument).
#' Rows and columns are then scanned to identify locations with a large number
#' of pixels that are more than the threshold pixel intensity. These transitions
#' are used to define the plate edges. For best results, the grid should be
#' approximately square to the edge of the image. Plate positions are numbered
#' from left to right, top to bottom.
#'
#' @return \code{rough_crop} returns a dataframe with the following columns:
#'
#' \item{position}{Integer position of plate in grid (row-wise).}
#' \item{top}{The rough top edge of the plate.}
#' \item{left}{The rough left edge of the plate.}
#' \item{right}{The rough right edge of the plate.}
#' \item{bot}{The rough bottom edge of the plate.}

rough_crop <- function(img, thresh, invert, pad) {

  if (EBImage::colorMode(img)) img <- EBImage::channel(img, 'luminance')
  small <- EBImage::resize(img, h = ncol(img) / 10) > 0.5

  rows <-
    rowSums(small) %>%
    find_valleys(thr = thresh * nrow(small), invert = invert) %>%
    mutate_each(funs(. * 10), -n) %>%
    rename_(x_plate = ~mid, plate_row = ~n)

  cols <-
    colSums(small) %>%
    find_valleys(thr = thresh * ncol(small), invert = invert) %>%
    mutate_each(funs(. * 10), -n) %>%
    rename_(top = ~left, bot = ~right, y_plate = ~mid, plate_col = ~n)

  # Generate all combinations of detected rows and columns
  expand.grid(rows$plate_row, cols$plate_col) %>%
    rename_(plate_row = ~Var1, plate_col = ~Var2) %>%
    left_join(rows, by = 'plate_row') %>%
    left_join(cols, by = 'plate_col') %>%
    mutate(position = 1:n()) %>%
    select_(~position, ~plate_row, ~plate_col, ~x_plate, ~y_plate, ~everything()) %>%
    mutate_(
      # Add padding if desired
      left  = ~left - pad[1],
      right = ~right + pad[2],
      top   = ~top - pad[3],
      bot   = ~bot + pad[4],
      # Limit edges if they excede dimensions of image after padding
      left  = ~ifelse(left < 1, 1, left),
      right = ~ifelse(right > nrow(img), nrow(img), right),
      top   = ~ifelse(top < 1, 1, top),
      bot   = ~ifelse(bot > ncol(img), ncol(img), bot)
    )
}



# ---- Rotate ----
#' Determine plate rotation angle
#'
#' Uses a Radon transform to determine the angle that aligns the colony grid
#'
#' @param img A rough cropped plate image
#' @param rough Rough angle in degrees clockwise with which to rotate plate
#' @param step Degree increment used in fine rotation
#'
#' @details The implementation of this function was adapted from Gitter
#' (see \href{http://www.ncbi.nlm.nih.gov/pubmed/24474170}{PMID:24474170})
#'
#' @importFrom EBImage rotate fillHull dilate makeBrush resize
#'
#' @export

grid_angle <- function(img, rough, range, step) {

  # Apply rough rotate
  rotated  <- rotate(img, rough)
  small <- fillHull(dilate(resize(rotated, h = 200), makeBrush(11)))

  # Determine rough rotation angle
  range <- seq(-range / 2, range / 2, by = step)
  variance <- sapply(range, function(angle) {
    #sum(rowSums(rotate(small, angle))^2)
    var(rowSums(rotate(small, angle)))
  })

  #angle <- range[which.min(variance)]
  spline <- predict(smooth.spline(range, variance), seq(-15, 15, by = 0.01))
  angle <- spline$x[which.min(spline$y)]

  return(rough + angle)
}



# ---- Fine Crop ----
#' Fine crop plates
#'
#' Fine crop plates
#'
#' @param img Image
#' @param rotate Rotate
#' @param invert Invert
#' @param pad Pad

fine_crop <- function(img, rotate, range, step, pad, invert) {

  # Invert plate image, blur, threshold and label objects
  if (invert) neg <- max(img) - img else neg <- img
  obj <-
    EBImage::gblur(neg, sigma = 6) %>%
    EBImage::thresh(w = 15, h = 15, offset = 0.05) %>%
    EBImage::bwlabel()

  # Calculate object features, identify dubious objects and remove them
  feat <- object_features(obj)
  crap <-
    feat %>%
    filter_(~
              area  > mean(area) + (3 * mad(area)) |
              area  < 10 |
              eccen > 0.8 |
              ndist > mean(ndist) + (3 * mad(ndist))
    )
  clean <- EBImage::rmObjects(obj, crap$obj) > 0

  # Determine rotation angle and rotate plate
  angle <- grid_angle(clean, rotate, range = range, step = step)
  rotated <- rotate(clean, angle)

  # Split objects that cross rough grid lines
  cols <- grid_breaks(rotated, 'col')
  rows <- grid_breaks(rotated, 'row')

  # Construct fine crop data
  data_frame(
    rotate = angle,
    left   = head(cols, 1) - pad[1],
    right  = tail(cols, 1) + pad[2],
    top    = head(rows, 1) - pad[3],
    bot    = tail(rows, 1) + pad[4]
  ) %>%
    mutate_(
      # Limit edges if they excede dimensions of image after padding
      fine_left  = ~ifelse(left < 1, 1, left),
      fine_right = ~ifelse(right > nrow(rotated), nrow(rotated), right),
      fine_top   = ~ifelse(top < 1, 1, top),
      fine_bot   = ~ifelse(bot > ncol(rotated), ncol(rotated), bot)
    ) %>%
    select_(~rotate, ~matches('fine'))
}



# ---- Object Features ----
#' Compute Object Features
#'
#' Adapted from \link[EBImage]{computeFeatures}
#'
#' @importFrom spatstat nndist nnwhich
#' @export

object_features <- function(img) {

  # Get labeled object image matrix (img should be result of EBImage::bwlabel)
  m <- EBImage::imageData(img)

  # Compute size features based on object contour
  radii <-
    EBImage::ocontour(m) %>%
    lapply(as.data.frame) %>%
    bind_rows(.id = 'obj') %>%
    mutate_(obj = ~as.integer(obj)) %>%
    group_by(obj) %>%
    mutate_(radius = ~sqrt((V1 - mean(V1))^2 + (V2 - mean(V2))^2)) %>%
    summarise_(
      perimeter   = ~n(),
      radius_mean = ~mean(radius),
      radius_sd   = ~sd(radius),
      radius_min  = ~min(radius),
      radius_max  = ~max(radius)
    )

  # Split object indices
  z  <- which(as.integer(m) > 0L)
  objs <- split(z, m[z])

  # Number of pixels in each object
  m00 <- sapply(objs, length)

  # Row sums
  w   <- row(m) * 1
  m10 <- sapply(objs, function(z) sum(w[z]))

  # Col sums
  w   <- col(m) * 1
  m01 <- sapply(objs, function(z) sum(w[z]))

  # Row bias
  w   <- (row(m)^2) * 1
  m20 <- sapply(objs, function(z) sum(w[z]))

  # Col bias
  w   <- (col(m)^2) * 1
  m02 <- sapply(objs, function(z) sum(w[z]))

  w   <- (col(m) * row(m)) * 1
  m11 <- sapply(objs, function(z) sum(w[z]))

  data_frame(
    obj   = 1:length(objs),
    x     = m10 / m00,
    y     = m01 / m00,
    area  = m00,
    mu20  = m20 / m00 - x^2,
    mu02  = m02 / m00 - y^2,
    mu11  = m11 / m00 - x * y,
    det   = sqrt(4 * mu11^2 + (mu20 - mu02)^2),
    theta = atan2(2 * mu11, (mu20 - mu02)) / 2,
    major = sqrt((mu20 + mu02 + det) / 2) * 4,
    minor = sqrt((mu20 + mu02 - det) / 2) * 4,
    eccen = sqrt(1 - minor^2 / major^2),
    ndist = nndist(x, y),
    nwhich = nnwhich(x, y)
  ) %>%
    left_join(radii, by = 'obj') %>%
    select_(~obj, ~x, ~y, ~area, ~perimeter, ~radius_mean, ~radius_max,
            ~radius_min, ~eccen, ~theta, ~major, ~minor, ~ndist, ~nwhich)
}



# ---- Grid Breaks ----
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
#' @export

grid_breaks <- function(img, type = c('col', 'row'), thresh = 0.03, edges = c('inner', 'outer', 'mid')) {

  if (grepl('col', type[1], ignore.case = T)) {
    # Rows of matrix correspond to x axis (i.e. columns)
    valleys <- find_valleys(rowSums(img), thr = thresh * nrow(img))
  } else if (grepl('row', type[1], ignore.case = T)) {
    # Columns of matrix correspond to y axis (i.e. rows)
    valleys <- find_valleys(colSums(img), thr = thresh * ncol(img))
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


# ---- CM engine Utilities ----

# Label continuous runs with an integer beginning with 1
label_runs <- function(x) {
  runs <- rle(x)$lengths
  rep.int(1:length(runs), runs)
}

# Find the midpoint, left, and right edge of regions of continuous low values
find_valleys <- function(y, thr, invert = F) {

  # Identify and label valleys
  is_peak <- y > thr
  if (invert) is_peak <- !is_peak # Invert peak definition if desired
  regions <- label_runs(is_peak)  # Label consecutive low (F) or high (T) runs
  regions[is_peak] <- 0           # Label peak regions as 0
  valleys <- unique(regions)
  valleys <- valleys[which(valleys != 0)]

  # Find left, middle, and right edge of valley
  bind_rows(
    lapply(valleys, function(v) {
      x <- which(regions == v)
      data_frame(left = min(x), mid = mean(x), right = max(x))
    })
  ) %>%
    mutate(n = 1:n())
}
