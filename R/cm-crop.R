# ---- Crop -------------------------------------------------------------------
#' Crop plates
#'
#' Crop plates from images.
#'
#' @param dir Directory of images to crop.
#' @param target Target directory for saving images.
#' @param overwrite Should existing cropped images be overwritten?
#' Defaults to \code{FALSE}.
#'
#' @importFrom stringr str_pad
#' @export

crop <- function(dir, target = 'cropped', overwrite = FALSE) {

  # Find screenmill-plates file in directory
  dir <- gsub('/$', '', dir)
  target <- gsub('/$|^\\./', '', target)
  full_target <- paste0(dir, '/', target)
  if (is.dir(dir)) {
    path <- paste(dir, 'screenmill-plates.csv', sep = '/')
  } else {
    path <- dir
  }
  if (!file.exists(path)) {
    stop('Could not find ', path, '. Please annotate and calibrate ',
         'plates before cropping. See ?annotate_plates and ?calibrate_crop for ',
         'more details.')
  }

  # Read screenmill plate annotations
  screenmill <- screenmill_plates(path)

  # Check current status of screenmill
  if(!attr(screenmill, 'calibrated')) {
    stop('Cropping has not been calibrated. See ?calibrate_crop for more details.')
  }
  if (attr(screenmill, 'cropped') && !overwrite) {
    message('Images have already been cropped. Set "overwrite = TRUE" to re-crop.')
    return(invisible(dir))
  }

  message('Saving cropped images to ', full_target)
  if (!dir.exists(full_target)) dir.create(full_target)

  data <-
    screenmill %>%
    mutate(
      grp3 = stringr::str_pad(group,     width = 3, side = 'left', pad = 0),
      pos3 = stringr::str_pad(position,  width = 3, side = 'left', pad = 0),
      tim3 = stringr::str_pad(timepoint, width = 3, side = 'left', pad = 0),
      img_crop   = paste0(target, '/', paste(date, grp3, pos3, tim3, sep = '-'), '.tif')
    ) %>%
    select(-grp3, -pos3, -tim3)

  files <- paste0(dir, '/', unique(data$file))
  progress <- progress_estimated(2 * length(files))
  for (file in files) {
    progress$tick()$print()

    # Get data for file
    coords <- data[which(data$file == basename(file)), ]

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
      EBImage::writeImage(fine, paste0(dir, '/', coords$img_crop[p]), type = 'tiff', compression = 'none')
    })
    progress$tick()$print()
  }

  write_csv(data, path)
  message('Finished!')
  return(invisible(dir))
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
