# ---- Crop -------------------------------------------------------------------
#' Crop plates
#'
#' Crop plates from images.
#'
#' @param dir Directory of images to process.
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
      rotated <- EBImage::rotate(rough, coords$rotate[p])
      fine    <- with(coords, img[ fine_left[p]:fine_right[p], fine_top[p]:fine_bot[p] ])
      EBImage::writeImage(fine, paste0(dir, '/', coords$img_crop[p]), type = 'tiff')
    })
    progress$tick()$print()
  }

  write_csv(data, path)
  message('Finished!')
  return(invisible(dir))
}
