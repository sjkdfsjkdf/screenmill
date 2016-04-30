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

  # Find screenmill-annotations file in directory
  dir <- gsub('/$', '', dir)
  target <- gsub('/$|^\\./', '', target)
  full_target <- paste0(dir, '/', target)
  if (is.dir(dir)) {
    path <- paste(dir, 'screenmill-annotations.csv', sep = '/')
  } else {
    path <- dir
  }
  if (!file.exists(path)) {
    stop('Could not find ', path, '. Please annotate and calibrate ',
         'plates before cropping. See ?annotate and ?calibrate for ',
         'more details.')
  }

  # Read screenmill plate annotations
  screenmill <- screenmill_annotations(path)

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

    img <- imageData(img)

    # Apply Crop calibration
    cores <- max(2L, detectCores(), na.rm = T)
    mclapply(1:nrow(coords), function(p) {
      rough   <- with(coords, img[ left[p]:right[p], top[p]:bot[p] ])
      rotated <- rotate(rough, coords$rotate[p])
      fine    <- with(coords, rotated[ fine_left[p]:fine_right[p], fine_top[p]:fine_bot[p] ])
      EBImage::writeImage(
        fine,
        paste0(dir, '/', coords$img_crop[p]),
        type = 'tiff',
        compression = 'none',
        bits.per.sample = 8L
      )
    }, mc.cores = cores)
    progress$tick()$print()
  }

  write_csv(data, path)
  message('Finished!')
  return(invisible(dir))
}


#' @importFrom parallel mclapply
#' @export

measure_colonies <- function(dir, invert = TRUE, overwrite = FALSE) {

  # ---- Validate inputs ----
  dir <- gsub('/$', '', dir)  # clean trailing slash
  stopifnot(is.dir(dir), is.flag(invert), is.flag(overwrite))

  plates_path <- paste(dir, 'screenmill-annotations.csv', sep = '/')
  grid_path   <- paste(dir, 'screenmill-grid.csv', sep = '/')
  target      <- paste(dir, 'screenmill-measurements.csv', sep = '/')

  if (!file.exists(plates_path)) stop('Could not find ', plate_path, '. Please annotate and crop plates before calibrating grid (see ?annotate_plates, ?calibrate_crop, ?crop).')
  if (!file.exists(grid_path)) stop('Could not find ', grid_path, '. Please calibrate grid before measuring colonies (see ?calibrate_grid).')
  if (file.exists(target) && !overwrite) {
    message('Colonies have already been measured. Set "overwrite = TRUE" to re-measure.')
    return(invisible(dir))
  }

  # ---- Read screenmill tables ----
  plates <-
    screenmill_annotations(plates_path) %>%
    select(img_crop, grid_template) %>%
    mutate(path = paste(dir, img_crop, sep = '/'))
  grids <- read_csv(grid_path)

  # ---- Track progress and keep user informed ----
  cores <- max(2L, detectCores(), na.rm = T)
  message('Measuring ', nrow(plates), ' plates ("Patience is bitter, but its fruit are sweet") ...')
  time <- Sys.time()

  # ---- Measure grid locations for each cropped plate ----
  result <-
    mclapply(1:nrow(plates), measure_plate, plates, grids, invert, mc.cores = cores) %>%
    bind_rows

  # ---- Write result ----
  message('Writing to ', target, ' ...')
  write_csv(result, target)
  message('Finished in ', format(round(Sys.time() - time, 2)))
  return(invisible(dir))
}


measure_plate <- function(i, plates, grids, inv) {
  img   <- imageData(read_greyscale(plates$path[i], invert = inv))
  tmp_i <- plates$grid_template[i]
  img_i <- plates$img_crop[i]
  grid  <- filter(grids, grid_template == tmp_i) %>% select(-grid_template)

  grid %>%
    mutate(
      img_crop = img_i,
      size = measureColonies(img, l, r, t, b, background)
    ) %>%
    select(img_crop, everything())
}
