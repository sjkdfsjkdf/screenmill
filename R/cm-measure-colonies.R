#' @importFrom parallel mclapply

measure_colonies <- function(dir, invert = TRUE, overwrite = FALSE) {

  # ---- Validate inputs ----
  dir <- gsub('/$', '', dir)  # clean trailing slash
  stopifnot(is.dir(dir), is.flag(invert), is.flag(overwrite))

  plates_path <- paste(dir, 'screenmill-plates.csv', sep = '/')
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
    screenmill_plates(plates_path) %>%
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
  setTxtProgressBar(pb, i)
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
