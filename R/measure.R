#' Measure Colonies
#'
#' Measure and save colonies.
#'
#' @param dir Path to directory to process.
#' @param overwrite Should previous measurements be overwritten?
#' @param save.plates Should cropped/rotated plates be saved? Defaults to \code{FALSE}.
#' @param save.colonies Should cropped colonies be saved? Defaults to \code{TRUE}.
#'
#' @importFrom readr read_csv write_csv
#' @importFrom parallel mclapply detectCores
#' @export

measure <- function(dir = '.', overwrite = F, save.plates = F, save.colonies = T) {

  # Validate input
  assert_that(is.dir(dir), is.flag(overwrite), is.flag(save.plates), is.flag(save.colonies))

  # Clean trailing slash from directory input
  dir <- gsub('/$', '', dir)
  ano_path <- file.path(dir, 'screenmill-annotations.csv', fsep = '/')
  crp_path <- file.path(dir, 'screenmill-calibration-crop.csv', fsep = '/')
  grd_path <- file.path(dir, 'screenmill-calibration-grid.csv', fsep = '/')
  target   <- file.path(dir, 'screenmill-measurements.csv', fsep = '/')

  # Stop if plates have not yet been annotated
  if (!(file.exists(crp_path) && file.exists(grd_path))) stop('Could not find calibration files. Please annotate and calibrate before measuring.\nSee ?annotate and ?calibrate for more details.')

  if (!overwrite && file.exists(target)) {
    # Exit if already calibratd and no overwrite
    message('This batch has already been measured Set "overwrite = TRUE" to re-measure.')
    return(invisible(dir))
  } else {
    # Remove pre-existing files
    if (file.exists(target)) file.remove(target)
  }

  # Read metadata
  annot <-
    read_csv(ano_path) %>% mutate(path = file.path(dir, file, fsep = '/')) %>%
    select(path, file, plate_id, template, position)
  paths <- unique(annot$path)
  plates <-
    left_join(annot, read_csv(crp_path), by = c('template', 'position')) %>%
    select(path, plate_id, starts_with('rough'), rotate, starts_with('fine'), invert)
  grids  <-
    left_join(annot, read_csv(grd_path), by = c('template', 'position')) %>%
    group_by(plate_id) %>%
    arrange(row, column, replicate) %>%
    mutate(colony_num = 1:n()) %>%
    arrange(plate_id, colony_num) %>%
    select(
      plate_id,
      strain_collection_id, plate, row, column, replicate,
      colony_row, colony_col, colony_num, l, r, t, b
    ) %>%
    filter(complete.cases(.)) %>%
    ungroup

  # Record start time
  time <- Sys.time()

  # For each image
  message('Measuring ', length(paths), ' images')
  progress <- progress_estimated(length(paths))
  cores <- ifelse(.Platform$OS.type == 'windows', 1, max(1, detectCores(), na.rm = T))
  lapply(paths, function(pth) {

    progress$tick()$print()
    img <- read_greyscale(pth)
    coords <- filter(plates, path == pth)
    plate_ids <- unique(coords$plate_id)

    # For each plate within this image
    measurements <-
      mclapply(plate_ids, function(p) {

        # Crop plates
        crop    <- filter(plates, plate_id == p)
        rough   <- with(crop, img[ rough_l:rough_r, rough_t:rough_b ])
        rotated <- EBImage::rotate(rough, crop$rotate)
        fine    <- with(crop, rotated[ fine_l:fine_r, fine_t:fine_b ])
        if (crop$invert) fine <- 1 - fine

        # Save cropped plate in desired format
        if (save.plates) {
          target <- paste0(dir, '/plates/')
          if (!dir.exists(target)) dir.create(target)
          EBImage::writeImage(
            fine,
            paste0(target, p, '.tif'),
            type = 'tiff',
            compression = 'none',
            bits.per.sample = 8L
          )
        }

        # ---- Measure colonies ----
        grid <-
          # Identify corner intensities
          filter(grids, plate_id == p) %>%
          mutate(
            tl = fine[as.matrix(cbind(l, t))],
            tr = fine[as.matrix(cbind(r, t))],
            bl = fine[as.matrix(cbind(l, b))],
            br = fine[as.matrix(cbind(r, b))],
            bg = apply(cbind(tl, tr, bl, br), 1, mean, trim = 0.5)
          )

        # Predict background intensity via loess smoothing
        grid$background <-
          loess(
            bg ~ colony_row + colony_col,
            data = grid, span = 0.3, normalize = F, degree = 2
          ) %>%
          predict

        result    <- with(grid, measureColonies(fine, l, r, t, b, background))
        grid$size <- result$measurements

        # Save colonies in desired format
        if (save.colonies) {
          target <- paste0(dir, '/colonies/')
          if (!dir.exists(target)) dir.create(target)
          saveRDS(result$colonies, paste0(target, p, '.rds'))
        }

        grid %>%
          select(
            plate_id, strain_collection_id, plate, row, column, replicate,
            colony_row, colony_col, colony_num, background, size
          )
      }, mc.cores = cores) %>%
      bind_rows

    write_csv(measurements, target, append = file.exists(target))
  })

  message('Finished measuring in ', format(round(Sys.time() - time, 2)))
  return(invisible(dir))
}

