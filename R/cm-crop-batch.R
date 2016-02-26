# library(screenmill)
# files        <- list.files(pattern = '\\.tif$')
# template     <- files[which.max(file.info(files)$mtime)]  # latest created file
# names(files) <- template
#
#
# coords <- crop(template)

#' @export

crop_batch <- function(files, coords) {
  message('Saving cropped images to ./crops ...')
  dir.create('crops', showWarnings = FALSE)
  write.csv(coords, 'crops/coordinates.csv')
  templates <- names(files)
  result <- NULL
  for (i in 1:length(files)) {
    message('Cropping ', files[i], '...')
    # Get crop coordinates for designated template
    ci <- filter(coords, file == templates[i])

    # Read as greyscale image
    img <- EBImage::readImage(files[i])
    if (EBImage::colorMode(img)) img <- EBImage::channel(img, 'luminance')

    for (p in ci$position) {
      # Create a file path
      path  <- paste0('crops/', gsub('\\..*?$', '', files[i]), '-' , p, '.tif')
      rough <- with(ci, img[ left[p]:right[p], top[p]:bot[p] ])
      rotated <- EBImage::rotate(rough, ci$rotate[p])
      fine  <- with(ci, rotated[ fine_left[p]:fine_right[p], fine_top[p]:fine_bot[p] ])
      EBImage::writeImage(fine, path, type = 'tiff', bits.per.sample = 8)
      result <- c(result, path)
    }
  }
  return(result)
}

#crop_batch(files, coords)
