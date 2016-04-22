library(screenmill)
library(dplyr)
library(readr)

# Current pipeline
where <- 'test-scans'
annotate_plates(where)
calibrate_crop(where)
crop(where)
calibrate_grid(where)
measure_colonies(where, overwrite = T)

grid <- read_csv('test-scans/screenmill-grid.csv') %>%
  filter(grid_template == 'cropped/2016-02-13-001-001-065.tif')

img <- EBImage::readImage('test-scans/cropped/2016-02-13-001-001-065.tif')

EBImage::display(img, method = 'raster')
with(grid, segments(l, t, r, t, col = 'red'))
with(grid, segments(l, b, r, b, col = 'red'))
with(grid, segments(l, t, l, b, col = 'red'))
with(grid, segments(r, t, r, b, col = 'red'))
