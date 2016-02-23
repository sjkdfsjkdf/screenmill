library(EBImage)
library(screenmill)
library(dplyr)
library(tidyr)


path <- '20160216-00012.tif'
options("EBImage.display" = 'raster')
img <- readImage(path)

crop <- rough_crop(img)

# Plate processing

# Rough crop a plate
pos <- 5
plate <- with(crop, img[(rough_left[pos]:rough_right[pos]), (rough_top[pos]:rough_bot[pos])])

# Invert plate image
plate_neg <- max(plate) - plate
plate_blur <- gblur(plate_neg, sigma = 6)

# Threshold image and label objects
plate_th <- thresh(plate_blur, w = 15, h = 15, offset = 0.05)
objects <- bwlabel(plate_th)

# Calculate object features and identify spurious objects
features <- object_features(objects)

spurious <-
  features %>%
  filter(
    area > 500 | area < 10 | perimeter > 100 |
    ndist > mean(ndist) + (4 * sd(ndist))
  )

colonies <- filter(features, !(obj %in% spurious$obj))

# Remove spurious objects
plate_clean <- rmObjects(objects, spurious$obj)

# Determine rotation angle and rotate plate
angle <- plate_rotate_angle(plate_clean > 0, 90)
plate_rot <- bwlabel(rotate(plate_clean, angle))
features <- object_features(plate_rot)

# Map objects to rows and columns
colony_grid <-
  features %>%
  mutate(
    colony_row = cut(y, grid_breaks(plate_rot > 0, 'row'), labels = FALSE),
    colony_col = cut(x, grid_breaks(plate_rot > 0, 'col'), labels = FALSE)
  ) %>%
  # If multiple objects map to one location on the grid keep one near average
  group_by(colony_row) %>%
  mutate(mean_y = mean(y)) %>%
  group_by(colony_col) %>%
  mutate(mean_x = mean(x)) %>%
  group_by(colony_row, colony_col) %>%
  summarise(x = min(x), y = min(y)) %>%
  ungroup %>%
  complete(colony_row, colony_col) %>%
  group_by(colony_row) %>%
  arrange(colony_col) %>%
  mutate(
    na = is.na(y),
    y_spline = predict(smooth.spline(colony_col[!na], y[!na]), colony_col)[[2]]
  ) %>%
  group_by(colony_col) %>%
  arrange(colony_row) %>%
  mutate(
    na = is.na(x),
    x_spline = predict(smooth.spline(colony_row[!na], x[!na]), colony_row)[[2]]
  ) %>%
  ungroup



display(plate_rot)
abline(v = col_valleys$mid, col = 'red')
abline(h = row_valleys$mid, col = 'red')

display(rotate(plate, angle))
abline(v = col_valleys$mid, col = 'red')
abline(h = row_valleys$mid, col = 'red')
