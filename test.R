library(EBImage)
library(screenmill)
library(dplyr)
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
angle <- plate_rotate_angle(plate_clean, 90)
plate_rot <- bwlabel(rotate(plate_clean, angle))
features <- object_features(plate_rot)

plot(smooth.spline(rowMeans(plate_rot)), type = 'l')
dist(f)
cut(features$x, 48)

# Fine crop



display(plate_rot)

display(plate_neg)
display(plate_blur)
display(rotate(plate, 90))
display(rotate(plate, angle))
display(plate_th)
