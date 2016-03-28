library(screenmill)

# Current pipeline
where <- 'test-scans'
annotate_plates(where)
calibrate_crop(where)
annotate_plates(where, overwrite = T)
calibrate_crop(where, overwrite = T)

###-------- This was in Fine crop, it splits the grid to identify and parse objects -------------
rotated[cols1, ] <- 0
rotated[, rows1] <- 0

# Re-compute objects
obj2  <- EBImage::bwlabel(rotated)
feat2 <- object_features(obj2)
if (inherits(prog, 'Progress')) prog$tick()$print()

# Map objects to rows and columns
grid1 <-
  feat2 %>%
  mutate_(
    colony_row = ~cut(y, rows1, labels = FALSE),
    colony_col = ~cut(x, cols1, labels = FALSE)
  )

# Identify dubious features and remove (too few in row/col or outside range)
crap_col <- count_(grid1, ~colony_col) %>% filter_(~n < n() / 4)
crap_row <- count_(grid1, ~colony_row) %>% filter_(~n < n() / 4)
crap2 <-
  grid1 %>%
  filter_(
    ~colony_col %in% crap_col$colony_col |
      colony_row %in% crap_row$colony_row |
      !complete.cases(.)
  )
clean2 <- EBImage::rmObjects(obj2, crap2$obj) > 0

# Re-compute grid
cols2 <- grid_breaks(clean2, 'col')
rows2 <- grid_breaks(clean2, 'row')



## -------------------- Re-imagine fine-cropping -------------------------------
# Blur threshold and label inverted image
neg <- max(plates[[4]]) - plates[[4]]
obj1 <-
  EBImage::gblur(neg, sigma = 5) %>%
  EBImage::thresh(w = 15, h = 15, offset = 0.05) %>%
  EBImage::bwlabel()

display(obj1)

# Compute object features to remove crap
feat1 <- object_features(obj1)
crap1 <-
  feat1 %>%
  filter_(~
    area  > mean(area) + (3 * mad(area)) |
    area  < 10 |
    eccen > 0.8 |
    ndist > mean(ndist) + (3 * mad(ndist))
  )
clean1 <- EBImage::rmObjects(obj1, crap1$obj) > 0

plate_rotate_angle(clean1, 90)
display(rotate(clean1, 89.6))
rotated  <- rotate(clean1, 93)
rot_fill <- fillHull(dilate(rotated, makeBrush(21)))
small <- resize(rot_fill, h = 200)

# Rough Rotate
rough_range <- seq(-15, 15, by = 0.2)

rough <- sapply(rough_range, function(angle) {
  var(rowMeans(rotate(small, angle)))
})


angle <- rough_range[which.max(rough)]

fine_range <- seq(angle - 0.2, angle + 0.2, by = 0.05)
fine <- sapply(fine_range, function(angle) {
  var(rowMeans(rotate(rotated, angle)))
})

fine_angle <- fine_range[which.max(fine)]
plot()
display(rotate(rotated, fine_angle))



# Find approximage edges in clean image
lr <- find_valleys(rowSums(clean1), thr = 0.03 * nrow(clean1), invert = T)
tb <- find_valleys(colSums(clean1), thr = 0.03 * ncol(clean1), invert = T)


feat2 <-
  feat1 %>%
  filter(feat1, !(obj %in% crap1$obj)) %>%
  mutate(dist = mad(ndist), left = min(lr), right = max(lr), top = min(tb), bot = max(tb))

l <- filter(feat2, abs(x - min(lr$mid)) < mad(ndist) / 2)

with(l, points(x, y, col = 'green'))

mean(nndist(head(sort(feat2$x), 20)))
mean(nndist(tail(sort(feat2$x), 20)))
mean(nndist(head(sort(feat2$y), 20)))
tail(sort(feat2$x))
#### Scratch pad
mutate(
  colony_row = as.integer(as.factor(colony_row)),
  colony_col = as.integer(as.factor(colony_col))
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

# fine crop clean
test <- with(fine_crop, rotate(img, angle) %>% extract(left:right, top:bot))
plate <- with(fine_crop, clean2[left:right, top:bot])
obj3  <- bwlabel(plate)
feat3 <- object_features(obj3)