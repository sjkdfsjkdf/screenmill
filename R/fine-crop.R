


# path <- '2016-02-16 11:17:29.tif'
# spar <- 0.4
# thresh <- 0.5
# library(EBImage) # readImage colorMode Grayscale
# library(magrittr)
# library(dplyr) # %>% last



# ---- Fine Crop Utility Functions ----

fine_crop <- function(img) {
  img_blur <- gblur(img, sigma = 6)
  img_blur_neg <- max(img_blur) - img_blur
  img_th  <- thresh(img_blur_neg, w = 15, h = 15, offset = 0.05)
  obj <- bwlabel(img_th)

  #
  obj_features <-
    obj %>%
    computeFeatures.moment %>%
    as.data.frame %>%
    mutate(
      object = 1:n(),
      # Group objects whose centers are within 10 pixels
      x_group = as.integer(round(m.cx, digits = -1)),
      y_group = as.integer(round(m.cy, digits = -1))
    )

  column <-
    obj_features %>%
    count(x_group) %>%
    filter(n > 20) %>%
    arrange(x_group) %>%
    extract2('x_group')

  lr_edges <-
    inner_join(
      filter(obj_features, x_group == head(column, 1)) %>% select(xl = m.cx, yl = m.cy, y_group),
      filter(obj_features, x_group == tail(column, 1)) %>% select(xr = m.cx, yr = m.cy, y_group),
      by = 'y_group'
    ) %>%
    mutate(
      A = xr - xl,
      B = yl - yr,
      angle = atan(B / A) * (180 / pi)
    )
  median(lr_edges$angle)
  img_rot <- rotate(img, median(lr_edges$angle))
  display(img)
  display(img_rot)
}