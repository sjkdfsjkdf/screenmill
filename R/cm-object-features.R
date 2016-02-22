# ---- Compute Object Features ----
#' Compute Object Features
#'
#' Adapted from \link[EBImage]{computeFeatures}
#'
#' @importFrom EBImage imageData
#' @importFrom spatstat nndist nnwhich
#' @importFrom dplyr data_frame select_ bind_rows mutate_ group_by summarise_ left_join
#' @export

object_features <- function(img) {

  # Get labeled object image matrix (img should be result of EBImage::bwlabel)
  m <- imageData(img)

  # Compute size features based on object contour
  radii <-
    ocontour(m) %>%
    lapply(as.data.frame) %>%
    bind_rows(.id = 'obj') %>%
    mutate_(obj = ~as.integer(obj)) %>%
    group_by(obj) %>%
    mutate_(radius = ~sqrt((V1 - mean(V1))^2 + (V2 - mean(V2))^2)) %>%
    summarise_(
      perimeter   = ~n(),
      radius_mean = ~mean(radius),
      radius_sd   = ~sd(radius),
      radius_min  = ~min(radius),
      radius_max  = ~max(radius)
    )

  # Split object indices
  z  <- which(as.integer(m) > 0L)
  objs <- split(z, m[z])

  # Number of pixels in each object
  m00 <- sapply(objs, length)

  # Row sums
  w   <- row(m) * 1
  m10 <- sapply(objs, function(z) sum(w[z]))

  # Col sums
  w   <- col(m) * 1
  m01 <- sapply(objs, function(z) sum(w[z]))

  # Row bias
  w   <- (row(m)^2) * 1
  m20 <- sapply(objs, function(z) sum(w[z]))

  # Col bias
  w   <- (col(m)^2) * 1
  m02 <- sapply(objs, function(z) sum(w[z]))

  w   <- (col(m) * row(m)) * 1
  m11 <- sapply(objs, function(z) sum(w[z]))

  data_frame(
    obj   = 1:length(objs),
    x     = m10 / m00,
    y     = m01 / m00,
    area  = m00,
    mu20  = m20 / m00 - x^2,
    mu02  = m02 / m00 - y^2,
    mu11  = m11 / m00 - x * y,
    det   = sqrt(4 * mu11^2 + (mu20 - mu02)^2),
    theta = atan2(2 * mu11, (mu20 - mu02)) / 2,
    major = sqrt((mu20 + mu02 + det) / 2) * 4,
    minor = sqrt((mu20 + mu02 - det) / 2) * 4,
    eccen = sqrt(1 - minor^2 / major^2),
    ndist = nndist(x, y),
    nwhich = nnwhich(x, y)
  ) %>%
  left_join(radii, by = 'obj') %>%
  select_(~obj, ~x, ~y, ~area, ~perimeter, ~radius_mean, ~radius_max,
          ~radius_min, ~eccen, ~theta, ~major, ~minor, ~ndist, ~nwhich)
}
