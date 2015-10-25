#' Calculate growth parameters
#'
#' Calculates colony growth parameters by fitting a smooth growth curve using
#' \code{\link[stats]{smooth.spline}}.
#'
#' @param time Numeric - time of growth.
#' @param size Numeric - size of colony.
#' @param ... Further arguments passed to \code{\link[stats]{smooth.spline}}.
#'
#' @return Returns a data frame with the following parameters derived from the
#' fit curve:
#' \tabular{ll}{
#'   \bold{A}        \tab Maximum growth. \cr
#'   \bold{A_t}      \tab Time at maximum growth. \cr
#'   \bold{mu}       \tab Maximum growth rate. \cr
#'   \bold{mu_t}     \tab Time at maximum growth rate. \cr
#'   \bold{mu_y}     \tab Growth at maximum growth rate. \cr
#'   \bold{lambda}   \tab Lag phase (x-intercept of line tangent to max growth
#'                        rate). \cr
#'   \bold{b}        \tab y-intercept of line tangent to max growth rate. \cr
#'   \bold{integral} \tab Area under growth curve. \cr
#'   \bold{spar}     \tab Smoothing parameter used to generate curve - can be
#'                        set by passing a \code{spar} argument to
#'                        \code{\link[stats]{smooth.spline}}.
#' }
#'
#' @importFrom dplyr data_frame arrange %>%
#' @importFrom stats smooth.spline predict integrate
#' @export

growth_parameters <- function(time, size, ...) {
  # Ensure data is sorted by time
  data  <- data_frame(time = time, size = size) %>% arrange(time)
  time  <- data$time
  # Time and midpoints used to predict
  t_mid <- c(time, (lag(time) + time) / 2) %>% sort %>% unique

  # Fit a smooth spline curve
  fit <- smooth.spline(time, data$size, ...)
  d0  <- predict(fit, t_mid)             # fit curve
  d1  <- predict(fit, t_mid, deriv = 1)  # first derivative of fit
  i   <- which.max(d1$y)
  x   <- d0$x[i]
  y   <- d0$y[i]
  f   <- function(t) predict(fit, t)$y

  data_frame(
    # The plateau estimated as the maximum of the fitted curve
    A   = max(d0$y),
    A_t = d0$x[which.max(d0$y)], # Time at max size
    # The max slope of the fitted curve (i.e. max of first derivative)
    mu   = d1$y[i],
    mu_t = d0$x[i], # Time at max slope
    mu_y = d0$y[i], # Size at max slope
    # The growth lag is the x intercept for the line tangent to mu
    lambda = -((y - (mu * x)) / mu),
    # The y intercept of the line tangent to mu
    b = y - (mu * x),
    # The integral of the predicted smooth curve
    integral = integrate(f, min(d0$x), max(d0$x))$value,
    # Smoothing parameter
    spar = fit$spar
  )
}
