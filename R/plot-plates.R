#------------------------------------------------------------------------------
#' Plot plate as heatmap
#'
#' This function will plot plates ~ queries for a desired metric with forced
#' upper and lower bounds
#'
#' @param data A data frame
#' @param fill String. Name of fill column.
#' @param row String. Name of row column.
#' @param col String. Name of column column.
#' @param plate String. Name of plate column. Defaults to 'plate'.
#' @param query String. Name of query column. Defaults to 'query_name'.
#' @param upper Scalar. Upper limit. Defaults to 1.
#' @param mid Scalar. Middle of scale. Defaults to 0.
#' @param lower Scalar. Lower limit. Defaults to -1.
#'
#' @return A ggplot object
#'
#' @importFrom dplyr %>% mutate data_frame
#' @importFrom ggplot2 ggplot geom_tile aes %+% facet_grid scale_fill_gradient2
#' @export


plot_plates <- function(data, fill, row, col, plate = 'plate',
                        query = 'query_name', upper = 1, mid = 0, lower = -1) {
  # These fields are required in `data`
  data_frame(
    row   = data[[row]],
    col   = data[[col]],
    fill  = data[[fill]],
    plate = data[[plate]],
    query = data[[query]]
  ) %>%
    mutate(
      # Row factor is reordered to place the highest row at the bottom
      row = row %>% ordered(levels = sort(unique(.), decreasing = TRUE)),
      # Bounds are set
      fill = ifelse(fill < lower, lower, ifelse(fill > upper, upper, fill))
    ) %>%
    ggplot() %+%
    geom_tile(aes(x = col, y = row, fill = fill)) %+%
    facet_grid(plate ~ query) %+%
    # Three color options
    scale_fill_gradient2(
      midpoint = mid,
      low = "turquoise1", mid = "black", high = "yellow", na.value = "grey50",
      name = '',
      space = 'Lab' # for ggplot2 v1.0.1 and scales 0.3.0 compatibility
    ) %+%
    # Remove unnecessary plot elements
    theme(
      axis.title = element_blank(), axis.ticks = element_blank(),
      axis.line = element_blank(),panel.background = element_blank()
    ) %+%
    # Fix the scale of rows and columns
    coord_fixed()
}