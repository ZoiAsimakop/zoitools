#' Make a nice histogram
#'
#' Draws a histogram of a numeric vector in a clean style, with a dashed
#' line at the median and the median value shown in the subtitle.
#'
#' @param x A numeric vector.
#' @param title The plot title.
#' @param xlab The label for the x-axis.
#' @param fill The colour of the bars.
#' @param bins The number of bars.
#' @return A ggplot object.
#' @examples
#' nice_hist(rnorm(500, mean = 100, sd = 15), title = "Random scores")
#' @export
nice_hist <- function(x, title = "Histogram", xlab = "Value",
                      fill = "#5B8DB8", bins = 30) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }

  plot_data <- data.frame(value = x)
  mid <- stats::median(x, na.rm = TRUE)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = bins, fill = fill, colour = "white") +
    ggplot2::geom_vline(xintercept = mid, linetype = "dashed", linewidth = 0.8) +
    ggplot2::labs(
      title = title,
      subtitle = paste("Median:", round(mid, 2)),
      x = xlab,
      y = "Count"
    ) +
    ggplot2::theme_bw()
}
