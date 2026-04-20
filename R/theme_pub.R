#' Publication Theme for bulkviz
#'
#' A clean, publication-oriented ggplot2 theme used across bulkviz plots.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A ggplot2 theme object.
#' @export
theme_pub <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(color = "black", face = "plain"),
      axis.text = ggplot2::element_text(color = "black"),
      legend.title = ggplot2::element_text(color = "black"),
      legend.text = ggplot2::element_text(color = "black"),
      plot.title = ggplot2::element_text(face = "bold", color = "black"),
      plot.subtitle = ggplot2::element_text(color = "black"),
      strip.text = ggplot2::element_text(face = "bold", color = "black")
    )
}
