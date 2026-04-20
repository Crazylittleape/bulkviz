#' Discrete Palette for bulkviz
#'
#' Default discrete palette for publication-style plots.
#'
#' @param n Number of colors.
#' @param style Palette style. Currently supports `"default"` and `"nature"`.
#'
#' @return A character vector of hex colors.
#' @export
palette_pub <- function(n = 2, style = "default") {
  palettes <- list(
    default = c("#2F5C85", "#D96B59", "#5C9E6E", "#C9A227", "#7E6AA2", "#4FA3B7"),
    nature = c("#3C5488", "#E64B35", "#00A087", "#F39B7F", "#8491B4", "#91D1C2")
  )

  if (!style %in% names(palettes)) {
    stop("Unsupported `style`. Choose one of: ", paste(names(palettes), collapse = ", "))
  }

  palette <- palettes[[style]]
  rep_len(palette, n)
}
