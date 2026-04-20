#' PCA Plot for Bulk Transcriptomics
#'
#' Draw a publication-oriented PCA plot from an expression matrix and sample
#' metadata, with optional group envelopes and marginal density plots.
#'
#' @param mat Numeric expression matrix with genes in rows and samples in columns.
#' @param meta Data frame containing sample metadata.
#' @param group Column name in `meta` used for color grouping.
#' @param label Optional column name in `meta` used for point labels.
#' @param scale Logical; whether to scale variables before PCA.
#' @param center Logical; whether to center variables before PCA.
#' @param palette Character vector of colors or `NULL` to use [palette_pub()].
#' @param point_size Point size.
#' @param point_alpha Point alpha.
#' @param show_origin_lines Logical; whether to show dashed lines at x = 0 and y = 0.
#' @param xlim Optional numeric vector of length 2 for x-axis limits.
#' @param ylim Optional numeric vector of length 2 for y-axis limits.
#' @param envelope One of `"none"`, `"ellipse"`, or `"hull"`.
#' @param envelope_alpha Fill alpha for ellipse/hull overlays.
#' @param show_marginal_density Logical; whether to add top/right marginal density plots.
#' @param density_alpha Fill alpha for marginal densities.
#' @param density_adjust Bandwidth adjustment for marginal densities.
#' @param legend_position Legend position; accepts standard ggplot2 values or a numeric vector.
#' @param return_data Logical; if `TRUE`, return a list with plot, PCA object, and plotting data.
#'
#' @return A ggplot object, a patchwork object, or a list when `return_data = TRUE`.
#' @export
pca_plot <- function(
  mat,
  meta,
  group,
  label = NULL,
  scale = TRUE,
  center = TRUE,
  palette = NULL,
  point_size = 2.8,
  point_alpha = 1,
  show_origin_lines = TRUE,
  xlim = NULL,
  ylim = NULL,
  envelope = c("none", "ellipse", "hull"),
  envelope_alpha = 0.12,
  show_marginal_density = FALSE,
  density_alpha = 0.7,
  density_adjust = 1.5,
  legend_position = c(0.95, 0.95),
  return_data = FALSE
) {
  envelope <- match.arg(envelope)

  if (!is.matrix(mat) && !is.data.frame(mat)) {
    stop("`mat` must be a matrix or data.frame.")
  }
  mat <- as.matrix(mat)

  if (!is.data.frame(meta)) {
    stop("`meta` must be a data.frame.")
  }

  if (!group %in% colnames(meta)) {
    stop("`group` must be a column in `meta`.")
  }

  if (!all(colnames(mat) %in% rownames(meta))) {
    stop("All sample names in `colnames(mat)` must exist in `rownames(meta)`.")
  }

  if (!is.null(label) && !label %in% colnames(meta)) {
    stop("`label` must be a column in `meta` when provided.")
  }

  meta <- meta[colnames(mat), , drop = FALSE]

  pca <- stats::prcomp(t(mat), center = center, scale. = scale)
  var_explained <- (pca$sdev^2) / sum(pca$sdev^2)

  plot_df <- data.frame(
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    Sample = colnames(mat),
    Group = meta[[group]],
    stringsAsFactors = FALSE
  )
  plot_df$Group <- as.factor(plot_df$Group)

  if (!is.null(label)) {
    plot_df$Label <- meta[[label]]
  }

  n_groups <- length(unique(plot_df$Group))
  if (is.null(palette)) {
    palette <- palette_pub(n_groups)
  }

  base_plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = PC1, y = PC2, color = Group)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::labs(
      x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.justification = c(1, 1),
      legend.title = ggplot2::element_blank()
    )

  if (!is.null(xlim) || !is.null(ylim)) {
    base_plot <- base_plot + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }

  if (show_origin_lines) {
    base_plot <- base_plot +
      ggplot2::geom_hline(yintercept = 0, color = "gray70", linetype = "dashed") +
      ggplot2::geom_vline(xintercept = 0, color = "gray70", linetype = "dashed")
  }

  if (envelope == "ellipse") {
    if (!requireNamespace("ggforce", quietly = TRUE)) {
      stop("Package `ggforce` is required when `envelope = 'ellipse'`.")
    }
    base_plot <- base_plot +
      ggforce::geom_mark_ellipse(
        ggplot2::aes(fill = Group),
        alpha = envelope_alpha,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(values = palette)
  }

  if (envelope == "hull") {
    hull_df <- do.call(
      rbind,
      lapply(split(plot_df, plot_df$Group), function(df) {
        if (nrow(df) < 3) {
          return(df)
        }
        df[grDevices::chull(df$PC1, df$PC2), , drop = FALSE]
      })
    )

    base_plot <- base_plot +
      ggplot2::geom_polygon(
        data = hull_df,
        ggplot2::aes(fill = Group, group = Group),
        alpha = envelope_alpha,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(values = palette)
  }

  if (!is.null(label)) {
    base_plot <- base_plot +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = Label),
        size = 3.5,
        show.legend = FALSE
      )
  }

  final_plot <- base_plot

  if (show_marginal_density) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("Package `patchwork` is required when `show_marginal_density = TRUE`.")
    }

    pc1_range <- if (is.null(xlim)) range(plot_df$PC1, na.rm = TRUE) else xlim
    pc2_range <- if (is.null(ylim)) range(plot_df$PC2, na.rm = TRUE) else ylim

    density_x <- ggplot2::ggplot(plot_df, ggplot2::aes(x = PC1, fill = Group, color = Group)) +
      ggplot2::geom_density(alpha = density_alpha, bw = "nrd", adjust = density_adjust) +
      ggplot2::scale_fill_manual(values = palette) +
      ggplot2::scale_color_manual(values = palette) +
      ggplot2::coord_cartesian(xlim = pc1_range) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    density_y <- ggplot2::ggplot(plot_df, ggplot2::aes(x = PC2, fill = Group, color = Group)) +
      ggplot2::geom_density(alpha = density_alpha, trim = FALSE, bw = "nrd", adjust = density_adjust) +
      ggplot2::scale_fill_manual(values = palette) +
      ggplot2::scale_color_manual(values = palette) +
      ggplot2::coord_cartesian(ylim = pc2_range) +
      ggplot2::coord_flip() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    final_plot <- density_x + patchwork::plot_spacer() + base_plot + density_y +
      patchwork::plot_layout(ncol = 2, widths = c(4, 1), heights = c(1, 4))
  }

  if (return_data) {
    return(list(plot = final_plot, pca = pca, data = plot_df, variance = var_explained))
  }

  final_plot
}
