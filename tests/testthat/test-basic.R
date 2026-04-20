test_that("palette_pub returns requested number of colors", {
  cols <- palette_pub(4)
  expect_length(cols, 4)
})

test_that("theme_pub returns a ggplot theme", {
  th <- theme_pub()
  expect_s3_class(th, "theme")
})

test_that("pca_plot returns a ggplot object", {
  set.seed(1)
  mat <- matrix(rnorm(200), nrow = 20, ncol = 10)
  colnames(mat) <- paste0("S", 1:10)
  rownames(mat) <- paste0("G", 1:20)

  meta <- data.frame(
    condition = rep(c("A", "B"), each = 5),
    sample_id = colnames(mat),
    row.names = colnames(mat),
    stringsAsFactors = FALSE
  )

  p <- pca_plot(mat, meta, group = "condition", label = "sample_id")
  expect_s3_class(p, "ggplot")
})
