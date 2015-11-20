context("Reorder Factors")

test_that("forder reorders factors based on another variable", {

  expect_identical(forder(iris$Species, iris$Sepal.Width, desc = TRUE),
                   dplyr::desc(stats::reorder(iris$Species, iris$Sepal.Width)))
  expect_identical(forder(iris$Species, iris$Sepal.Width, desc = FALSE),
                   stats::reorder(iris$Species, iris$Sepal.Width))
  expect_error(forder(iris$Sepal.Length, iris$Sepal.Width))
})
