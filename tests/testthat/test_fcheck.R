context("Checking Factors")

test_that("fcheck identifies factors that should be char", {
  x <- as.factor(c("a", "b", "c"))
  y <- as.factor(c("hello", "world"))

  p <- as.factor(c("a", "a", "b", "b"))
  q <- as.factor(c("hello", "hello", "world"))

  r <- 1:10

  expect_identical(fcheck(x), fcheck(y))
  expect_identical(fcheck(p), fcheck(q))
  expect_error(fcheck(r))
})
