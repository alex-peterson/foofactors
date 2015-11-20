#' Reorder Factor Levels
#'
#'This function reorders a factor in a data.frame based
#'off of another (quanitative) variable in that data.frame. The default function
#'is mean, although other functions can be passed.
#'
#' @param df
#' @param factor
#' @param var
#'
#' @return factor
#' @export
#' @examples
#' iris$Species <- forder(iris, iris$Species, iris$Sepal.Width)
#' boxplot(Sepal.Width ~ Species, data = iris)
forder <- function(df, factor, var, fun = mean) {
  if (class(df) != "data.frame") {
    stop("Error: this function can only take data.frames as input\n",
         "You have provided an object of class ", class(df))
  }
  if (nlevels(df) == 1) {
    stop("Error: your factor only has 1 level, no reason to reorder.")
  }
  factor <- stats::reorder(factor, var, fun)
}
