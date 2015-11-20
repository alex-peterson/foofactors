#' Reorder Factor Levels
#'
#'This function reorders a factor in a data.frame based
#'off of another (quanitative) variable in that data.frame. The default function
#'is mean, although other functions can be passed. And the default behavior is
#'descending order, although it can be changed to ascending with desc = FALSE.
#'
#' @param df dataframe
#' @param factor factor
#' @param var variable for sorting
#' @param desc logical
#'
#' @return factor
#' @export
#' @examples
#' iris$Species <- forder(iris, iris$Species, iris$Sepal.Width, desc = FALSE)
#' boxplot(Sepal.Width ~ Species, data = iris)
forder <- function(df, factor, var, fun = mean, desc = TRUE) {
  if (class(df) != "data.frame") {
    stop("Error: this function can only take data.frames as input\n",
         "You have provided an object of class ", class(df))
  }
  if (nlevels(df) == 1) {
    stop("Error: your factor only has 1 level, no reason to reorder.")
  }
  if (desc == TRUE) {
  return(factor <- dplyr::desc(stats::reorder(factor, var, fun)))
  }
  else {
  return(factor <- stats::reorder(factor, var, fun))
}
}
