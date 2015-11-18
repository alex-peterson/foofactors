#' Check if factor type is appropriate
#'
#' This function checks to see if char type may be more appropriate.
#' It returns TRUE if it looks okay, and FALSE if the number of levels
#' is equal to the length of the vector.
#'
#' @param x the factor
#'
#' @return logical
#' @export
#'
#' @examples
#' x <- as.factor(c("cat", "dog", "fish", "blob"))
#' fcheck(x)
#' fcheck(iris$Species)
fcheck <- function(x) {
  if(length(x) == nlevels(x)) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}
