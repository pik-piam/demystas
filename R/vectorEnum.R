#' Enumerating vectors
#'
#' Enumarates components of a vector in order to make each component unique. Useful for cases where duplicates of vector components must be avoided.
#'
#' @name vectorEnum
#' @concept demystas
#' @param x a vector with possibly duplicated components
#' @param sep a character indicating how the enumeration and original vector components should be separated. Defaults to "."
#' @return a vector in the same order and dimension as `x` with each component uniquely enumerated
#' @author Atreya Shankar
#' @export
#' @examples
#' \dontrun{
#'
#' x <- c(rep("foo", 10), rep("bat", 25), rep("baz", 10), rep("foo", 10))
#' test <- vectorEnum(x)
#' }

vectorEnum <- function(x, sep = "."){

  if(!is.vector(x)){
    stop("x must be a vector")
  }

  if(!is.character(sep) | length(sep) > 1){
    stop("sep must be a character")
  }

  z <- lapply(unique(x), function(k) which(x == k))

  for(i in 1:length(z)){
    for(j in 1:length(z[[i]])){
      x[z[[i]][j]] <- paste0(x[z[[i]][j]], sep, j)
    }
  }
  return(x)
}
