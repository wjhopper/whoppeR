#' String Character Sorting
#' @description Split a character vector on a delimeter, and sort the resulting pieces
#' Useful for sorting the output of `interaction()`.
#' @param x A character vector. Factors or numerics will be coerced with `as.character`
#' @param splitter A character vector which divides the inputs into pieces
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x <- c("zZaAG", 'AtC_Z')
#' strSort(x,splitter="")
#'
strSort <- function(x,splitter = '.') {
  stopifnot((is.numeric(x) || is.character(x) || is.factor(x)))
  if (identical(class(x),"factor") || identical(class(x),"numeric")) {
    x <- as.character(x)
  }
  sapply(lapply(strsplit(x,splitter,fixed = TRUE), sort),
         paste, collapse=splitter)
}

#' Sequential Range
#' @description Change a non-sequential vector of numeric identifiers into
#' a vector of sequential identifiers.
#' @param x A numeric vector
#' @param baseValue Determines the value the identifiers should start at
#'
#' @return A numeric vector
#' @note The minimum value in the input vector x will be the one which takes
#' on the value baseValue, the second smallest value in x will take
#' on the value baseValue + 1, etc.
#'
#' Thus, if the input vector is not ascendingly sorted, the first values in the
#' output will NOT be the lowest (i.e. will not be baseValue)
#'
#' It is recommended that the output not be sorted, as there is no guarantee
#' each unique id value will occur equally often in the input vector, and sorting
#' may cause loss of information if this vector used to index another data structure
#' or as observation identifies in a data frame. However, if you use the resulting vector
#' verbatim to overwrite original ids in a data stucture, you can then sort the entire
#' structure safely with no loss of information.

#' @export
#'
#' @examples
#' ids <- c(rep(5,4),rep(c(6,8),each=6), rep(11,3), rep(3,5))
#' print(ids)
#' new_ids <- sequentialRange(ids)
#' print(new_ids)
#'
#' testDataFrame <- data.frame(ids = testVec, scores = rnorm(length(testVec),5,3))
#' backup <- testDataFrame
#' testDataFrame$ids <- sequentialRange(testDataFrame$ids)
#'
sequentialRange <- function(x,baseValue = 1) {
  # NOTE
  # The minimum value in the input vector x will be the one which takes
  # on the value baseValue, the second smallest value in x will take
  # on the value baseValue + 1, etc
  # So, if the input vector is not ascendingly sorted, the first values in?
  # output will NOT be the lowest (i.e. will not be baseValue)
  stopifnot(all(is.numeric(x)))
  x <- factor(x,labels=baseValue:length(unique(x)))
  x <- as.numeric(levels(x)[x])
  return(x)
}
