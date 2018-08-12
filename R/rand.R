#' getRandString
#' @description Create a psuedo-random alphanumeric string
#'
#' @param len The number of characters in the string created
#'
#' @return A character vector of length 1
#' @export
#'
#' @details The characters in the string are sampled with a replacement from a
#'  pool of the numbers 0 through 9, A-Z, and a-z, with equal sampling probability for all
#'  items in the pool.
#'
#'  The string returned is not gauranteed to be unique across runs, so take care when using this function
#'  to create unique identifiers (e.g., for files names). Consider packages such as
#'  \href{https://cran.r-project.org/web/packages/ids/}{ids} or \href{https://cran.r-project.org/web/packages/uuid}{uuid} in situations where all generated identifies
#'   must be globally unique.
#'
#' @examples
#' getRandString(len = 12)
#'
getRandString <- function(len=12) {
  string <- paste(sample(c(0:9, LETTERS, letters), len, replace = TRUE),
                  collapse='')
  return(string)
}
