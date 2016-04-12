
#' getRandString
#'
#' @param len
#'
#' @return A character vector of length 1
#' @export
#'
#' @examples
#' getRandString(len = 12)
getRandString <- function(len=12) {
  string <- paste(sample(c(rep(0:9,each=5), LETTERS, letters), len, replace = TRUE),
                  collapse='')
  return(string)
}
