
#' @export
strSort <- function(x,splitter = '.') {
  sapply(lapply(strsplit(x,splitter,fixed = TRUE), sort), paste, collapse=".")
}
