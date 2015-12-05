
#' @export
strSort <- function(x,splitter = '.') {
  if (identical(class(x),"factor")) {
    x <- as.character(x)
  }
  sapply(lapply(strsplit(x,splitter,fixed = TRUE), sort),
         paste, collapse=splitter)
}
