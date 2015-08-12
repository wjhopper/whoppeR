
#' @export
score <- function(target,response, checkDuplicates = FALSE) {

  if (length(target) != length(response)) {
    stop("Target vector and response vector are not of equal length")
  }

  acc = vector(mode="numeric",length = length(target))

  for (k in which(grepl('.', response))) {
    # Try hard string matching first
    if (response[k] == target[k]) {
      acc[k] <- 1 # response is exact match to target string
    } else {
      # try fuzzy matching
      if (agrepl(response[k], target[k])) {
        acc[k] <- 2 # A fuzzy substring match was found
      } else  {
        acc[k] <- 0 # hard and fuzzy matching failed
      }
    }
  }

  # check for duplicate responses, both correct and incorrecct
  if (checkDuplicates){
    dupes <- duplicated(response)
    acc[dupes] <- FALSE
    return(list(accuracy=acc, duplicates = which(dupes)))
  } else {
    return(acc)
  }
}
