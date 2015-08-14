
#' @export
score <- function(target,response, checkDuplicates = FALSE, ignoreVals=NA,
                  index.return = FALSE) {

  if (length(target) != length(response)) {
    stop("Target vector and response vector are not of equal length")
  }

  acc <- vector(mode="numeric",length = length(response))
  position <- rep(NA, length(response))

  for (k in which(grepl('.', response))) {

    # Try hard string matching first
    matches <- response[k] == target
    if (any(matches)) {
      acc[k] <- 1 # response is exact match to target string
      position[k] <- which(matches)

    } else {

      # try fuzzy matching
      fuzzy_matches <- agrepl(response[k], target)
      if (sum(fuzzy_matches) == 1) {
        # A single fuzzy substring match was found
        acc[k] <- 2 # A fuzzy substring match was found
        position[k] <- which(fuzzy_matches)[1]

      } else if (sum(fuzzy_matches) > 1) {

        # Multiple fuzzy substring matches were found
        # to decide where the match is, we just compare the
        # absolute differences in character count
        candidates <- target[fuzzy_matches]
        distance <- abs(nchar(candidates) - nchar(response[k]))

        # pick the winner
        # if they tie then just pick one, it doesnt matter...
        winner <- candidates[distance == min(distance)][1]
        position[k] <- which(target == winner)
        acc[k] <- 2
      }
    }
  }

  if (checkDuplicates) {
    dupes <- duplicated(response,incomparables = ignoreVals)
    acc[dupes] <- 0
    position[dupes]  <- NA_real_
  }

  # Returne the vector of match position if requested
  if (index.return) {
    return(list(accuracy=acc, position = position))
  } else {
    return(acc)
  }
}

