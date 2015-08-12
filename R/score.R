score <- function(target,resp,s) {
  for (k in which(grepl('.', resp))) {
    # Try hard string matching first
    acc <- resp[k] == target[k]
    if (acc) {
      s[k] <- 1 # response is exact match to target string
    } else { 
      # try fuzzy matching 
      inds <- agrepl(resp[k], target[k])
      if (inds) {
        s[k] <- 2 # A fuzzy substring match was found
      } else  {
        s[k] <- 0 # hard and fuzzy matching failed
        
      }
    }
  }

  return(as.numeric(s))
}
