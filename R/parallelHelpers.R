#' @export
tryDoParallelCluster <- function(nCores){
    packs <- requireNamespace(c('foreach', 'doParallel'), quietly = TRUE)
    if (all(packs)) {
      library(foreach)
      library(doParallel)
      cl <- makeCluster(nCores)
      registerDoParallel(cl)
      #     registerDoRNG(456)
      return(list(success = TRUE, handle = cl))
    } else {
      warning("Packages foreach and doParallel not found to do parallel processing")
      return(list(success = FALSE, handle = NULL))    }
}
