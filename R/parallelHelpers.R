#' @export
doParallelCluster <- function(nCores){

    packs <- requireNamespace(c('foreach', 'doParallel'), quietly = TRUE)

    if (all(packs)) {
      library(foreach)
      library(doParallel)
      out <- tempfile(pattern = "parallel", fileext = ".log")
      cluster <- makeCluster(nCores, outfile = out)
      registerDoParallel(cl)
      return(list(cluster,logfile = out))

    } else {
      warning("Packages foreach and doParallel not found to do parallel processing")
      return(NULL)
    }
}
