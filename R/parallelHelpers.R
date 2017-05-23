#' @export
doParallelCluster <- function(nCores){

    packs <- requireNamespace(c('foreach', 'doParallel'), quietly = TRUE)

    if (packs) {
      out <- tempfile(pattern = "parallel", fileext = ".log")
      cluster <- makeCluster(nCores, outfile = out)
      registerDoParallel(cluster)
      return(list(handle = cluster, logfile = out))

    } else {
      warning("Packages foreach and doParallel not found to do parallel processing")
      return(NULL)
    }
}
