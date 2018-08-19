#' Load Knitr Cache
#' @description Loads the packages and objects stored in a knitr cache. Useful for restoring the
#' internal state of a knitted document for use in an interactive session without forcing you to
#' re-run all the code interactively.
#'
#' @param path The path to the knitr cache directory.
#' @param load_packages A logical scalar controlling whether the packages loaded when the document was knit
#' should be loaded now. Defaults to \code{TRUE}, which loads the packages.
#' @param eval_promises A logical scalar controlling whether the cached objects are loaded as promises,
#' (as created by \link{lazyLoad}) or whether the promises are evaluated. Defaults to \code{TRUE}, which
#' evaluates promises. This may be a slow process if there are many large cached objects.
#' @param envir The environment to load the cached objects and packges into.
#' Defaults to the current calling environment
#'
#' @return Returns `NULL` invisibly
#' @export
#'
#' @details
#' The knitr cache directory is usually stored in the same directory as the document. The knitr cache directory naming schema is
#' `$DOCUMENTNAME$_cache/$OUTPUTFORMAT$/`. So, if your document named "Analysis.Rmd" was knit to HTML
#' and cached, then the cache would be stored at `Analysis_cache/html/`.
#'
#' @examples
#' \dontrun{
#' load_knitr_cache("Analysis_cache/html")
#' }
load_knitr_cache <- function(path, eval_promises = TRUE, load_packages = TRUE,
                             envir = parent.frame()) {

  packages_file <- file.path(path, "__packages")
  if (file.exists(packages_file) && load_packages) {
    packages <- readLines(con = packages_file)
    for (p in packages) {
      library(p, character.only = TRUE)
    }
  }
  cache_names <- gsub(pattern = "\\.RData", "",
                      list.files(path = path, pattern = "*.RData", full.names = TRUE)
                      )
  for (cache in cache_names) {
    lazyLoad(cache, envir)
  }

  if (eval_promises) {
    sapply(ls(envir), get)
  }

  return(invisible())
}
