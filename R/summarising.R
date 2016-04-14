#' @import dplyr
#' @importFrom lazyeval lazy_dots
#'
#' @title Hierarchical Summary
#' @description Summarise a data frame at cascading levels of granularity
#'
#' @param collapse
#'  A character vector of variables to group the data frame by,
#'  which will be succesively collapsed over.
#' @param hold
#'  A character vector of variables to consistently group the data frame by,
#'  even while other variables are collapsed
#' @param rawData
#'  The data frame to summmarise iteratively
#' @param ...
#'  Name-value pairs of summary functions like mean(), sd() etc.
#'  See details for warnings about naming conventions
#' @return
#'  A list of tbl_df's, the same length as the character vector passed as the
#'  \code{collapse} argument
#' @export
#'
#' @examples
#' derp <- heirarchicalSummary(collapse = c("subject","group"),
#'                             hold = c("practice","other_type",
#'                                      "prac_score", "other_prac_acc"),
#'                             rawData=ungroup(cbind(LB4L_allSs,n=1)),
#'                             weighted.mean(final_score,n), mean(final_score),
#'                             n = sum(n))
#' str(derp)
#'
heirarchicalSummary <- function(collapse, hold,
                                rawData, ...) {

  summaryFcn <- function(x,  ...) {
    group_by_(x, .dots = lapply(...,as.symbol)) %>%
      summarise_(.dots=fcns)
  }

  fcns <- lazyeval::lazy_dots(...)
  unnamed_fncs <- sapply(names(fcns),identical,"")
  if (any(unnamed_fncs)) {
    fun_arg_pairs <- lapply(lapply(fcns,
                                   `[[`, 'expr'),
                            as.character)
    proposed_names <- unlist(lapply(fun_arg_pairs,`[[`, 2))
    dup_names <- duplicated(proposed_names)
    proposed_names[dup_names] <- ''
    proposed_names[!unnamed_fncs] <- names(fcns)[!unnamed_fncs]
    fcns <- setNames(fcns,proposed_names)
  }
  fcns <- c(fcns, lazyeval::lazy_dots(groupsize=n()))
  index <- mapply(seq, 1:length(collapse), length(collapse))
  gvars <- lapply(index,function(x) c(collapse[x], hold))
  out <- Reduce(summaryFcn, x = c(list(rawData),gvars), accumulate = TRUE)
  out <- setNames(out[-1], collapse)
  return(out)
}



#' @title Performance Bins
#' @description Summarise a variables by groups and bin the observations
#'
#' @param data
#'  data frame to work with
#' @param bin_by
#'  Character vector defining the variables to group the data by
#' @param cutpoints
#'  numeric vector of value to bin the measured variable at. Values not within the range
#'  of this vector will be grouped into the NA category. By default, data is binned
#'  into quartiles.
#' @param ...
#'  Name-value pairs of summary functions like mean(), sd() etc.
#' @return
#'  An ungrouped table df summmarising the observed count and frequency in each bin
#' @export
#'
#' @examples
#' filter(LB4L_allSs, list==1) %>%
#' performanceBins(bin_by = "subject",
#'                 cutpoints=c(0,.25, .75,1),
#'                 acc=mean(final_score))
#'
performanceBins <- function(data, bin_by,
                            cutpoints = c(0,.25,.5,.75,1), ...) {

  fcns <- lazyeval::lazy_dots(...)
  binned <- data %>%
    group_by_(.dots = bin_by) %>%
    summarise_(.dots=fcns) %>%
    mutate_each_(funs(cut(., breaks = cutpoints, include.lowest=TRUE)),
                 vars = names(fcns)) %>%
    group_by_(.dots=names(fcns)) %>%
    summarise(count=n()) %>%
    ungroup() %>%
    mutate(percentage = count/sum(count))
  return(binned)

}

#' WISEsummmary
#'
#' Within-subject Error Summary
#'
#' @param data A data frame
#' @param DV Character vector giving the dependent variable
#' @param betweenvars Character vector giving the between subject variables
#' @param withinvars Character vector giving the within subject variables
#' @param idvar Character vector giving the name of the column holding subject
#' identifiers
#' @param CI_width Numeric vector giving the confidence level for computing the
#' confidence interval boundaries. Must be between 0 and 1, non-inclusive.
#'
#' @return A data frame
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %<>%
#' @importFrom lazyeval interp
#'
#' @examples
#' library(whoppeR)
#' collapsed <- WISEsummary(MemoryDrugs, DV = "Recall", idvar = "Subject",
#'                          betweenvars = c("Gender", "Dosage"),
#'                          withinvars = c("Task", "Valence"))
#'
WISEsummary <- function(data, DV, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, CI_width=.95) {

  # Norm each subject's data so that each subject's mean is equal to the mean
  # of the between subject condition they are in
  #
  # To do this, we get each subject's mean, join it with the raw data,
  # then center the obsevations from each subject around the grand mean
  # by subtracting off the indvidual mean for each subject, and then add
  # the grand mean
  #
  # Then we use this re-centered data as the new "raw" data, to calculate
  # means, sd, and sem as usual

  normed_avg <- data %>% group_by_(.dots = idvar) %>%
    summarise_(subject_avg = lazyeval::interp(~mean(var), var = as.name(DV))) %>%
    left_join(x = data, y = . , by = idvar) %>%
    mutate_(centered_avg = interp(~ DV - subject_avg + mean(DV),
                                  DV = as.name(DV))) %>%
    select(-subject_avg) %>%
    group_by_(.dots =  c(betweenvars, withinvars)) %>%
    summarise_each(funs = funs(mean,sd, n()),centered_avg) %>%
    mutate(sem = sd/sqrt(n))

  # Get the averages in each condition (grouping by within and between variables,
  # ignoring the subjects. Standard 'unnormed' means.
  data %<>% group_by_(.dots =  c(betweenvars, withinvars)) %>%
    summarise_(mean = lazyeval::interp(~mean(var), var = as.name(DV)))

  # Combine the normed and unnormed averages
  normed_avg %<>%
    ungroup() %>%
    left_join(x = data, y = . , by = c(betweenvars, withinvars)) %>%
    mutate(CI =  qt((1-CI_width)/2, n-1, lower.tail = FALSE)*sem)

  # Apply correction from Morey (2008) to the standard error and confidence interval
  # Get the product of the number of conditions of within-S variables
  nCells <- ungroup(normed_avg) %>%
    select_(.dots = withinvars) %>%
    distinct() %>%
    nrow()

  correction <- sqrt(x = (nCells/(nCells - 1)))

  # Apply the correction factor to all our measures of variablity
  normed_avg[,c("sd","sem","CI")] <- lapply(normed_avg[,c("sd","sem","CI")], `*`, correction)

  # Calculate CI upper and lower bounds
  normed_avg %<>% mutate(CI_upper = mean.x + CI,
                         CI_lower = mean.x - CI) %>%
    rename_(.dots = setNames(c("mean.y", "mean.x"), c(paste0("normed_",DV), DV)))

  return(normed_avg)
}
