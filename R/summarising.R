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
#' @param dependentvars Character vector giving the dependent variable
#' @param betweenvars Character vector giving the between subject variables
#' @param withinvars Character vector giving the within subject variables
#' @param idvar Character vector giving the name of the column holding subject
#' identifiers
#' @param CI_width Numeric vector giving the confidence level for computing the
#' confidence interval boundaries. Must be between 0 and 1, non-inclusive.
#' @param na.rm a logical value indicating whether NA values should be removed from the Dependent Variables.
#'
#' @return A data frame
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(whoppeR)
#' head(MemoryDrugs)
#' collapsed <- WISEsummary(MemoryDrugs,
#'                          dependentvars = "Recall",
#'                          idvar = "Subject",
#'                          betweenvars = c("Gender", "Dosage"),
#'                          withinvars = c("Task", "Valence"))
#'
#'
WISEsummary <- function(data, dependentvars, betweenvars=NULL, withinvars=NULL,
                        idvar=NULL, CI_width=.95, na.rm=FALSE) {

  # Norm each subject's data so that each subject's mean is equal to the mean
  # of the between subject condition they are in
  #
  # To do this, we get each subject's mean, join it with the raw data,
  # then center the observations from each subject around the grand mean
  # by subtracting off the individual mean for each subject, and then add
  # the grand mean
  #
  # Then we use this re-centered data as the new "raw" data, to calculate
  # means, sd, and sem as usual

  # Reshape the data into a long format that combines values from different DV's into
  # one column. This makes the operations that calculate different means, SEMs, and CI
  # widths for different DVs simple column-wise operations on data frames grouped by
  # the DV variable name.

  # Get the averages in each condition (grouping by within and between variables,
  # ignoring the subjects. Standard 'unnormed' means.

  by_dv <- data %>%
    tidyr::pivot_longer(cols = {{dependentvars}}, names_to = "DV")

  cell_means <- by_dv %>%
    dplyr::group_by(.data$DV, dplyr::across({{betweenvars}}), dplyr::across({{withinvars}})) %>%
    dplyr::summarise(
      dplyr::across(
        .data$value,
        .fns = ~mean(.x, na.rm = na.rm),
        .names = "mean"),
      .groups = "drop")

  nCells <- nrow(dplyr::distinct(cell_means, dplyr::across({{withinvars}})))
  correction <- if(nCells > 1) sqrt((nCells/(nCells - 1))) else 1

  recentered <- by_dv %>%
    dplyr::group_by(.data$DV, dplyr::across({{idvar}})) %>%
    dplyr::mutate(subject_avg = mean(.data$value)) %>%
    dplyr::group_by(.data$DV) %>%
    dplyr::mutate(recentered_value = .data$value - .data$subject_avg + mean(.data$value)) %>%
    dplyr::group_by(.data$DV, dplyr::across({{withinvars}}), dplyr::across({{betweenvars}})) %>%
    dplyr::summarise(
      dplyr::across(
        .data$recentered_value,
        .fns = list(recentered_mean = mean, sem = sem, n = length),
        .names = "{.fn}"),
      .groups = "drop")

  by_cols <- names(cell_means)
  by_cols <- by_cols[!by_cols == "mean"]

  dplyr::left_join(cell_means, recentered, by = by_cols) %>%
    dplyr::mutate(
      sem = .data$sem * correction,
      CI = stats::qt((1 - CI_width)/2, df = .data$n-1, lower.tail = FALSE) * .data$sem,
      CI_lower = .data$mean - .data$CI,
      CI_upper = .data$mean + .data$CI) %>%
    dplyr::select(-.data$CI) %>%
    tidyr::pivot_wider(
      names_from = .data$DV,
      values_from = c(.data$mean, .data$recentered_mean, .data$sem, .data$n, .data$CI_lower, .data$CI_upper),
      names_glue = "{DV}_{.value}")

}


#' Standard Error of the Mean
#'
#' Calculates the standard error of the mean statistic, an estimate of the variability
#'  of the sampling distribution of the mean. See "details" for equation.
#'
#' @param x A numeric or logical atomic vector
#' @param na.rm a logical value indicating whether NA values should be removed from the input.
#'
#' @return A scalar numeric vector
#' @export
#'
#' @details
#' Uses the following forumula:
#'
#' \deqn{S.E.M. = \sqrt{\frac{Var(x)}{N}}}{S.E.M. =  sqrt(Var(x)/N)}
#'
#' @examples
#'
#' x <- runif(30, 5, 2)
#' sem(x)
#'
sem <- function(x, na.rm = FALSE) {

  if (!(is.numeric(x) || is.logical(x))) {
    stop('argument "x" is not a numeric or logical atomic vector')
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ## Standard error of the mean calculation
  sqrt(var(x) / length(x))
}
