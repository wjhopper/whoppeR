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
#' @import dplyr
#' @importFrom tidyr gather_
#' @importFrom tidyr spread_
#' @importFrom magrittr %<>%
#' @importFrom lazyeval interp
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
WISEsummary <- function(data, dependentvars, betweenvars=NULL, withinvars=NULL,
                        idvar=NULL, CI_width=.95, na.rm=FALSE) {

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

  # Reshape the data into a long format that combines values from different DV's into
  # one column. This makes the operations that calculate different means, SEMs, and CI
  # widths for different DVs simple column-wise operations on data frames grouped by
  # the DV variable name.

  # Get the averages in each condition (grouping by within and between variables,
  # ignoring the subjects. Standard 'unnormed' means.
  cell_means <- group_by_(data,.dots =  c(betweenvars, withinvars))
  cell_means <- summarise_at(cell_means, dependentvars, funs(mean = mean), na.rm = na.rm)
  cell_means <- ungroup(cell_means)
  if (length(dependentvars) == 1) {
    names(cell_means)[ncol(cell_means)] %<>% paste(dependentvars, ., sep="_")
  }

  data <- gather_(data, "DV", "value", dependentvars, na.rm = na.rm)
  data <- group_by_(data, .dots = c("DV", idvar))
  recentered <- summarise_at(data, "value", funs(subject_avg = mean))
  recentered <- left_join(x = data,
                          y = recentered,
                          by = c(idvar,"DV"))
  recentered <- group_by(recentered, DV)
  recentered <- mutate(recentered,
                       recentered_value = value - subject_avg + mean(value))
  recentered <- group_by_(recentered, .dots =  c("DV", betweenvars, withinvars))
  recentered <- summarise_at(recentered, "recentered_value",
                             funs(recentered_mean = mean, sem, n())
                             )
  recentered <- ungroup(recentered)

  # Apply correction from Morey (2008) to the standard error
  # Get the product of the number of conditions of within-subject variables
  nCells <- nrow(distinct_(cell_means, .dots = withinvars))
  correction <- sqrt((nCells/(nCells - 1)))

  # Apply the correction factor to the SEM estimate
  recentered$sem <- recentered$sem * correction

  # Calculate CI upper and lower bounds
  recentered <- mutate(recentered,
                       CI = qt((1-CI_width)/2, df = n-1, lower.tail = FALSE)*sem)

  # Put the recentered data back into its original form, with a different column
  # for eaech DV
  each_DV <- split(recentered, recentered$DV)
  new_vars <- c("recentered_mean","sem","n","CI")
  each_DV <- lapply(each_DV,
                    function(d) {
                      names(d)[names(d) %in% new_vars] <- paste(d$DV[1], new_vars, sep="_")
                      select(d, -DV)
                      }
                    )
  recentered <- Reduce(function(x,y) full_join(x,y, by=c(betweenvars, withinvars)),
                       c(list(cell_means),each_DV))

  CIbounds <- lapply(list(CI_upper=`+`, CI_lower=`-`),
                     Map,
                     recentered[paste0(dependentvars, "_mean")],
                     recentered[paste0(dependentvars, "_CI")]
                     )
  CIbounds <- unlist(CIbounds, recursive = FALSE)
  names(CIbounds) <- sapply(strsplit(names(CIbounds), "\\."),
                            function(x) sub("_mean","", paste(rev(x),collapse = "_")))
  recentered <- bind_cols(select(recentered, -contains("CI")),
                          CIbounds)

  DV_var_locations <- unlist(lapply(dependentvars, grep, x = names(recentered)))
  ID_var_locations <- setdiff(1:ncol(recentered), DV_var_locations)
  recentered[c(ID_var_locations,DV_var_locations)]

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
