#' as.index: equivalent of MATLAB's ind2sub
#'
#' Given the row  and column  of elements in an m × n matrix,
#' compute the single index which can be used to access those
#' elements of A (also works if r and c are vectors)
#'
#' @param rows Total number of rows in the matrix. Scalar value.
#' @param rownum row subscript. vector of any length.
#' @param colnum column subscript. vector of any length.
#'
#' @return A numeric vector of linear indices
#' @export
#'
as.index <- function(rows, rownum,colnum) {
  index <- (colnum-1)*rows + rownum
}
