#' Dash Matrix
#' @description Function to obtain the matrix of number of 1-1s, 1-2s, and so on.
#' @param data dataset with competitors as rows and judges as columns
#' @return A matrix:
#' \item{dashmatrix}{matrix of number of placements}
#' @examples
#' dashmatrix(testdata)
#' @export

dashmatrix <- function(data) {
  dashmat <- matrix(NA, nrow(data), nrow(data))
  for (i in 1:nrow(data)) {
    dashmat[i,] <- cumsum(table(factor(data[i,], levels = 1:nrow(data))))
  }
  dashmat
}
