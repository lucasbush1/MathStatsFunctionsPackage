#' @title Find Mode
#'
#' @param v vector containing points
#'
#' @return The mode of the vector
#' @export
#'
#' @examples
#' v = c(1, 4, 4, 4, 5, 6)
#' getmode(v)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
