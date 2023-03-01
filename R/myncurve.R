#' @title Normal Distribution Curve Probability Function
#'
#' @param mu the mean value
#' @param sigma the standard deviation
#' @param a the value in which you are finding a probability from negative infinity to value a
#'
#' @return Returns a normal distribution curve of the mean and standard deviation with a probability of a value being selected from negative infinity to the value a
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a) {
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu-(3* sigma), mu+(3*sigma)))
  xcurve = seq(mu-(3* sigma), a, length = 1000)
  ycurve= dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(mu-(3* sigma), xcurve, a), c(0, ycurve, 0), col = "Red")
  prob = pnorm(a, mean = mu, sd = sigma)
  prob = round(prob, 4)
  prob
}
