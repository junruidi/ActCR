#' @title Interdaily Statbility
#' @description This function calcualte interdaily stability, a nonparametric metric
#' of circadian rhtymicity
#'
#' @param x  \code{data.frame} of dimension ndays by p, where p is the dimension of the data.
#'
#'
#' @export
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = example_activity_data$count[c(1,2,3),-c(1,2)]
#' is = IS(x = count1)


IS = function(
  x
  ){
  p = ncol(x)
  xh = colMeans(x)

  v = c(t(x))
  n = length(v)

  numerator = sum((xh - mean(v))^2)/p
  denominator = sum((v - mean(v))^2)/n

  return(numerator/denominator)
}
