#' @title Intradaily Variability
#' @description This function calcualte intradaily variability, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} of activity data
#' @return IV
#'
#'
#' @export
#' @references Junrui Di et al. Joint and individual representation of domains of physical activity, sleep, and circadian rhythmicity. Statistics in Biosciences.
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#' iv = IV(x = count1)
#'
#'

IV = function(
  x
){
  mean.counts = mean(x)
  numerator = sum(diff(x)^2)/(length(x) - 1)
  denominator = sum((x - mean.counts)^2)/length(x)
  return(numerator/denominator)
}
