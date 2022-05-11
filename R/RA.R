#' @title Relative Amplitude
#' @description This function calcualte relative amplitude, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} vector of activity data
#' @param window since the caculation of M10 and L5 depends on the dimension of data, we need to include
#' window size as an argument.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#'
#' @return RA
#'
#' @importFrom zoo rollapplyr
#'
#' @export
#' @references Junrui Di et al. Joint and individual representation of domains of physical activity, sleep, and circadian rhythmicity. Statistics in Biosciences.
#'
#' @examples
#' data(example_activity_data)
#' count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#' ra = RA(x = count1, window = 10, method = "average")
#'
#'

RA = function(
  x,
  window = 1,
  method = c("average","sum")
  ){

  if(length(x) %% 1440/window != 0){
    stop("Window size and length of input vector doesn't match.
         Only use window size that is an integer factor of 1440")
  }

  x_bin = bin_data(x, window = window, method = method)
  M10 = max(roll(x_bin, 10 * 1440/window/24))
  L5 = min(roll(x_bin, 5 * 1440/window/24))
  relaamp = (M10 - L5)/(M10 + L5)
  return(relaamp)
}


roll = function(day.counts,k){
  kvec = rollapplyr(day.counts, k, function(x) mean(x,na.rm = T), fill = NA)
  kvec = kvec[!is.na(kvec)]
  return(kvec)
}
