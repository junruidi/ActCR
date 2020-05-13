#' @title Cosinor Model for Circadian Rhythmicity
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.
#'
#'
#' @param x \code{vector} vector of dimension n*1440 which reprsents n days of 1440 minute activity data
#' @param window The calcuation needs to understand what is the window size of the data
#'
#'
#' @importFrom cosinor cosinor.lm
#' @importFrom cosinor2 correct.acrophase
#'
#' @return A list with elements
#' \item{mes}{mesor}
#' \item{amp}{amplitude}
#' \item{acro}{acrophase in negative radians}
#' \item{acrotime}{acrophase in time domain (hour)}
#'
#' @export
#' @examples
#' count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#' cos_coeff = ActCosinor(x = count1, window = 1)


ActCosinor = function(
  x,
  window = 1
){
  if(1440 %% window != 0){
    stop("Only use window size that is an integer factor of 1440")
  }


  if(length(x) %% (1440/window) != 0){
    stop("Window size and length of input vector doesn't match.
         Only use window size that is an integer factor of 1440")
  }

  dim = 1440/window
  n.days = length(x)/dim

  tmp.dat = data.frame(time = rep(1:dim, n.days) / (60/window), Y = x)
  fit = cosinor.lm(Y ~ time(time) + 1, data = tmp.dat, period = 24)

  mesor = fit$coefficients[1]
  amp = fit$coefficients[2]
  # acr = fit$coefficients[3]
  acr = correct.acrophase(fit)
  acrotime = (-1) * acr * 24/(2 * pi)

  names(mesor) = names(amp) = names(acr) = names(acrotime) = NULL

  ret = list("mes" = mesor,
             "amp" = amp,
             "acr" = acr,
             "acrotime" = acrotime,
             "ndays" = n.days)

  return(ret)

}

