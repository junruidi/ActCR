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
#' \item{mes}{MESOR which is short for midline statistics of rhythm, which is a rhythm adjusted mean. This represents mean activity level.}
#' \item{amp}{amplitude, a measure of half the extend of predictable variation within a cycle. This represents the highest activity one can achieve.}
#' \item{acro}{acrophase, a meaure of the time of the overall high values recurring in each cycle. Here it has a unit of radian. This represents time to reach the peak.}
#' \item{acrotime}{acrophase in the unit of the time (hours)}
#' \item{ndays}{Number of days modeled}
#'
#'
#' @references Cornelissen, G. Cosinor-based rhythmometry. Theor Biol Med Model 11, 16 (2014). https://doi.org/10.1186/1742-4682-11-16
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

