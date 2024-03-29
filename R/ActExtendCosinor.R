#' @title Extended Cosinor Model for Circadian Rhythmicity
#' @description Extended cosinor model based on sigmoidally transformed cosine curve using anti-logistic transformation
#'
#'
#' @param x \code{vector} vector of dimension n*1440 which represents n days of 1440 minute activity data
#' @param window The calculation needs the window size of the data. E.g window = 1 means each epoch is in one-minute window.
#' @param lower A numeric vector of lower bounds on each of the five parameters (in the order of minimum, amplitude, alpha, beta, acrophase) for the NLS. If not given, the default lower bound for each parameter is set to \code{-Inf}.
#' @param upper A numeric vector of upper bounds on each of the five parameters (in the order of minimum, amplitude, alpha, beta, acrophase) for the NLS. If not given, the default lower bound for each parameter is set to \code{Inf}
#' @param export_ts A Boolean to indicate whether time series should be exported
#'
#' @importFrom cosinor cosinor.lm
#' @importFrom cosinor2 correct.acrophase
#' @importFrom minpack.lm nls.lm nls.lm.control
#' @importFrom stats coef residuals fitted
#'
#' @return A list with elements
#' \item{minimum}{Minimum value of the of the function.}
#' \item{amp}{amplitude, a measure of half the extend of predictable variation within a cycle. This represents the highest activity one can achieve.}
#' \item{alpha}{It determines whether the peaks of the curve are wider than the troughs: when alpha is small, the troughs are narrow and the peaks are wide; when alpha is large, the troughs are wide and the peaks are narrow.}
#' \item{beta}{It dertermines whether the transformed function rises and falls more steeply than the cosine curve: large values of beta produce curves that are nearly square waves.}
#' \item{acrotime}{acrophase is the time of day of the peak in the unit of the time (hours)}
#' \item{F_pseudo}{Measure the improvement of the fit obtained by the non-linear estimation of the transformed cosine model}
#' \item{UpMesor}{Time of day of switch from low to high activity. Represents the timing of the rest- activity rhythm. Lower (earlier) values indicate increase in activity earlier in the day and suggest a more advanced circadian phase.}
#' \item{DownMesor}{Time of day of switch from high to low activity. Represents the timing of the rest-activity rhythm. Lower (earlier) values indicate decline in activity earlier in the day, suggesting a more advanced circadian phase.}
#' \item{MESOR}{A measure analogous to the MESOR of the cosine model (or half the deflection of the curve) can be obtained from mes=min+amp/2. However, it goes through the middle of the peak, and is therefore not equal to the MESOR of the cosine model, which is the mean of the data.}
#' \item{ndays}{Number of days modeled.}
#' \item{cosinor_ts}{Exported data frame with time, time over days, original time series, fitted time series using cosinor model from step 1, and fitted extended cosinor model from step 2}
#'
#'
#' @references Marler MR, Gehrman P, Martin JL, Ancoli-Israel S. The sigmoidally transformed cosine curve: a mathematical model for circadian rhythms with symmetric non-sinusoidal shapes. Stat Med.
#' @export
#' @examples
#' count1 = c(t(example_activity_data$count[c(1:2),-c(1,2)]))
#' cos_coeff = ActExtendCosinor(x = count1, window = 1,export_ts = TRUE)


ActExtendCosinor = function(
  x,
  window = 1,
  lower = c(0, 0, -1, 0, -3), ## min, amp, alpha, beta, phi
  upper = c(Inf, Inf, 1, Inf, 27),
  export_ts = FALSE
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

  # Stage 1 ---- Cosinor Model
  tmp.dat = data.frame(time = rep(1:dim, n.days) / (60/window), Y = x)
  fit = cosinor.lm(Y ~ time(time) + 1, data = tmp.dat, period = 24)

  mesor = fit$coefficients[1]
  amp = fit$coefficients[2]
  acr = correct.acrophase(fit)
  acrotime = (-1) * acr * 24/(2 * pi)

  names(mesor) = names(amp) = names(acr) = names(acrotime) = NULL

  # Stage 2 ---- Transformation

  ## Set up the initial values
  e_min0 = max(mesor - amp, 0)
  e_amp0 = 2 * amp
  e_phi0 = acrotime
  e_par0 = c(e_min0, e_amp0, 0, 2, e_phi0) ## min, amp, alpha, beta, phi

  tmp.dat = tmp.dat[!is.na(tmp.dat$Y),] # ignore missing values

  fit_nls = nls.lm(e_par0, fn = fn_obj,
                   lower = lower,
                   upper = upper,
                   tmp.dat = tmp.dat,
                   control = nls.lm.control(maxiter = 1000))

  if (export_ts == TRUE) {
    # Put time series in data.frame and add it to output:
    fittedYext = tmp.dat$Y - residuals(fit_nls) #extended cosinor model
    fittedY = fitted(fit$fit) #cosinor omodel
    original = tmp.dat$Y #original data
    time = tmp.dat$time #time
    time2 = time
    drops = which(diff(time) < 0) + 1
    for (k in drops) {
      time2[k:length(time)] = time2[k:length(time)] + 24
    }
    time_across_days = time2
    cosinor_ts = as.data.frame(cbind(time, time_across_days, original, fittedY, fittedYext)) # data.frame with all signal
  } else {
    cosinor_ts = NULL
  }

  ## Estimated exteded cosinor parameters,in the order of
  ## minimum, amplitude, alpha, beta, acrophase
  coef.nls = coef(fit_nls)

  e_min = coef.nls[1]
  e_amp = coef.nls[2]
  e_alpha = coef.nls[3]
  e_beta = coef.nls[4]
  e_acrotime = coef.nls[5]

  ## Pseudo F statistics
  RSS_cos = sum((fit$fit$residuals)^2)
  RSS_ext = sum(residuals(fit_nls)^2)
  F_pseudo = ((RSS_cos - RSS_ext)/2)/(RSS_ext/(nrow(tmp.dat) - 5))

  ## Derived metrics
  UpMesor = -acos(e_alpha)/(2*pi/24) + e_acrotime
  DownMesor = acos(e_alpha)/(2*pi/24) + e_acrotime
  MESOR = e_min + e_amp/2

  params = list("minimum" = e_min,
             "amp" = e_amp,
             "alpha" = e_alpha,
             "beta" = e_beta,
             "acrotime" = e_acrotime,
             "F_pseudo" = F_pseudo,
             "UpMesor" = UpMesor,
             "DownMesor" = DownMesor,
             "MESOR" = MESOR,
             "ndays" = n.days)
  ret = list("params" = params, "cosinor_ts" = cosinor_ts)

  return(ret)

}


## Objective function to optimize for extended cosinor model
fn_obj = function(par, tmp.dat) {
  ct = cos((tmp.dat[, 1] - par[5]) * 2 * pi / 24)
  lct = exp(par[4] * (ct - par[3])) / (1 + exp(par[4] * (ct - par[3])))
  rt = par[1] + par[2] * lct
  tmp.dat[, 2] - rt
}

