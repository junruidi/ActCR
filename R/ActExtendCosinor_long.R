#' @title Cosinor Model for Circadian Rhythmicity for the Whole Dataset
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.This function is a whole dataset
#' wrapper for \code{RA}.
#'
#' @param count.data \code{data.frame} of dimension n * (p+2) containing the
#' p dimensional activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be
#' either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequency of days within each subject.
#' @param window since the caculation of M10 and L5 depends on the dimension of data, we need to include
#' window size as an argument.
#' @param lower A numeric vector of lower bounds on each parameter for the NLS. If not given, the default lower bound for each parameter is set to \code{-Inf}.
#' @param upper A numeric vector of upper bounds on each parameter for the NLS. If not given, the default lower bound for each parameter is set to \code{Inf}
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by %>% do
#'
#'
#' @return A \code{data.frame} with the following 5 columns
#' \item{ID}{ID}
#' \item{ndays}{number of days}
#' \item{minimum}{Minimum value of the of the function.}
#' \item{amp}{amplitude, a measure of half the extend of predictable variation within a cycle. This represents the highest activity one can achieve.}
#' \item{alpha}{It determines whether the peaks of the curve are wider than the troughs: when alpha is small, the troughs are narrow and the peaks are wide; when alpha is large, the troughs are wide and the peaks are narrow.}
#' \item{beta}{It dertermines whether the transformed function rises and falls more steeply than the cosine curve: large values of beta produce curves that are nearly square waves.}
#' \item{acrotime}{acrophase is the time of day of the peak in the unit of the time (hours)}
#' \item{F_pseudo}{Measure the improvement of the fit obtained by the non-linear estimation of the transformed cosine model}
#' \item{UpMesor}{Time of day of switch from low to high activity. Represents the timing of the rest- activity rhythm. Lower (earlier) values indicate increase in activity earlier in the day and suggest a more advanced circadian phase.}
#' \item{DownMesor}{Time of day of switch from high to low activity. Represents the timing of the rest-activity rhythm. Lower (earlier) values indicate decline in activity earlier in the day, suggesting a more advanced circadian phase.}
#' \item{MESOR}{A measure analogous to the MESOR of the cosine model (or half the deflection of the curve) can be obtained from mes=min+amp/2. However, it goes through the middle of the peak, and is therefore not equal to the MESOR of the cosine model, which is the mean of the data.}
#'
#' @export
#' @examples
#' counts_1 = example_activity_data$count
#' cos_all_1 = ActExtendCosinor_long(count.data = counts_1, window = 1)
#' counts_10 = cbind(counts_1[,1:2],
#' as.data.frame(t(apply(counts_1[,-c(1:2)], 1,
#' FUN = bin_data, window = 10, method = "average"))))
#' cos_all_10 = ActExtendCosinor_long(count.data = counts_10, window = 10)


ActExtendCosinor_long = function(
  count.data,
  window = 1,
  lower = c(0, 0, -1, 0, -3), ## min, amp, alpha, beta, phi
  upper = c(Inf, Inf, 1, Inf, 27)
){
  ID = value = . = NULL
  rm(list = c("ID", "value", "."))

  long.count = reshape(count.data, varying = names(count.data)[3:ncol(count.data)],direction = "long",
                       timevar = "Time",idvar = c("ID","Day"),v.names = "values",new.row.names = c(1:((ncol(count.data)-2)*nrow(count.data))))
  long.count = long.count[
    with(long.count, order(ID, Day,Time)),
  ]


  result= long.count  %>% group_by(ID) %>% do(out = ActExtendCosinor(.$values,
                                                               window = window))

  out = unlist(result$out)

  result$ndays = out[which(names(out) == "ndays")]
  result$minimum = out[which(names(out) == "minimum")]
  result$amp = out[which(names(out) == "amp")]
  result$alpha = out[which(names(out) == "alpha")]
  result$beta = out[which(names(out) == "beta")]
  result$acrotime = out[which(names(out) == "acrotime")]
  result$F_pseudo = out[which(names(out) == "F_pseudo")]
  result$UpMesor = out[which(names(out) == "UpMesor")]
  result$DownMesor = out[which(names(out) == "DownMesor")]
  result$MESOR = out[which(names(out) == "MESOR")]


  result$out = NULL
  names(result)[3:11] = paste0(names(result)[3:11],"_",window)
  return(result)

}
