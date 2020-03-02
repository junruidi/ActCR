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
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by %>% do
#'
#'
#' @return A \code{data.frame} with the following 5 columns
#' \item{ID}{ID}
#' \item{ndays}{number of days}
#' \item{mes}{mesor}
#' \item{amp}{amplitude}
#' \item{acro}{acrophase}
#'
#' @export
#' @examples
#' counts_1 = example_activity_data$count
#' cos_all_1 = ActCosinor_long(count.data = counts_1, window = 1)
#' counts_10 = cbind(counts_1[,1:2],
#' as.data.frame(t(apply(counts_1[,-c(1:2)], 1,
#' FUN = bin_data, window = 10, method = "average"))))
#' cos_all_10 = ActCosinor_long(count.data = counts_10, window = 10)


ActCosinor_long = function(
  count.data,
  window = 1
){
  ID = value = . = NULL
  rm(list = c("ID", "value", "."))

  long.count = reshape(count.data, varying = names(count.data)[3:ncol(count.data)],direction = "long",
                       timevar = "Time",idvar = c("ID","Day"),v.names = "values",new.row.names = c(1:((ncol(count.data)-2)*nrow(count.data))))
  long.count = long.count[
    with(long.count, order(ID, Day,Time)),
    ]


  result= long.count  %>% group_by(ID) %>% do(out = ActCosinor(.$values,
                                                                window = window))

  out = unlist(result$out)

  result$ndays = out[which(names(out) == "ndays")]
  result$mes = out[which(names(out) == "mes")]
  result$amp = out[which(names(out) == "amp")]
  result$acr = out[which(names(out) == "acr")]

  result$out = NULL
  names(result)[3:5] = paste0(names(result)[3:5],"_",window)
  return(result)

}
