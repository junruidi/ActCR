#' @title Cosinor Model for Circadian Rhythmicity for the Whole Dataset
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.This function is a whole dataset
#' wrapper for \code{ActCosinor}.
#'
#' @param count.data \code{data.frame} of dimension n * (p+2) containing the
#' p dimensional activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be
#' either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequence of days within each subject.
#' @param window The calculation needs the window size of the data. E.g window = 1 means each epoch is in one-minute window.
#' @param export_ts A Boolean to indicate whether time series should be exported (notice: it takes time and storage space to export time series data for all subject-days. Use this with caution. Suggest to only export time series for selected subjects)
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by %>% do mutate
#'
#'
#' @return A \code{data.frame} with the following 5 columns
#' \item{ID}{ID}
#' \item{ndays}{number of days}
#' \item{mes}{MESRO, which is short for midline statistics of rhythm, which is a rhythm adjusted mean. This represents mean activity level.}
#' \item{amp}{amplitude, a measure of half the extend of predictable variation within a cycle. This represents the highest activity one can achieve.}
#' \item{acro}{acrophase, a meaure of the time of the overall high values recurring in each cycle. Here it has a unit of radian. This represents time to reach the peak.}
#' \item{acrotime}{acrophase in the unit of the time (hours)}
#' \item{ndays}{Number of days modeled}
#' \item{pr}{Percent rhythm, which evaluates the elative power of the rhythm by checking the proportion of the variance explained by the model}
#' \item{pr_pval}{P values for the percent rhythm}
#' \item{cosinor_ts}{Exported data frame with time, time over days, original time series, fitted time series using cosinor model}
#'
#' @export
#' @examples
#' counts_1 = example_activity_data$count[c(1:12),]
#' cos_all_1 = ActCosinor_long(count.data = counts_1, window = 1,export_ts = TRUE)
#' counts_10 = cbind(counts_1[,1:2],
#' as.data.frame(t(apply(counts_1[,-c(1:2)], 1,
#' FUN = bin_data, window = 10, method = "average"))))
#' cos_all_10 = ActCosinor_long(count.data = counts_10, window = 10)


ActCosinor_long = function(
  count.data,
  window = 1,
  export_ts = FALSE
){
  ID = value = . = NULL
  rm(list = c("ID", "value", "."))

  long.count = reshape(count.data, varying = names(count.data)[3:ncol(count.data)],direction = "long",
                       timevar = "Time",idvar = c("ID","Day"),v.names = "values",new.row.names = c(1:((ncol(count.data)-2)*nrow(count.data))))
  long.count = long.count[
    with(long.count, order(ID, Day,Time)),
    ]


  result= long.count  %>% group_by(ID) %>% do(out = ActCosinor(.$values,
                                                                window = window, export_ts = export_ts))

  # out = unlist(result$out)
  #
  # result$ndays = out[which(names(out) == "ndays")]
  # result$mes = out[which(names(out) == "mes")]
  # result$amp = out[which(names(out) == "amp")]
  # result$acr = out[which(names(out) == "acr")]
  # result$acrotime = out[which(names(out) == "acrotime")]
  #
  # result$out = NULL
  # names(result)[3:6] = paste0(names(result)[3:6],"_",window)
  # return(result)
  #

  ## Exporting the parameters

  out = unlist(sapply(result$out, function(x) x[1]))
  params = as.data.frame(matrix(out,ncol = 7,byrow = T))
  names(params) = gsub("params.","", names(out)[1:7])
  params = params %>% mutate(ID = result$ID)
  params = params[,c("ID","ndays","mes","amp","acr","acrotime","pr","pr_pval")]
  names(params)[3:ncol(params)] = paste0( names(params)[3:ncol(params)],"_",window)

  ## Exporting the parameters
  if(export_ts){
    data_ts = sapply(result$out, function(x) x[2])
    names(data_ts) = result$ID
    ret = list("params" = params,"cosinor_ts" = data_ts)
  }else{
    ret = params
  }

  return(ret)

}
