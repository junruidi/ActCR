#' @title Interdaily Statbility for the Whole Dataset
#' @description This function calcualte interdaily stability, a nonparametric metric
#' of circadian rhtymicity. This function is a whole dataset
#' wrapper for \code{IS}
#'
#' @param count.data \code{data.frame} of dimension n * (1440+2) containing the
#' 1440 dimensional activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be
#' either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequency of days within each subject.
#' @param  window an \code{integer} indicating what is the window to bin the data before
#' the function can be apply to the dataset. For details, see \code{bin_data}.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#'
#' @return A \code{data.frame} with the following 2 columns
#' \item{ID}{ID}
#' \item{IS}{IS}
#'
#' @references Junrui Di et al. Joint and individual representation of domains of physical activity, sleep, and circadian rhythmicity. Statistics in Biosciences.
#' @export
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = example_activity_data$count
#' is_subj = IS_long(count.data = count1, window = 10, method = "average")


IS_long = function(
  count.data,
  window = 1,
  method = c("average","sum")
  ){

  x = count.data

  if(! "ID" %in% names(x)){
    stop("Please name the ID column with the name ID")
  }

  x = cbind(x[,1:2], as.data.frame(
    t(apply(x[,-c(1:2)], 1,
            FUN = bin_data, window = window, method = method))))
  a = split(x,f = x$ID)
  y = unlist(lapply(a, function(x) IS(x[,-c(1:2)])))
  is.out = data.frame(ID = names(y), IS = y)
  names(is.out)[2] = paste0("IS_",window)
  return(is.out)
}

