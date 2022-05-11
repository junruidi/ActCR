#' @title Relative Amplitude for the Whole Datset
#' @description This function calcualte relative amplitude, a nonparametric metric
#' of circadian rhtymicity. This function is a whole dataset
#' wrapper for \code{RA}.
#'
#' @param count.data \code{data.frame} of dimension n * (p+2) containing the
#' p dimensional activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be
#' either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequency of days within each subject.
#' @param window since the caculation of M10 and L5 depends on the dimension of data, we need to include
#' window size as an argument. This function is a whole dataset
#' wrapper for \code{RA}.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#'
#' @return A \code{data.frame} with the following 3 columns
#' \item{ID}{ID}
#' \item{Day}{Day}
#' \item{RA}{RA}
#'
#' @importFrom zoo rollapplyr
#'
#' @export
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = example_activity_data$count[1:12,]
#' ra_all = RA_long(count.data = count1, window = 10, method = "average")
#'

RA_long = function(
  count.data,
  window = 1,
  method = c("average","sum")
  ){
  x = count.data
  ra_out =  apply(x[,-c(1,2)], 1,
                  RA, window = window, method = method)
  out = unlist(ra_out)
  params = as.data.frame(matrix(out,ncol = 3,byrow = T))
  names(params) = c("M10","L5","RA")
  params = params %>% mutate(ID = x$ID,Day = x$Day)
  names(params)[1:3] = paste0(names(params)[1:3],"_",window)
  params = params[,c(4,5,1:3)]

  return(params)
}

