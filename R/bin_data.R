#' @title Bin data into longer windows
#' @description Bin minute level data into different time resolutions
#'
#' @param x  \code{vector} of activity data.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#' @param window window size used to bin the original 1440 dimensional data into. Window size
#' should be an integer factor of 1440
#' @return a vector of binned data

#'
#' @importFrom zoo rollapply

#'
#' @export
#'
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#' xbin = bin_data(x = count1, window = 10, method = "average")


bin_data = function(
  x = x,
  window = 1,
  method = c("average","sum")
){

  if(length(x) != 1440){
    stop("Please inpute 1440 dimensional minute-level activity data!")
  }

  if(1440 %% window != 0){
    stop("Only use window size that is an integer factor of 1440")
  }


  method = match.arg(method)

  if(method == "sum"){
    binx = rollapply(x, width = window, by = window, FUN = sum)
  }
  if(method == "average"){
    binx = rollapply(x, width = window, by = window, FUN = sum)
  }

  return(binx)

}
