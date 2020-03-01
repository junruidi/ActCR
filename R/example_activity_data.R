#' @title  Activity/Wear Data from 50 Subjects from NHANES 2003 - 2006
#'
#' @description A list of two \code{data.frames} containing the counts and the weartime
#' for 50 NHANES subjects 
#'
#' @format A list of two \code{data.frame}s with 1442 columns, which are in the following order:
#' \describe{
#' \item{ID}{identifier of the person.}
#' \item{Day}{\code{numeric} sequence 1,2,.. indicating the order of days within a subject.}
#' \item{MIN1-MIN1440}{counts of activity of that specific minute.}
#' }
"example_activity_data"