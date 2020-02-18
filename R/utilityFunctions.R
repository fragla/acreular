#' Checks if object is of class Date.
#'
#'
#' @param date
#'
#' @examples
#' date <- as.Date("2010-01-01")
#'
#' is.Date(date)
#'
#' @export
is.Date <- function(x) inherits(x, 'Date')

is.empty <- function(x) {
  if(length(x)==0) {
    return(TRUE)
  }

  if(is.na(x)) {
    return(TRUE)
  }

  if(is.null(x)) {
    return(TRUE)
  }

  if(is.nan(x)) {
    return(TRUE)
  }

  if (is.character(x) && nchar(gsub("[[:space:]]", "", x))==0) {
    return(TRUE)
  }

  return(FALSE)
}

#' Convert two dates to a duration in days.
#'
#' Converts onset and assessment dates to a duration in days.
#'
#' @param onset date
#' @param assessment date
#'
#' @examples
#' onset <- as.Date("2010-01-01")
#' assessment <- as.Date("2010-02-13")
#'
#' datesToDuration(onset, assessment)
#'
#' @export
datesToDuration <- function(onset, assessment) {
  if(is.na(onset) || is.na(assessment)) {
    return(NA)
  }

  if (!is.Date(onset) || !is.Date(assessment)) {
    stop("Non-date object found.")
  }

  duration <- as.numeric(assessment-onset)

  return(duration)
}
