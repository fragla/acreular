is.Date <- function(x) inherits(x, 'Date')

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
    return(NA)
  }

  duration <- as.numeric(assessment-onset)

  return(duration)
}
