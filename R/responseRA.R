#' Calculate EULAR response
#'
#' Calculate EULAR response from two DAS28-ESR or DAS28-CRP
#' measurements.
#'
#' @param das1 Initial DAS28-ESR/CRP measurement(s)
#' @param das2 Follow up DAS28-ESR/CRP measurement(s)
#' @examples
#'   eularResponse(5.31, 1.3)
#'
#'   baseline <- c(5.24, 3.6, 1.2)
#'   followup <- c(1.30, 3.3, 1.8)
#'
#'   eularResponse(baseline, followup)
#'
#' @export
eularResponse <- function(das1, das2) {
  stopifnot(length(das1)==length(das2))

  response <- rep(NA, length(das1))

  response[which(das2 <= 3.2 & (das1-das2) > 1.2)] <- "Good"
  response[which(das2 <= 3.2 & (das1-das2) > 0.6 & (das1-das2) <= 1.2)] <- "Moderate"
  response[which(das2 <= 3.2 & (das1-das2) <= 0.6)] <- "No response"
  response[which(das2 > 3.2 & das2 <= 5.1 & (das1-das2) > 1.2)] <- "Moderate"
  response[which(das2 > 3.2 & das2 <= 5.1 & (das1-das2) > 0.6 & (das1-das2) <= 1.2)] <- "Moderate"
  response[which(das2 > 3.2 & das2 <= 5.1 & (das1-das2) <= 0.6)] <- "No response"
  response[which(das2 > 5.1 & (das1-das2) > 1.2)] <- "Moderate"
  response[which(das2 > 5.1 & (das1-das2) > 0.6 & (das1-das2) <= 1.2)] <- "No response"
  response[which(das2 > 5.1 & (das1-das2) <= 0.6)] <- "No response"

  return(response)
}
