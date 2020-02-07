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

#' Calculate ACR 20/50/70 response
#'
#' Calculate ACR 20/50/70 response from SJC, TJC, patient global assessment,
#' patient pain VAS, physician global assessment, HAQ and
#' measurements.
#'
#' @param sjc1 numeric initial swollen joint count.
#' @param sjc2 numeric follow up swollen joint count.
#' @param tjc1 numeric initial tender joint count.
#' @param tjc1 numeric follow up tender joint count.
#' @param ptGA1 numeric initial patient global assessment score.
#' @param ptGA2 numeric follow up patient global assessment score.
#' @param ptPain1 numeric initial patient pain assessment score.
#' @param ptPain2 numeric initial patient pain assessment score.
#' @param phGA1 numeric initial physician global assessment score.
#' @param phGA2 numeric follow up physician global assessment score.
#' @param haq1 numeric initial health assessment questionairre score.
#' @param haq2 numeric follow health assessment questionairre score.
#' @param apr1 numeric initial acute phase reactant value.
#' @param apr2 numeric follow up acute phase reactant value.
#'
#' @examples
#'
#'   acrResponse(tjc1=6, tjc2=1, sjc1=6, sjc2=1,
#'               ptGA1=17, ptGA2=1, ptPain1=11, ptPain2=1,
#'               phGA1=60, phGA2=5, haq1=5, haq2=3, apr1=24, apr2=5)
#'
#' @export
acrResponse <- function(tjc1, tjc2, sjc1, sjc2, ptGA1, ptGA2, ptPain1, ptPain2, phGA1, phGA2, haq1, haq2, apr1, apr2) {
  percentDecrease <- function(original, current) {
    decrease <- (original - current) / original * 100
    return(decrease)
  }

  ##percent decrease values
  tjc <- percentDecrease(tjc1, tjc2)
  sjc <- percentDecrease(sjc1, sjc2)
  ptGA <- percentDecrease(ptGA1, ptGA2)
  ptPain <- percentDecrease(ptPain1, ptPain2)
  phGA <- percentDecrease(phGA1, phGA2)
  haq <- percentDecrease(haq1, haq2)
  apr <- percentDecrease(apr1, apr2)

  if(any(is.na(c(tjc, sjc, ptGA, ptPain, phGA, haq, apr))))
    return(NA)

  if(tjc >= 70 & sjc >= 70 & sum(c(ptGA, ptPain, phGA, haq, apr) >=70) >= 3) {
    return("ACR70")
  } else if (tjc >= 50 & sjc >= 50 & sum(c(ptGA, ptPain, phGA, haq, apr) >=50) >= 3) {
    return("ACR50")
  } else if (tjc >= 20 & sjc >= 20 & sum(c(ptGA, ptPain, phGA, haq, apr) >=20) >= 3) {
    return("ACR20")
  } else {
    return("No response")
  }
}
