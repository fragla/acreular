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

#' Create a new acrRA class
#'
#' \code{new_acrRA} returns an acrRA object.
#'
#' @param sjc numeric swollen joint count.
#' @param tjc numeric tender joint count.
#' @param ptGA numeric patient global assessment score.
#' @param ptPain numeric patient pain assessment score.
#' @param phGA numeric physician global assessment score.
#' @param haq numeric health assessment questionairre score.
#' @param apr numeric acute phase reactant value.
#'
#' @return An acrRA object.
#'
#' @examples
#'  obj <- new_acrRA(sjc=8, tjc=12, ptGA=50, ptPain=29, phGA=60, haq=0.875, apr=15)
#'
#' @export
new_acrRA <- function(sjc=numeric(), tjc=numeric(), ptGA=numeric(), ptPain=numeric(), phGA=60, haq=numeric(), apr=numeric()) {

  value <- list(sjc=sjc, tjc=tjc, ptGA=ptGA, ptPain=ptPain, phGA=phGA, haq=haq, apr=apr)

  attr(value, "class") <- "acrRA"
  return(value)
}

#' Helper function for creating an acrRA class.
#'
#' Creates an acr RA object from different parameters used in calculating
#' ACR20/50/70 scores.
#'
#' @param sjc numeric swollen joint count. Numeric between 0 and 28 of total
#' number of swollen.
#' @param tjc numeric tender joint count. Numeric between 0 and 28 of total
#' number of twollen.
#' @param ptGA numeric patient global assessment score between 0 and 100.
#' @param ptPain numeric patient pain assessment score between 0 and 100.
#' @param phGA numeric physician global assessment score between 0 and 100.
#' @param haq numeric health assessment questionairre score between 0 and 3.
#' @param apr numeric acute phase reactant value. Either CRP or ESR
#'
#' @return An acrRA object.
#'
#' @examples
#'  obj1 <- acrRA(sjc=8, tjc=12, ptGA=50, ptPain=35, phGA=60, haq=0.850, apr=15)
#'  obj2 <- acrRA(sjc=4, tjc=7, ptGA=20, ptPain=25, phGA=30, haq=0.350, apr=10)
#'
#'  acrResponse(obj1, obj2)
#'
#'
#' @export
acrRA <- function(sjc=numeric(), tjc=numeric(), ptGA=numeric(), ptPain=numeric(), phGA=numeric(), haq=numeric(), apr=numeric()) {

  object <- new_acrRA()

  ##Joint
  if(!is.empty(sjc) && sjc >=0 && sjc <=28) {
    object$sjc <- sjc
  }

  if(!is.empty(tjc) && tjc >=0 && tjc <=28) {
    object$tjc <- tjc
  }

  #Patient Global Assessment
  if(!is.empty(ptGA) && ptGA >=0 && ptGA <=100) {
    object$ptGA <- ptGA
  }

  #Patient Pain Assessment
  if(!is.empty(ptPain) && ptPain >=0 && ptPain <=100) {
    object$ptPain <- ptPain
  }

  #Physician Global Assessment
  if(!is.empty(phGA) && phGA >=0 && phGA <=100) {
    object$phGA <- phGA
  }

  #Health Assessment Questionnaire
  if(!is.empty(haq) && haq >=0 && haq <=3) { #include others?
    object$haq <- haq
  }

  #Acute phase reactant
  if(!is.empty(apr) && apr >=0) {
    object$apr <- apr
  }

  return(object)

}

#' Calculate ACR 20/50/70 response
#'
#' Calculate ACR 20/50/70 response from SJC, TJC, patient global assessment,
#' patient pain VAS, physician global assessment, HAQ and
#' measurements.
#'
#' @param acr1 acrRA object with initial measurements.
#' @param acr2 acrRA object with follow up measurements.
#' @param na.rm logical. Should missing values be removed.
#'
#' @examples
#'   acr1 <- acrRA(sjc=8, tjc=12, ptGA=50, ptPain=35, phGA=60, haq=0.850, apr=15)
#'   acr2 <- acrRA(sjc=4, tjc=7, ptGA=20, ptPain=25, phGA=30, haq=0.350, apr=10)
#'
#'   acrResponse(acr1, acr2)
#'
#' @export
acrResponse <- function(acr1, acr2, na.rm=TRUE) {
  percentDecrease <- function(original, current) {
    decrease <- (original - current) / original * 100
    return(decrease)
  }

  ##percent decrease values
  tjc <- percentDecrease(acr1$tjc, acr2$tjc)
  sjc <- percentDecrease(acr1$sjc, acr2$sjc)
  ptGA <- percentDecrease(acr1$ptGA, acr2$ptGA)
  ptPain <- percentDecrease(acr1$ptPain, acr2$ptPain)
  phGA <- percentDecrease(acr1$phGA, acr2$phGA)
  haq <- percentDecrease(acr1$haq, acr2$haq)
  apr <- percentDecrease(acr1$apr, acr2$apr)

  if(is.empty(tjc) || is.empty(sjc)) {
    return("Incomplete data")
  }

  if(na.rm==FALSE && any(is.empty(c(tjc, sjc, ptGA, ptPain, phGA, haq, apr))))
    return("Incomplete data")

  if(tjc >= 70 & sjc >= 70 & sum(c(ptGA, ptPain, phGA, haq, apr) >=70, na.rm=na.rm) >= 3) {
    return("ACR70")
  }

  else if (tjc >= 50 & sjc >= 50 & sum(c(ptGA, ptPain, phGA, haq, apr) >=50, na.rm=na.rm) >= 3) {
    return("ACR50")
  }

  else if (tjc >= 20 & sjc >= 20 & sum(c(ptGA, ptPain, phGA, haq, apr) >=20, na.rm=na.rm) >= 3) {
    return("ACR20")
  }

  else {
    return("No response")
  }
}
