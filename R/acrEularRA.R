#' Create a new acrEularRA class
#'
#' \code{new_acrEularRA} returns an acrEularRA object.
#'
#' @param ljc numeric large joint count. Value between 0 and 10 of total
#' number of swollen and/or tender large joints.
#' @param sjc numeric small joint count. Value between 0 and 18 of total
#' number of swollen and/or tender small joints.
#' @param duration numeric patient’s self-report on the maximum duration
#' (in days) of signs and symptoms of any joint that is clinically
#' involved at the time of assessment.
#' @param apr character acute phase reactant levels. "Normal" or "Abnormal"
#' @param serology character CCP and/or rheumatoid factor levels. "Negative",
#' "Low" positive or "High" positive.
#'
#' @return An acrEularRA object.
#'
#' @examples
#'  obj <- new_acrEularRA(ljc=8, sjc=12, duration=43, apr="Normal", serology="High")
#'
#' @export
new_acrEularRA <- function(ljc=numeric(), sjc=numeric(), duration=numeric(),
                          apr=character(), serology=character()) {

  value <- list(ljc=ljc, sjc=sjc, duration=duration, apr=apr, serology=serology)

  attr(value, "class") <- "acrEularRA"
  return(value)
}

#' Helper function for creating an acrEularRA class.
#'
#' Creates an acrEular RA object from different parameters. Converts dates to
#' duration value and serology and acute phase reactant values to
#' classifications.
#'
#' @param ljc large joint count. Numeric between 0 and 10 of total
#' number of swollen and/or tender large joints.
#' @param sjc  small joint count. Numeric between 0 and 18 of total
#' number of swollen and/or tender small joints.
#' @param duration numeric patient’s self-report on the maximum duration
#' (in days) of signs and symptoms of any joint that is clinically
#' involved at the time of assessment.
#' @param onset Date signs and symptoms started.
#' @param assessment Date of initial assessment.
#' @param apr character acute phase reactant levels. "Normal" or "Abnormal"
#' @param crp numeric of C-reactive protein test result.
#' @param esr numeric of erythrocyte sedimentation rate test result.
#' @param crp.uln numeric for upper limit of normal for the C-reactive protein test.
#' @param esr.uln numeric for upper limit of normal for the erythrocyte sedimentation
#' rate test.
#' @param serology character CCP and/or rheumatoid factor levels. "Negative",
#' "Low" positive or "High" positive.
#' @param ccp numeric of ccp test result.
#' @param rf numeric of rheumatoid factor test result.
#' @param ccp.uln numeric for upper limit of normal for the ccp test
#' @param rf.uln numeric for upper limit of normal for the RF test
#'
#' @return An acrEularRA object.
#'
#' @examples
#'  obj1 <- acrEularRA(ljc=8, sjc=12, duration=43, apr="Normal", serology="High")
#'  obj2 <- acrEularRA(ljc=8, sjc=12,
#'              onset=as.Date("2010-01-01"), assessment=as.Date("2010-02-13"),
#'              crp=5, esr=12, ccp=32, rf=71)
#'
#'  all.equal(obj1, obj2)
#'
#' @export
acrEularRA <- function(ljc=numeric(), sjc=numeric(), duration=numeric(),
                       onset=NULL, assessment=NULL, apr=character(),
                       crp=numeric(), esr=numeric(), crp.uln=10, esr.uln=15,
                       serology=character(), ccp=numeric(), rf=numeric(),
                       ccp.uln=10, rf.uln=20) {

  object <- new_acrEularRA()

  ##Joint
  if(length(ljc)==1 && ljc >=0 && ljc <=10) {
    object$ljc <- ljc
  }

  if(length(sjc)==1 && sjc >=0 && sjc <=18) {
    object$sjc <- sjc
  }

  #Duration
  if(length(onset)==1 && length(assessment)==1) {
    if(!is.na(onset) && !is.na(assessment)) {
      object$duration <- datesToDuration(onset, assessment)
    }
  }

  if(length(duration)==1 && duration > 0) {
    if(length(object$duration) > 0 && object$duration!=duration) {
      stop("duration and onset/assessment parameters used that give different value.")
    }
    object$duration <- duration
  }

  ##Serology
  if((length(ccp)==1 && ccp>=0) || (length(rf)==1 && rf>=0)) {
    object$serology <- serologyClassification(ccp=ccp, rf=rf, ccp.uln=ccp.uln, rf.uln=rf.uln)
  }

  if(length(serology)==1 && tolower(serology) %in% c("negative", "low", "high")) {
    if(length(object$serology) > 0 && object$serology!=serology) {
      stop("Serology test results and serology classification give different values.")
    }
    object$serology <- serology
  }

  ##Acute phase reactants
  if((length(crp)==1 && crp>=0) || (length(esr)==1 && esr>=0)) {
    object$apr <- aprClassification(crp=crp, esr=esr, crp.uln=crp.uln, esr.uln=esr.uln)
  }

  if(length(apr)==1 && tolower(apr) %in% c("normal", "abnormal")) {
    if(length(object$apr) > 0 && object$apr!=apr) {
      stop("Acute phase reactant test results and classification give different values.")
    }
    object$apr <- apr
  }
  return(object)

}

#' Calculate acute phase reactant component score
#'
#' Calculate acute phase reactant component score. Converts acute phase
#' reactant status to a numeric score
#'
#' @param object acrEularRA object
#' @examples
#' acreular <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Abnormal",serology="High")
#' aprScore(acreular)
#'
#' @export
aprScore <- function(object) {
  score <- NA

  if(is.na(object$apr) || length(object$apr)==0) {
    return(NA)
  }

  if(object$apr=="Abnormal") {
    score <- 1
  } else if(object$apr=="Normal") {
    score <- 0
  } else {
    score <- NA
  }
  return(score)
}

#' Calculate duration component score
#'
#' Calculate duration component score. Converts patients self-reported duration
#' of signs and symptoms (in days) to a numeric score
#'
#' @param object acrEularRA object
#' @examples
#' acreular <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Abnormal",serology="High")
#' durationScore(acreular)
#'
#' @export
durationScore <- function(object) {
  score <- 0
  if(is.na(object$duration) || length(object$duration)==0) {
    return(NA)
  }

  if(object$duration > 42) {
    score <- 1
  }
  return(score)
}

#' Calculate joint component score
#'
#' Calculate joint component score. Converts patients swollen/tender joint
#' counts to a numeric score.
#'
#' @param object acrEularRA object
#' @examples
#' acreular <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Abnormal",serology="High")
#' jointScore(acreular)
#'
#' @export
jointScore <- function(object) {
  score <- 0

  large <- object$ljc
  small <- object$sjc

  if(is.na(large) || is.na(small)) {
    return(NA)
  }
  if (large != as.integer(large) || small != as.integer(small)) {
    stop("Non-integer joint count value provided.")
  }
  if (large==1) {
    score <- 0
  }
  if (large >= 2 & large <= 10) {
    score <- 1
  }
  if (small >=1 & small <= 3) {
    score <- 2
  }
  if (small >= 4 & small <= 10) {
    score <- 3
  }
  if (large + small > 10 & small >= 1) {
    score <- 5
  }
  return(score)
}

#' Calculate serology component score
#'
#' Calculate joint component score. Converts patients serology status to
#' a numeric score.
#'
#' @param object acrEularRA object
#' @examples
#' acreular <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Abnormal",serology="High")
#' serologyScore(acreular)
#'
#' @export
serologyScore <- function(object) {
  score <- NA

  if((!is.na(object$serology) & tolower(object$serology) == "negative")) {
    score <- 0
  }

  if((!is.na(object$serology) & tolower(object$serology) == "low")) {
    score <- 2
  }

  if((!is.na(object$serology) & tolower(object$serology) == "high")) {
    score <- 3
  }
  return(score)
}

#' Calculate ACR/EULAR 2010 RA score
#'
#' Calculates ACR/EULAR 2010 RA score from the individual components.
#'
#' @param object acrEularRA object
#' @param na.rm boolean specifying whether to remove NAs from calculation
#' @examples
#' acreular <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Abnormal",serology="High")
#' acrEularRAScore(acreular)
#'
#' @export
acrEularRAScore <- function(object, na.rm=FALSE) {
  sum(aprScore(object), durationScore(object), jointScore(object), serologyScore(object), na.rm=na.rm)
}

#' Calculate ACR/EULAR 2010 RA classification
#'
#' Calculates ACR/EULAR 2010 RA classification from the individual components.
#'
#' @param object acrEularRA object
#' @examples
#' acreular <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Abnormal",serology="High")
#' acrEularRAClassification(acreular)
#'
#' @export
acrEularRAClassification <- function(object) {

  score <- acrEularRAScore(object)

  if(is.na(score))
    return(NA)

  classif <- ifelse(score >= 6, "RA (ACR/EULAR 2010)", "UA")
  return(classif)
}

#' Calculate serology classification from test scores and ULN
#'
#' Calculates serology classification for CCP and/or rheumatoid factor given
#' the test scores and the upper limit of normal..
#'
#' @param ccp numeric of ccp test result
#' @param rf numeric of rheumatoid factor test result
#' @param ccp.uln numeric for upper limit of normal for the ccp test
#' @param rf.uln numeric for upper limit of normal for the RF test
#' @examples
#' serologyClassification(ccp=9, rf=21, ccp.uln=10, rf.uln=20)
#'
#' @export
serologyClassification <- function(ccp, rf, ccp.uln=10, rf.uln=20) {

  #what if only ccp or rf done
  ccp <- .serologyClassif(ccp, ccp.uln)
  rf <- .serologyClassif(rf, rf.uln)

  if(is.na(ccp) & is.na(rf)) {
    return(NA)
  }

  classif <- "Negative"

  if((!is.na(ccp) && ccp=="Low") || (!is.na(rf) && rf== "Low")) {
    classif <- "Low"
  }

  if((!is.na(ccp) && ccp=="High") || (!is.na(rf) && rf== "High")) {
    classif <- "High"
  }

  return(classif)
}

.serologyClassif <- function(score, uln) {
   if(is.na(score) || !is.numeric(score)) {
     return(NA)
   }

  if(is.na(uln) || !is.numeric(uln)) {
    stop("Incorrect serology ULN parameter used.")
  }

   classification <- "Negative"

   if(score > uln * 3) {
     classification <- "High"
   } else if(score > uln) {
     classification <- "Low"
   }

   return(classification)
}

#' Calculate acute phase reactant classification from test scores and ULN.
#'
#' Calculates acute phase reactant classification for given the C-reactive
#' protein and ESR test scores and the upper limit of normal.
#'
#' @param crp numeric of C-reactive protein test result.
#' @param esr numeric of erythrocyte sedimentation rate test result.
#' @param crp.uln numeric for upper limit of normal for the C-reactive protein test.
#' @param esr.uln numeric for upper limit of normal for the erythrocyte sedimentation
#' rate test.
#' @examples
#' aprClassification(crp=9, esr=16, crp.uln=10, esr.uln=15)
#'
#' @export
aprClassification <- function(crp, esr, crp.uln=10, esr.uln=15) {
  #what if only ccp or rf done
  crp <- .aprClassif(crp, crp.uln)
  esr <- .aprClassif(esr, esr.uln)

  if(is.na(crp) & is.na(esr)) {
    return(NA)
  }

  classif <- "Normal"

  if((!is.na(crp) && crp=="Abnormal") || (!is.na(esr) && esr== "Abnormal")) {
    classif <- "Abnormal"
  }

  return(classif)
}

.aprClassif <- function(score, uln) {
  if(is.na(score) || !is.numeric(score)) {
    return(NA)
  }

  if(is.na(uln) || !is.numeric(uln)) {
    stop("Incorrect APR ULN parameter used.")
  }

  classification <- "Normal"

  if(score > uln) {
    classification <- "Abnormal"
  }

  return(classification)
}
