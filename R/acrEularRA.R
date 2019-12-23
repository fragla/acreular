##acrEularRA class
new_acrEularRA <- function(ljc=numeric(), sjc=numeric(), duration=numeric(),
                          crp=numeric(), esr=numeric(),
                          ccp=numeric(), rf=numeric()) {

  value <- list(ljc=ljc, sjc=sjc, duration=duration, crp=crp, esr=esr, ccp=ccp, rf=rf)

  attr(value, "class") <- "acrEularRA"
  return(value)
}

##AcrEularRA score
acrEularRAScore <- function(object, ...) {
  arguments <- list(...)
  message(paste(names(arguments)))
  aprScore(object, ...) + durationScore(object) + jointScore(object) + serologyScore(object, ...)
}

acrEularClassification <- function(object, ...) {
  arguments <- list(...)
  message(paste(names(arguments)))
  score <- acrEularRAScore(object, ...)

  if(is.na(score))
    return()

  classif <- ifelse(score >= 6, "RA (ACR/EULAR 2010)", "UA")
  return(classif)
}

##Acute Phase Reactant Score
aprScore <- function(object, crp.uln=10, esr.uln=15) { ##change crp to 6?
  score <- 0

  if((is.na(crp.uln) && is.na(esr.uln))) {
    stop("No upper limit of normal specified.")
  }

  if(length(crp.uln)==0 && length(esr.uln)==0) {
    stop("No upper limit of normal specified.")
  }

  if(!is.numeric(crp.uln) || !is.numeric(esr.uln)) {
    stop("No upper limit of normal specified.")
  }

  if(is.na(object$crp) && is.na(object$esr)) {
    return(NA)
  }

  if(length(object$crp)==0 && length(object$esr)==0) {
    return(NA)
  }

  if(!is.numeric(object$crp) || !is.numeric(object$esr)) {
    return(NA)
  }

  if(object$crp > crp.uln || object$esr > esr.uln) {
    score <- 1
  }

  return(score)
}

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

jointScore <- function(object) {
  score <- 0

  large <- object$ljc
  small <- object$sjc

  if(is.na(large) || is.na(small)) {
    return(NA)
  }
  if (!all.equal(large, as.integer(large)) || !all.equal(small, as.integer(small))) {
    return(NA)
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

serologyScore <- function(object, ccp.uln=10, rf.uln=20) { #pass in as high/low/neg?
  score <- 0

  if(is.na(object$ccp) && is.na(object$rf)) {
    return(NA)
  }

  ccp <- serologyClassification(object$ccp, ccp.uln)
  rf <- serologyClassification(object$rf, rf.uln)

  categories <- c("negative", "low", "high")

  ccp <- tolower(ccp)
  rf <- tolower(rf)

  if((!is.na(ccp) & tolower(ccp) == "low") || (!is.na(rf) & tolower(rf) == "low")) {
    score <- 2
  }

  if((!is.na(ccp) & tolower(ccp) == "high") || (!is.na(rf) & tolower(rf) == "high")) {
    score <- 3
  }

  if(!tolower(ccp) %in% categories && !tolower(rf) %in% categories) {
    score <- NA
  }

  return(score)
}

serologyClassification <- function(score, uln) {
  ####VALIDATIONS for score and uln~~~~~
  if(!is.numeric(score)) {
    return(NA)
  }

  classification <- "negative"

  if(score > uln * 3) {
    classification <- "high"
  } else if(score > uln) {
    classification <- "low"
  }

  return(classification)
}
