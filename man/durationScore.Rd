% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acrEularRA.R
\name{durationScore}
\alias{durationScore}
\title{Calculate duration component score}
\usage{
durationScore(object)
}
\arguments{
\item{object}{acrEularRA object}
}
\description{
Calculate duration component score. Converts patients self-reported duration
of signs and symptoms (in days) to a numeric score
}
\examples{
acreular <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Abnormal",serology="High")
durationScore(acreular)

}
