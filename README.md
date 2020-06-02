
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/fragla/acreular.svg?branch=master)](https://travis-ci.org/fragla/acreular)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/fragla/acreular?branch=master&svg=true)](https://ci.appveyor.com/project/fragla/acreular)
[![Codecov test
coverage](https://codecov.io/gh/fragla/acreular/branch/master/graph/badge.svg)](https://codecov.io/gh/fragla/acreular?branch=master)
<!-- badges: end -->

# acreular

The American College of Rheumatology (ACR) and the European League
Against Rheumatism (EULAR) individually and collaboratively have
produced diagnositic classification, response and functional status
criteria for a range of different rheumatic diseases. The acreular
package aims to bring together methods for calculating these measures
into a single R package, making it simpler to calculate these values for
whole patient cohorts. Calculations can be performed either from within
R or by using the accompanying web application, which also enables the
graphical visualisation of data and the calculation of comparative
statistics.

The package is currently focused on ACR/EULAR RA related disease
measures. However, we plan to further develop the package by adding
additional RA related criteria and by adding ACR/EULAR related measures
for other rheumatic disorders.

## Installation

You can install acreular from [GitHub](https://github.com/) with the
command:

``` r
 # install.packages("devtools")
 devtools::install_github("fragla/acreular")
```

## Quick Start

**Load the package**

``` r
library(acreular)
```

**ACR/EULAR 2010 calculation**

``` r
#create an acreular object

#using duration in days and apr and serology classifications
obj1 <- acrEularRA(ljc=8, sjc=12, duration=43, apr="Normal", serology="High")

#using onset/assessment dates and CRP/ESR, CCP/RF measurements (cut offs can be configured)
obj2 <- acrEularRA(ljc=8, sjc=12,
              onset=as.Date("2010-01-01"), assessment=as.Date("2010-02-13"),
              crp=5, esr=12, ccp=32, rf=71)

#both ways create identical objects
all.equal(obj1, obj2)
#> [1] TRUE

acrEularRAClassification(obj1)
#> [1] "RA (ACR/EULAR 2010)"
```

**EULAR response**

``` r
#single calculation
eularResponse(5.31, 1.3)
#> [1] "Good"

#multiple calculations
baseline <- c(5.24, 3.6, 1.2)
followup <- c(1.30, 3.3, 1.8)

eularResponse(baseline, followup)
#> [1] "Good"        "No response" "No response"
```

**ACR 20/50/70**

``` r
#create an acr objects
acr1 <- acrRA(sjc=8, tjc=12, ptGA=50, ptPain=35, phGA=60, haq=0.850, apr=15)
acr2 <- acrRA(sjc=4, tjc=7, ptGA=20, ptPain=25, phGA=30, haq=0.350, apr=10)

acrResponse(acr1, acr2)
#> [1] "ACR20"
```

## Shiny web interface

The calculation (and visualisation) of ACR/EULAR classifications and
EULAR response can also be performed by upload of a CSV or Excel file
using the packaged [Shiny](https://shiny.rstudio.com) app. This requires
the [shiny](https://cran.r-project.org/package=shiny),
[DT](https://cran.r-project.org/package=DT),
[FSA](https://cran.r-project.org/package=FSA),
[ggplot2](https://cran.r-project.org/package=ggplot2),
[ggiraph](https://cran.r-project.org/package=ggiraph),
[ggiraphExtra](https://cran.r-project.org/package=ggiraphExtra),
[mime](https://cran.r-project.org/package=mime),
[PMCMRplus](https://cran.r-project.org/package=PMCMRplus),
[readxl](https://cran.r-project.org/package=readxl),
[shinycssloaders](https://cran.r-project.org/package=shinycssloaders)
and [shinyWidgets](https://cran.r-project.org/package=shinyWidgets)
packages. The CSV/Excel headers should be the same as the names of the
vector passed to the ***acrEularRA*** function i.e. ljc, sjc, duration
(or onset/assessment dates), apr classification (or CRP/ESR measurement)
and serology classification (or CCP/RF measurement). Example data files
are included with the web application.

The app is launched using the ***shiny\_acreular*** function.

``` r
shiny_acreular()
```

Alternatively, it can be accessed without installing R/Shiny/eq5d by
visiting [shinyapps.io](https://fragla.shinyapps.io/acreular-eq5d).

## License

This project is licensed under the MIT License - see the
[LICENSE.md](https://github.com/fragla/acreular/blob/master/LICENSE.md)
file for details.
