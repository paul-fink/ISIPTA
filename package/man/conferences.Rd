\name{conferences}
\alias{conferences}
\docType{data}
\title{ISIPTA conference facts}
\description{
Dates, locations, etc. about the ISIPTA conferences.
}
\usage{data("conferences")}
\format{
  A data frame with 10 observations on the following 10 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{country_code}}{a factor with levels: \code{AT}, \code{BE}, \code{CH}, \code{CZ}, \code{FR}, \code{GB}, \code{IT}, \code{US}}
    \item{\code{country_name}}{a factor with levels: \code{Austria}, \code{Belgium}, \code{Czech Republic}, \code{France}, \code{Italy}, \code{Switzerland}, \code{United Kingdom}, \code{United States}}
    \item{\code{city}}{a character vector}
    \item{\code{city_lat}}{a numeric vector}
    \item{\code{city_lon}}{a numeric vector}
    \item{\code{university}}{a character vector}
    \item{\code{department}}{a character vector}
    \item{\code{date_start}}{a Date}
    \item{\code{date_end}}{a Date}
  }
}
\source{
Information scraped from the ISIPTA websites \url{http://www.sipta.org/};

For details of generation see \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation} and its subdirectories
}
\examples{
data(conferences)

# Years
table(conferences$year)
}
\keyword{datasets}
\seealso{
\code{\link{papers}}
}
