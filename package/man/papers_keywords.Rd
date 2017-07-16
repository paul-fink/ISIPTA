\name{papers_keywords}
\alias{papers_keywords}
\docType{data}
\title{Keywords of the ISIPTA papers}
\description{
Keywords of the ISIPTA papers. The keywords are not normalized.
}
\usage{data("papers_keywords")}
\format{
  A data frame with 2180 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{id}}{a numeric vector}
    \item{\code{keyword}}{a character vector}
  }
}
\source{
Information scraped from the ISIPTA websites \url{http://www.sipta.org/};

For details of generation see \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation} and its subdirectories
}
\examples{
data(papers_keywords)
}
\keyword{datasets}
\seealso{
\code{\link{papers}}, \code{\link{authors_locations}}, \code{\link{papers_authors}}
}
