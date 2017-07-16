\name{papers}
\alias{papers}
\docType{data}
\title{ISIPTA papers}
\description{
Information about the ISIPTA papers.
}
\usage{data("papers")}
\format{
  A data frame with 424 observations on the following 5 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{id}}{a numeric vector}
    \item{\code{title}}{a character vector}
    \item{\code{abstract}}{a character vector}
    \item{\code{url}}{a character vector}
  }
}
\source{
Information scraped from the ISIPTA websites \url{http://www.sipta.org/};

For details of generation see \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation} and its subdirectories
}
\examples{
data(papers)

# The title of the author's first paper
papers[papers$id == 2013014,]$title
}
\keyword{datasets}
\seealso{
\code{\link{papers_authors}}, \code{\link{authors_locations}}, \code{\link{papers_keywords}}
}
