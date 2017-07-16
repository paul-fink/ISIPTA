\name{authors_locations}
\alias{authors_locations}
\docType{data}
\title{(Estimated) location of authors}
\description{
Location information about the ISIPTA authors.
\cr
The geocodes are obtained from the Data Science Toolkit collection by the help of the \code{geocode} function of the \code{ggmap} package
}
\usage{data("authors_locations")}
\format{
  A data frame with 716 observations on the following 10 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{author}}{a factor with levels (only 10 most frequent): \code{Barbara Vantaggi}, \code{Enrique Miranda}, \code{Fabio Cozman}, \code{Gert de Cooman}, \code{Jirina Vejnarova}, \code{Lev Utkin}, \code{Marco Zaffalon}, \code{Matthias Troffaes}, \code{Paolo Vicig}, \code{Thomas Augustin}, \ldots}
    \item{\code{email}}{a character vector}
    \item{\code{country_code}}{a factor with levels (only 10 most frequent): \code{BE}, \code{BR}, \code{CH}, \code{DE}, \code{ES}, \code{FR}, \code{GB}, \code{IT}, \code{RU}, \code{US}, \ldots}
    \item{\code{country_name}}{a factor with levels (only 10 most frequent): \code{Belgium}, \code{Brazil}, \code{France}, \code{Germany}, \code{Italy}, \code{Russian Federation}, \code{Spain}, \code{Switzerland}, \code{United Kingdom}, \code{United States}, \ldots}
    \item{\code{city}}{a factor with levels (only 10 most frequent): \code{Durham (UK)}, \code{Ghent}, \code{Granada}, \code{Manno}, \code{Munich}, \code{Perugia}, \code{Pittsburgh}, \code{Prague}, \code{Rome}, \code{Sao Paulo}, \ldots}
    \item{\code{city_lat}}{a numeric vector}
    \item{\code{city_lon}}{a numeric vector}
    \item{\code{university}}{a character vector}
    \item{\code{department}}{a character vector}
  }
}
\source{
Information scraped from the ISIPTA websites \url{http://www.sipta.org/};

For details of generation see \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation} and its subdirectories
}
\references{
Kahle, D. and Wickham, H. (2013)  ggmap: Spatial Visualization with ggplot2. \emph{The R Journal}, \bold{5}(1), 144--161. \url{http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf}
}
\examples{
data(authors_locations)
}
\keyword{datasets}
\seealso{
\code{\link{papers_authors}}
}
