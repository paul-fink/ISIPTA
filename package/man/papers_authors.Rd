\name{papers_authors}
\alias{papers_authors}
\docType{data}
\title{Authors of the ISIPTA papers}
\description{
Authors of the ISIPTA papers; the names are normalized to ASCII characters.
}
\usage{data("papers_authors")}
\format{
  A data frame with 835 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{id}}{a numeric vector}
    \item{\code{author}}{a factor with levels (only 10 most frequent): \code{Barbara Vantaggi}, \code{Enrique Miranda}, \code{Fabio Cozman}, \code{Frank Coolen}, \code{Gert de Cooman}, \code{Lev Utkin}, \code{Marco Zaffalon}, \code{Matthias Troffaes}, \code{Serafin Moral}, \code{Thomas Augustin}, \ldots}
  }
}
\source{
Information scraped from the ISIPTA websites \url{http://www.sipta.org/};

For details of generation see \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation} and its subdirectories
}
\examples{
data(papers_authors)

# Co-authors for paper with id 2013014
papers_authors[papers_authors$id == 2013014,]

# Table of all authors
table(papers_authors$author)
}
\keyword{datasets}
\seealso{
\code{\link{papers}}, \code{\link{authors_locations}}, \code{\link{papers_keywords}}
}
