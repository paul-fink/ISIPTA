#' @docType data
#' @keywords datasets
#' @name authors_locations
#' 
#' @title (Estimated) location of authors
#' 
#' @description
#' Location information about the ISIPTA authors.
#' \cr
#' The geocodes are obtained from the Data Science Toolkit collection 
#' by the help of the \code{geocode} function of the \code{ggmap} package.
#'
#' @usage data(authors_locations)
#'
#' @format
#' A data frame with 729 observations on the following 10 variables,
#' where each row contains information on one author in a certain year.
#' \describe{
#'  \item{\code{year}}{Integer vector of the year of the }
#'  \item{\code{author}}{A factor of the authors' names.}
#'  \item{\code{email}}{A character vector with the authors' 
#'        email adress, if available.}
#'  \item{\code{country_code}}{A factor of the country code 
#'        according to ISO 3166 alpha 2.}
#'  \item{\code{country_name}}{A factor of the country name.}
#'  \item{\code{city}}{A factor of the city name.}
#'  \item{\code{city_lat}}{A numeric vector with the city's latitude.}
#'  \item{\code{city_lon}}{A numeric vector with the city's longitude.}
#'  \item{\code{university}}{A character vector containing the author's 
#'        affiliated university. Currently contains only NA}
#'  \item{\code{department}}{A character vector containing the author's
#'        affiliated department within the university.
#'        Currently contains only NA}
#' }
#'
#' @details
#' The author names are unique within a conference, i.e. within same year, 
#' but are not unique overall to account for changing universities.
#'
#' @source
#' Information scraped from the ISIPTA websites \url{http://www.sipta.org/}.
#'
#' For details of generation see 
#' \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation}
#' and its subdirectories.
#'
#' @references
#' Kahle, D. and Wickham, H. (2013)  ggmap: Spatial Visualization with ggplot2.
#' \emph{The R Journal}, \bold{5}(1), 144--161. 
#' \url{https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf}
#'
#' @seealso
#' \code{\link{papers_authors}}
#'
#' @examples
#' data(authors_locations)
"authors_locations"

#' @docType data
#' @keywords datasets
#' @name conferences
#' 
#' @title ISIPTA conference facts
#' 
#' @description 
#' Dates, locations, etc. about the ISIPTA conferences.
#'
#' @usage data(conferences)
#'
#' @format
#' A data frame with 10 observations on the following 10 variables.
#' \describe{
#'  \item{\code{year}}{An integer vector with the year of the conference}
#'  \item{\code{country_code}}{A factor of the country code according 
#'        to ISO 3166 alpha 2.}
#'  \item{\code{country_name}}{A factor of the country name.}
#'  \item{\code{city}}{A character vector of the city name.}
#'  \item{\code{city_lat}}{A numeric vector with the city's latitude.}
#'  \item{\code{city_lon}}{A numeric vector with the city's longitude.}
#'  \item{\code{university}}{A character vector containing the name of 
#'        the hosting university.}
#'  \item{\code{department}}{A character vector containing the department
#'        within the hosting university.}
#'  \item{\code{date_start}}{The start date as Date.}
#'  \item{\code{date_end}}{The end date as Date.}
#' }
#'
#' @source
#' Information scraped from the ISIPTA websites \url{http://www.sipta.org/}.
#'
#' For details of generation see 
#' \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation}
#' and its subdirectories.
#'
#' @seealso
#' \code{\link{papers}}
#'
#' @examples
#' data(conferences)
#' # Years
#' conferences$year
"conferences"

#' @docType data
#' @keywords datasets
#' @name papers
#' 
#' @title ISIPTA papers
#' 
#' @description 
#' Information about the ISIPTA papers.
#'
#' @usage data(papers)
#'
#' @format
#' A data frame with 424 observations on the following 5 variables,
#' where each row contains information about one paper.
#' \describe{
#'  \item{\code{year}}{The year in which the paper featured as numeric vector}
#'  \item{\code{id}}{A numeric vector as the unique paper id.
#'        The first 4 digits give the year and the last 3 the unique 
#'        identifier within the year.}
#'  \item{\code{title}}{The paper's title as a character vector}
#'  \item{\code{abstract}}{The paper's abstract as a character vector}
#'  \item{\code{url}}{The url for the paper's document in Portable Document
#'        Format (PDF) or PostScript (PS) as a character vector}
#' }
#' 
#' @details
#' The authors and keywords of a paper are in the dedicated objects
#' \code{\link{papers_authors}} and \code{\link{papers_keywords}}, 
#' respectively.
#'
#' @source
#' Information scraped from the ISIPTA websites \url{http://www.sipta.org/}.
#'
#' For details of generation see 
#' \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation}
#' and its subdirectories.
#'
#' @seealso
#' \code{\link{papers_authors}},
#' \code{\link{authors_locations}}, 
#' \code{\link{papers_keywords}}
#'
#' @examples
#' data(papers)
#' # The title of the author's first paper
#' papers[papers$id == 2013014,]$title
"papers"

#' @docType data
#' @keywords datasets
#' @name papers_authors
#' 
#' @title Authors of the ISIPTA papers
#' 
#' @description 
#' Matching authors to the ISIPTA papers
#'
#' @usage data(papers_authors)
#'
#' @format
#' A data frame with 848 observations on the following 3 variables,
#' where each row contains one author of a paper.
#' \describe{
#'  \item{\code{year}}{The year in which the paper featured as numeric vector.}
#'  \item{\code{id}}{A numeric vector as the unique paper id.
#'        See \code{\link{papers}} for details.}
#'  \item{\code{author}}{A factor with name of the papers' authors.
#'        The names are normalized to ASCII characters.}
#' }
#'
#' @source
#' Information scraped from the ISIPTA websites \url{http://www.sipta.org/}.
#'
#' For details of generation see 
#' \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation}
#' and its subdirectories.
#'
#' @seealso
#' \code{\link{papers}},
#' \code{\link{authors_locations}}, 
#' \code{\link{papers_keywords}}
#'
#' @examples
#' data(papers_authors)
#'
#' # Co-authors for paper with id 2013014
#' papers_authors[papers_authors$id == 2013014,]
#'
#' # Table of all authors
#' table(papers_authors$author)
"papers_authors"

#' @docType data
#' @keywords datasets
#' @name papers_keywords
#' 
#' @title Keywords of the ISIPTA papers
#' 
#' @description 
#' Keywords of the ISIPTA papers.
#'
#' @usage data(papers_keywords)
#'
#' @format
#' A data frame with 2184 observations on the following 3 variables,
#' where each row contains one keyword of a paper.
#' \describe{
#'  \item{\code{year}}{The year in which the paper featured as numeric vector.}
#'  \item{\code{id}}{A numeric vector as the unique paper id.
#'        See \code{\link{papers}} for details.}
#'  \item{\code{keyword}}{A character vector with the papers' keywords.
#'        The keywords are slightly normalized.}
#' }
#'
#' @source
#' Information scraped from the ISIPTA websites \url{http://www.sipta.org/}.
#'
#' For details of generation see 
#' \url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation}
#' and its subdirectories.
#'
#' @seealso
#' \code{\link{papers}},
#' \code{\link{authors_locations}}, 
#' \code{\link{papers_authors}}
#'
#' @examples
#' data(papers_keywords)
"papers_keywords"
