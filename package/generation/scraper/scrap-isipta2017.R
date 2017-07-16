### ISIPTA 2017
library(bibtex)
source("base-scraper.R")
source("xml.R")
source("../locator/locate-authors.R")

parse_proceedings17 <- function(fileBib, fileLocations, year, date, location) {
  
  bis <- read.bib(fileBib)
  
  authorLocations  <- read.csv(fileLocations, stringsAsFactors = FALSE)
  
  proc <- xmlProceedings(year, date, location)
  for ( i in seq_along(bis) )
    proc$addNode(parse_paper17(bis[[i]], i, authorLocations))
  
  proc
}

parse_paper17 <- function(bibentry, i, authorLocations) {
  
  authors <- sapply(bibentry$author, function(x) {
    paste(paste(x$given, collapse = " "), paste(x$family, collapse = " "))
  })
  preAuthor <- matrix(apply(authorLocations[authorLocations$author %in% authors, c("author","email"), drop = FALSE], 
                            2, function(x) {as.character(x)}), ncol = 2)
  colnames(preAuthor) <- c("name","email")
  emails <- lapply(seq_len(NROW(preAuthor)), function(i) {preAuthor[i,]})
  
  xmlPaper(sprintf("%#03d",as.integer(i)),
           clean_title(bibentry$title),
           if(!is.null(bibentry$keywords)) {clean_keywords(bibentry$keywords, pretext = "^")},
           clean_abstract(bibentry$abstract),
           clean_pdf(bibentry$pdf),
           clean_authors(authors, emails)
  )
}

i17 <- parse_proceedings17(fileBib = "../raw/2017/isipta2017.bib",
                           fileLocations = "../raw/2017/locations2017.csv",
                           year = "2017",
                           date = c("2017-07-10", "2017-07-14"),
                           location = c(
                             country_name = "Switzerland",
                             country_code ="CH",
                             city = "Lugano",
                             city_lat = "46.01008",
                             city_lon = "8.96004",
                             university = "University of Lugano",
                             department = ""
                           )
)
saveXML(i17$value(), file = "../xml/isipta2017.xml")

doit(2017)
