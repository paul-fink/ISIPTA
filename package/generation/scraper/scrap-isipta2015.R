### ISIPTA 2015
library(bibtex)
source("base-scraper.R")
source("xml.R")
source("../locator/locate-authors.R")

parse_proceedings15 <- function(fileBib, fileLocations, year, date, location) {
  
  bis_raw <- read.bib(fileBib)
  bis <- bis_raw[sapply(seq_along(bis_raw), function(i) {grepl("-ISIPTA15p",attributes(bis_raw[i])$names)}),]
  
  authorLocations  <- read.csv(fileLocations)
  
  proc <- xmlProceedings(year, date, location)
  for ( bibentry in bis )
    proc$addNode(parse_paper15(bibentry, authorLocations))
  
  proc
}

parse_paper15 <- function(bibentry, authorLocations) {
  
  authors <- sapply(bibentry$author, function(x) {
    paste(paste(x$given, collapse = " "), paste(x$family, collapse = " "))
  })
  preAuthor <- matrix(apply(authorLocations[authorLocations$author %in% authors, c("author","email"), drop = FALSE], 
                               2, function(x) {as.character(x)}), ncol = 2)
  colnames(preAuthor) <- c("name","email")
  emails <- lapply(seq_len(NROW(preAuthor)), function(i) {preAuthor[i,]})
  
  xmlPaper(sprintf("%#03d",as.integer(gsub(".*/(\\d+)\\.pdf", "\\1", bibentry$url))),
           clean_title(bibentry$title),
           if(!is.null(bibentry$keywords)) {clean_keywords(bibentry$keywords, pretext = "^")},
           clean_abstract(bibentry$abstract),
           clean_pdf(bibentry$url),
           clean_authors(authors, emails)
  )
}

i15 <- parse_proceedings15(fileBib = "../raw/2015/isipta2015.bib",
                           fileLocations = "../raw/2015/locations2015.csv",
                           year = "2015",
                           date = c("2015-07-20", "2015-07-24"),
                           location = c(
                             country_name = "Italy",
                             country_code ="IT",
                             city = "Pescara",
                             city_lat = "42.46024",
                             city_lon = "14.21021",
                             university = "",
                             department = ""
                             )
                           )

saveXML(i15$value(), file = "../xml/isipta2015.xml")

doit(2015)
  
