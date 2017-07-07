### ISIPTA 2015
library(bibtex)

#fileBib <- "parser/2015/isipta15bibtex.bib"
#fileLocations <- "parser/2015/location2015_to_clean.csv"
#"location2015_to_clean.csv"
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
  
  xmlPaper(sprintf("2015%#03d",as.integer(gsub(".*/(\\d+)\\.pdf", "\\1", bibentry$url))),
           clean_title(bibentry$title),
           if(is.null(bibentry$keywords)) {NA_character_} else {clean_keywords(bibentry$keywords, pretext = "^")},
           clean_abstract(bibentry$abstract),
           clean_pdf(bibentry$url),
           clean_authors(authors, emails)
  )
}

i15 <- parse_proceedings15(fileBib = "../raw/2015/isipta15bibtex.bib",
                           fileLocations = "../raw/2015/location2015_to_clean.csv",
                           year = "2015",
                           date = c("2015-07-20", "2015-07-24"),
                           location = c(
                             country_name = "Italy",
                             country_code ="IT",
                             city = "Pescara",
                             city_lat = "42.46024",
                             city_lon = "14.21021",
                             university = NA_character_,
                             department = NA_character_
                             )
                           )
                           
      

saveXML(i15$value(), file = "../xml/isipta2015.xml")