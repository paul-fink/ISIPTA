### Estimate the geolocation of the authors.

source("../scraper/xml.R")
source("../locator/locations.R")

geolocation_by_hand <- function(city, contry_name, country_code) {

  loc <- NULL
  loc
}

author_geolocation <- function(authorLocationRow) {
  
  geolocData <- geolocation_by_hand(authorLocationRow$city,
                                    authorLocationRow$country_name,
                                    authorLocationRow$country_code)
  
  if(is.null(geolocData)) {
    geolocData <- lookup_location(authorLocationRow$city,
                                  authorLocationRow$country_name,
                                  authorLocationRow$country_code)
  }
  authorLocationRow$city_lat <- geolocData$city_lat
  authorLocationRow$city_lon <- geolocData$city_lon
  
  names(authorLocationRow)[names(authorLocationRow) == "author"] <- "name"
  author <- xmlAuthor(authorLocationRow)
  loc <- xmlLocation(authorLocationRow)
  addChildren(author, kids = list(loc))
}

prepare_authors_geolocation <- function(year) {
  
  file <- sprintf("../xml-cleaned/isipta%s.xml", year)
  raw <- readLines(file, encoding = "UTF-8")
  xml <- xmlTreeParse(raw, useInternalNodes = TRUE)
  authors <- getNodeSet(xml, "//author")
  free(xml)
  
  dups <- duplicated(sapply(authors, function(x) xmlValue(x[[1]])))
  authors <- authors[!dups]
  authors_list <- lapply(authors, function(x) {
    data.frame(year = year,
               author = xmlValue(x[[1]]),
               email = xmlValue(x[[2]]),
               stringsAsFactors = FALSE)
  })
  authors_save <- do.call(rbind, authors_list)
  authors_save$department <-
    authors_save$university <-
    authors_save$city_lon <-
    authors_save$city_lat <- 
    authors_save$city <-
    authors_save$country_name <- 
    authors_save$country_code <- NA 
  filename <- sprintf("../raw/%s/locations%s_to_clean.csv", year, year)
  write.csv(authors_save, file = filename)
  message("Please fill at least the city, the country name and country code column and afterwards remove the string '_to_clean' from the file name!")
  filename
}

scrap_authors_geolocation <- function(year) {
  
  authorLocations  <- read.csv(paste0("../raw/", year, "/locations", year, ".csv"), stringsAsFactors = FALSE, colClasses = "character")
  
  res <- xmlAuthors(year)
  for(i in seq_len(NROW(authorLocations))) {
    addChildren(res, author_geolocation(authorLocations[i,]))
  }
  res
}


### Scrap authors geolocation:

doit <- function(year) {
  i <- scrap_authors_geolocation(year)
  saveXML(i, file = sprintf("../xml/geoloc_authors%s.xml", year))
}

#doit(1999)
#doit(2001)
#doit(2003)
#doit(2005)
#doit(2007)
#doit(2009)
#doit(2011)
#doit(2013)
#doit(2015)
#doit(2017)
