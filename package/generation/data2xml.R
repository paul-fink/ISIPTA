library(xml2)

isEmpty <- function(x) {
  return(any(is.null(x)) || any(is.na(x)))
}

isNotEmpty <- function(x) {
  return(!isEmpty(x))
}

addCountry <- function(node, x) {
  countryNode <- xml_add_child(node, "country")
  if(isEmpty(x$country_code) || isEmpty(x$country_name)) stop("x missing values in columns 'country_code' and 'country_name'!")
  xml_add_child(countryNode, "code", as.character(x$country_code))
  xml_add_child(countryNode, "name", as.character(x$country_name))
}

addCity <- function(node, x) {
  cityNode <- xml_add_child(node, "city")
  if(isEmpty(x$city)) stop("x missing value in column 'city'!")
  xml_add_child(cityNode, "name", as.character(x$city))
  if(isNotEmpty(x$city_lat)) {
    xml_add_child(cityNode, "latitude", format(x$city_lat, nsmall = 5))
  }
  if(isNotEmpty(x$city_lon)) {
    xml_add_child(cityNode, "longitude", format(x$city_lon, nsmall = 5))
  }
}

addUniversity <- function(node, x) {
  universityNode <- xml_add_child(node, "university")
  if(isNotEmpty(x$university)) {
    xml_add_child(universityNode, "name", as.character(x$university))
  }
  if(isNotEmpty(x$department)) {
    xml_add_child(universityNode, "department", as.character(x$department))
  }
}

addLocation <- function(node, x) {
  locationNode <- xml_add_child(node, "location")
  addCountry(locationNode, x)
  addCity(locationNode, x)
  addUniversity(locationNode, x)
}

addConferenceDuration <- function(node, x) {
  durationNode <- xml_add_child(node, "date")
  if(isEmpty(x$date_start) || isEmpty(x$date_end)) stop("x missing values in columns 'date_start' and 'date_end'!")
  xml_add_child(durationNode, "start", as.character(x$date_start))
  xml_add_child(durationNode, "end", as.character(x$date_end))
}

addConferenceInfo <- function(node, year) {
  conf <- conferences[conferences$year == year,]
  conferenceNode <- xml_add_child(node, "conference")
  addConferenceDuration(conferenceNode, conf)
  addLocation(conferenceNode, conf)
}

addPaperAuthors <- function(node, id) {
  paperauthors <- papers_authors[papers_authors$id == id,"author"]
  authorsNode <- xml_add_child(node, "authors")
  lapply(paperauthors, function(x) {xml_add_child(authorsNode, "author", as.character(x))})
}

addKeywords <- function(node, id) {
  paperkeywords <- papers_keywords[papers_keywords$id == id,"keyword"]
  keywordsNode <- xml_add_child(node, "keywords")
  if(length(paperkeywords)) {
    lapply(paperkeywords, function(x) {xml_add_child(keywordsNode, "keyword", as.character(x))})
  }
}

addPaper <-function(node, id) {
  paper <- papers[papers$id == id,]
  paperNode <- xml_add_child(node, "paper")
  xml_add_child(paperNode, "id", paper$id)
  xml_add_child(paperNode, "title", as.character(paper$title))
  addPaperAuthors(paperNode, id)
  addKeywords(paperNode, id)
  if(isNotEmpty(paper$abstract)) {
    xml_add_child(paperNode, "abstract", as.character(paper$abstract))
  }
  if(isNotEmpty(paper$url)) {
    xml_add_child(paperNode, "pdf", as.character(paper$url))
  }
}

addPapers <- function(node, year) {
  paperIds <- papers[papers$year == year, "id"]
  lapply(paperIds, function(x) {addPaper(node, x)})
}

addProceedings <- function(node, year) {
  proceedingNode <- xml_add_child(node, "proceedings")
  xml_add_child(proceedingNode, "year", year)
  addConferenceInfo(proceedingNode, year)
  addPapers(proceedingNode, year)
}

doc <- xml_new_root("isipta")
years <- seq(from = 1999, by = 2, length = 10)
lapply(years, function(x) {addProceedings(doc, x)})
write_xml(doc, "isipta.xml")



addAuthorList <- function(node, year) {
  authorlistNode <- xml_add_child(node, "authorlist")
  xml_add_child(authorlistNode, "year", as.integer(year))
}



doc <- xml_new_root("isiptaauthors")






  
