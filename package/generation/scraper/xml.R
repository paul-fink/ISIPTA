
library("XML")


xmlProceedings <- function(year, date, location) {
  doc <- xmlTree("proceedings")

  doc$addNode("year", year)
  doc$addNode(xmlConference(date, location))

  doc
}


xmlPaper <- function(id, title, keywords, abstract, pdf, authors) {
  addChildren(newXMLNode("paper"),
              kids = list(newXMLNode("id", id),
              newXMLNode("title", title),
              xmlPaperAuthors(authors),
              xmlPaperKeywords(keywords),
              newXMLNode("abstract", abstract),
              newXMLNode("pdf", pdf)))
}


xmlConference <- function(date, location) {
  n <- newXMLNode("conference")
  addChildren(n, kids = list(xmlProceedingsDate(date),
                 xmlLocation(location)))
}


xmlProceedingsDate <- function(date) {
  n <- newXMLNode("date")
  addChildren(n, kids = list(newXMLNode("start", date[1]),
                 newXMLNode("end", date[2])))
}


xmlPaperKeywords <- function(keywords) {
  addChildren(newXMLNode("keywords"),
              kids = lapply(keywords, function(x) newXMLNode("keyword", x)))
}


xmlAuthor <- function(author) {
  n0 <- newXMLNode("author")
  n0 <- addChildren(n0,
                    kids = list(newXMLNode("name", author[1, "name"]),
                                newXMLNode("email", author[1, "email"])))
  n0
}

xmlPaperAuthors <- function(authors) {
  n <- newXMLNode("authors")

  for ( i in seq(length = NROW(authors)) ) {
    
    n0 <- xmlAuthor(authors[i,])
    n <- addChildren(n, kids = list(n0))
  }

  n
}


xmlLocationCountry <- function(location) {
  n <- newXMLNode("country")
  newXMLNode("code", location[1,"country_code"], parent = n)
  newXMLNode("name", location[1,"country_name"], parent = n)
  n
}


xmlLocationCity <- function(location) {
  n <- newXMLNode("city")
  newXMLNode("name", location[1, "city"], parent = n)
  newXMLNode("latitude", location[1, "city_lat"], parent = n)
  newXMLNode("longitude", location[1, "city_lon"], parent = n)
  n
}

xmlLocationUniversity <- function(location) {
  n <- newXMLNode("university")
  newXMLNode("name", location[1 ,"university"], parent = n)
  newXMLNode("department", location[1, "department"], parent = n)
  n
}


xmlLocation <- function(location) {
  n <- newXMLNode("location")
  addChildren(n, kids = list(xmlLocationCountry(location),
                 xmlLocationCity(location),
                 xmlLocationUniversity(location)))

}

xmlLocationShort <- function(location) {
  n <- newXMLNode("location")
  addChildren(n, kids = list(xmlLocationCountry(location),
                             xmlLocationCity(location)))
}


xmlDomain <- function(name, location) {
  n <- newXMLNode("domain")
  addChildren(n, kids = list(newXMLNode("name", name),
                 xmlLocation(location)))
}


xmlLocations <- function() {
  newXMLNode("locations")
}

xmlDomains <- function() {
  xmlTree("domains")
}


xmlAuthors <- function(year) {
  doc <- newXMLNode("authors")
  newXMLNode("year", year, parent = doc)
  doc
}
