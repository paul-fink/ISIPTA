library(ggmap)

source("../scraper/xml.R")

initXMLLocations <- function() {
  
  filename <- "../xml-cleaned/geolocations.xml"
  
  toLookList <- lapply(dir("../raw/"), function(year) {
    df <- read.csv(file = paste0("../raw/", year, "/locations", year, ".csv"), stringsAsFactors = FALSE)
    tolook <- paste(df$city, df$country_name, df$country_code, sep = ",")
    tolook
  })
  ## Unique list of locations
  uLook <- unique(unlist(toLookList))
  gc <- geocode(location = gsub("(.*),\\w{2}", "\\1", uLook), source = "dsk")
  
  locs <- do.call(rbind, lapply(strsplit(uLook, ","), function(x) {
    data.frame(country_code = x[3], country_name = x[2], city = x[1], stringsAsFactors = FALSE)
  }))
  locs$city_lon_raw = gc$lon
  locs$city_lat_raw = gc$lat
  locs$city_lon<-format(locs$city_lon_raw, nsmall = 5, digits = 1, trim = TRUE)
  locs$city_lat<-format(locs$city_lat_raw, nsmall = 5, digits = 1, trim = TRUE)
  # To deal with the guy who intentionally provided no details about his whereabouts
  locs[(locs$city == "NA"),] <- rep(NA, ncol(locs))
  
  res <- xmlLocations()
  for(i in seq_len(NROW(locs))) {
    addChildren(res, xmlLocationShort(locs[i, c("country_code",
                                                  "country_name",
                                                  "city",
                                                  "city_lat",
                                                  "city_lon")]))
  }
  saveXML(res, file = filename)
  warning("Several coordinates of places might be incorrect, especially those in the United States")
}


lookup_location <- function(city, country_name, country_code) {
  
  filename <- "../xml-cleaned/geolocations.xml"
  if(!file.exists(filename)) {
    initXMLLocations()
  }
  xml <- xmlTreeParse(filename, useInternalNodes = TRUE, encoding = "UTF-8")
  locations <- getNodeSet(xml, "//location")
  locList <- sapply(locations, function(x) {
    paste(xmlValue(x[[2]][[1]]),xmlValue(x[[1]][[2]]),sep=",")
  })
  lookupID <- which(locList == paste(city, country_name, sep = ","))
  
  if(length(lookupID)) {
    loc <- locations[[lookupID]]
    res <- data.frame(city_lat = xmlValue(loc[[2]][[2]]),
               city_lon = xmlValue(loc[[2]][[3]]),
               stringsAsFactors = FALSE)
        
  } else {
    gc <- geocode(paste(city, country_name, sep = ","), source = "dsk", force = TRUE)
    res <- data.frame(country_code = country_code,
                      country_name = country_name,
                      city = city,
                      city_lat = format(gc$lat, nsmall = 5, digits = 1, trim = TRUE),
                      city_lon = format(gc$lon, nsmall = 5, digits = 1, trim = TRUE),
                      stringsAsFactors = FALSE)
    addChildren(xmlRoot(xml), xmlLocationShort(res))
    saveXML(xmlRoot(xml), file = filename)
    res <- res[,-c(1:3)]
    warning(sprintf("Please validate the entry in '%s' for %s and rerun", gsub("../", "", filename, fixed = TRUE), city))
  }
  free(xml)
  res
}
