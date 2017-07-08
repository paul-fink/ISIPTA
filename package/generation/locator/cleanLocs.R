# #########################################################
# #### Code for documentary purpose how the locations  ####
# #### where obtained; functionality used in main now  ####  
# #########################################################
#
# ### Get files for manual cleaning
# load("../data/authors_locations.RData")
#
# ### Manual cleaning is required
# dflist <- split(authors_locations, f = as.factor(authors_locations$year))
# ndflist <- names(dflist)
# lapply(seq_along(dflist), function(i) {
#   year <- ndflist[i]
#   write.csv(dflist[[i]], file = paste0("raw/", year, "/locations", year, ".csv"))
# })
# 
# ### Manual cleaning is performed
# 
# ### Get the coordinates after the manual cleaning
# library(ggmap)
# 
# toLookList <- lapply(seq(1999, by = 2, length = 10), function(year) {
#   df <- read.csv(file = paste0("../raw/", year, "/locations", year, ".csv"), stringsAsFactors = FALSE)
#   tolook <- paste(df$city, df$country_name, df$country_code, sep = ",")
#   tolook
# })
# ### Unique list of locations
# uLook <- unique(unlist(toLookList))
# gc <- geocode(uLook)
# 
# ### now make the data set
# locs <- do.call(rbind, lapply(strsplit(uLook, ","), function(x) {
#   data.frame(country_code=x[3], country_name = x[2], city = x[1], stringsAsFactors = FALSE)
# }))
# locs$city_lon_raw = gc$lon
# locs$city_lat_raw = gc$lat
# locs$city_lon<-format(locs$city_lon_raw, nsmall = 5, digits = 1, trim = TRUE)
# locs$city_lat<-format(locs$city_lat_raw, nsmall = 5, digits = 1, trim = TRUE)
# ### To deal with the guy who intentionally provided no details about his whereabouts
# locs[(locs$city == "NA"),] <- rep(NA, ncol(locs))
#
# ### Applying the newly acquired coordinates on the files
# lapply(seq(2001, by = 2, length = 9), function(year) {
#   filename <- paste0("raw/", year, "/locations", year, ".csv")
#   df <- read.csv(file = filename, stringsAsFactors = FALSE, row.names = 1)
#   mdf <- merge(data.frame(city = df$city, id = seq_along(df$city), stringsAsFactors = FALSE), locs, by = "city")
#   mdfo <- mdf[order(mdf$id),]
#   df$city_lat <- mdfo$city_lat
#   df$city_lon <- mdfo$city_lon
#   write.csv(df, file = filename, row.names = FALSE)
# })
