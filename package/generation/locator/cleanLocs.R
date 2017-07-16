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
# source("locations.R")
#
# initXMLLocations()
#
# ### An another cleaning step of the coordinates