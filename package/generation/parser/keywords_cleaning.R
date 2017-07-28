# ### Normalization of keywords (after ISIPTA 2017)
# 
# library(XML)
# 
# source("../scraper/base-scraper.R")
# 
# XML_FILES <- list.files("../xml-cleaned",
#                         pattern = "isipta(\\d){4}\\.xml", 
#                         full.names = TRUE)
# 
# normalizeKeywordsXML <- function(xmlfile) {
#   tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
#   proc <- xmlRoot(tree)
#   kwNodes <- getNodeSet(proc, "//keyword")
#   lapply(kwNodes, function(x) {
#     xmlValue(x) <- normalize_keyword(xmlValue(x))
#     NULL
#   })
#   saveXML(tree, file = xmlfile)
#   NULL
# }
# 
# for(xmlfile in XML_FILES) {
#   normalizeKeywordsXML(xmlfile = xmlfile)
# }
# 
