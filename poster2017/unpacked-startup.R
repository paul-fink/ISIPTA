# for starting up the ISIPTA not-yet-package
# from within the 'package' folder "ISIPTA"

#install.packages(c("igraph", "reshape2", "colorspace", "geosphere", "rworldmap", "ggplot2", "plyr", "stringr"))
library(igraph)
library(reshape2)
library(colorspace)
library(geosphere)
library(rworldmap)
library(ggplot2)
library(plyr)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(xml2)
# to get the CM Roman fonts
library(extrafont)
if(!any(fonttable()$package == "fontcm")) font_install("fontcm")
loadfonts()


load("../package/data/authors_locations.RData")
load("../package/data/conferences.RData")
load("../package/data/papers.RData")
load("../package/data/papers_authors.RData")
load("../package/data/papers_keywords.RData")
 
                                                  # depends on...
source("../package/demo/simple-summary.R")        #
source("../package/demo/authors-locations.R")     #                        (produces some warnings, please ignore them)
source("../package/demo/country-contributions.R") # authors-locations.R    (produces some warnings, please ignore them)
source("../package/demo/authors-per-paper.R")     #
source("../package/demo/regular-contributors.R")  # simple-summary.R
source("../package/demo/papers-per-author.R")     #
source("../package/demo/coauthors-per-author.R")  # authors-per-paper.R
source("../package/demo/coauthors-network.R")     # regular-contributors.R (this takes a while)
#source("../package/demo/coauthors-worldmap.R")    # coauthors-network.R

#setwd("./..")
#source("../package/R/graph.R")

# now all should be ready!






#
