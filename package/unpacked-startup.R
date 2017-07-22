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

load("./data/authors_locations.RData")
load("./data/conferences.RData")
load("./data/papers.RData")
load("./data/papers_authors.RData")
load("./data/papers_keywords.RData")
 
                                         # depends on...
source("./demo/simple-summary.R")        #
source("./demo/authors-locations.R")     #                        (produces some warnings, please ignore them)
source("./demo/country-contributions.R") # authors-locations.R    (produces some warnings, please ignore them)
source("./demo/authors-per-paper.R")     #
source("./demo/regular-contributors.R")  # simple-summary.R
source("./demo/papers-per-author.R")     #
source("./demo/coauthors-per-author.R")  # authors-per-paper.R
source("./demo/coauthors-network.R")     # regular-contributors.R simple-summary.R
source("./demo/coauthors-worldmap.R")    # coauthors-network.R regular-contributors.R simple-summary.R

source("./R/cache.R")
source("./R/graph.R")

# now all should be ready!