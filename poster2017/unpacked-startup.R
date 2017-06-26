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

load("../Package/data/authors_locations.RData")
load("../Package/data/conferences.RData")
load("../Package/data/papers.RData")
load("../Package/data/papers_authors.RData")
load("../Package/data/papers_keywords.RData")
 
setwd("../Package/demo")                   # depends on...
source("simple-summary.R")        #
source("authors-locations.R")     #                        (produces some warnings, please ignore them)
source("country-contributions.R") # authors-locations.R    (produces some warnings, please ignore them)
source("authors-per-paper.R")     #
source("regular-contributors.R")  # simple-summary.R
source("papers-per-author.R")     #
source("coauthors-per-author.R")  # authors-per-paper.R
source("coauthors-network.R")     # regular-contributors.R (this takes a while)
source("coauthors-worldmap.R")    # coauthors-network.R

#setwd("./..")
#source("./R/graph.R")

# now all should be ready!






#
