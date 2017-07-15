# Startup script
source("unpacked-startup.R")

# Preparation of all needed variables across scripts
source("init.R")

# summary of papers and authors
source("1-summary.R")

# flows of contributors
source("2-authorflows.R")

# papers per author
source("3-papersPerAuthor.R")

# authors per paper
source("4-authorsPerPaper.R")

# wordclouds of keywords
source("5-wordcloud.R")

# author network evolutions
source("6-network.R")

# current network with ECSQARU
source("7-ecsqaru2017.R")

# helper functions
source("8-helper.R")
