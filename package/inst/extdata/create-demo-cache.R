
load_demo <- function(demo, envir) {
  sys.source(demo, envir = envir)
}

load_demos <- function(package = TRUE) {
  
  op <- options(warn = -1)
  envir <- new.env()
  
  if(package){ # when the package is packed
    data("papers", package = "ISIPTA", envir = envir)
    data("papers_authors", package = "ISIPTA", envir = envir)
    data("authors_locations", package = "ISIPTA", envir = envir)
    data("conferences", package = "ISIPTA", envir = envir)
    data("papers_keywords", package = "ISIPTA", envir = envir)
  } else { # when package is not ready
    load("./../../data/papers.RData", envir = envir)
    load("./../../data/papers_authors.RData", envir = envir)
    load("./../../data/authors_locations.RData", envir = envir)
    load("./../../data/conferences.RData", envir = envir)
    load("./../../data/papers_keywords.RData", envir = envir)
  }
  # when the package is packed
  if(package) {
    demos <- list.files(system.file("demo", package = "ISIPTA"),
                        pattern = ".R", full.names = TRUE)
  # when package is not ready
  # (one needs to comment out all library("ISIPTA"), data(..., library="ISIPTA") statements in the demos)
  } else {
    sys.source("../../R/utils.R")
    demos <- paste0("./../../demo/", c("simple-summary.R",
                                       "authors-locations.R",
                                       "country-contributions.R",
                                       "authors-per-paper.R",
                                       "regular-contributors.R",
                                       "papers-per-author.R",
                                       "coauthors-per-author.R",
                                       "coauthors-network.R",
                                       "coauthors-worldmap.R")) 
  }
  
  for ( demo in demos ) {
    load_demo(demo, envir)
  }
  
  options(op)

  envir
}

# when the package is packed
#demos <- load_demos()
# when package is not ready
demos <- load_demos(package = FALSE)
save(demos, file = "demo-cache.RData")


