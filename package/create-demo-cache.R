# expects to be in path 'packageroot'

# helper function: execute a simple demo
load_demo <- function(demofile, envir) {
  tmpfile <- tempfile()
  demotext <- readLines(demofile)
  demotext <- demotext[!grepl("ISIPTA.eProceedings", demotext)]
  writeLines(demotext, tmpfile)
  sys.source(tmpfile, envir = envir)
  unlink(tmpfile)
}

load_demos <- function() {
  
  op <- options(warn = -1)
  envir <- new.env()
  
  # when the package is packed
  sys.source("R/cache.R")
  demos <- paste0("demo/", c("simple-summary.R",
                             "authors-locations.R",
                             "country-contributions.R",
                             "authors-per-paper.R",
                             "regular-contributors.R",
                             "papers-per-author.R",
                             "coauthors-per-author.R",
                             "coauthors-network.R",
                             "coauthors-worldmap.R",
                             "keywords.R"))
    
  # load all data into the new environment
  load("data/papers.RData", envir = envir)
  load("data/papers_authors.RData", envir = envir)
  load("data/authors_locations.RData", envir = envir)
  load("data/conferences.RData", envir = envir)
  load("data/papers_keywords.RData", envir = envir)
  
  
  for ( demo in demos ) {
    load_demo(demo, envir)
  }
  
  options(op)

  envir
}

demos <- load_demos()
save(demos, file = "inst/extdata/demo-cache.RData")
