######################################################################
### Generate of the released files:                                ###
###   1. RData files with data.frames generated from the XML       ###
###   2. Global XML file containing all information                ###
######################################################################

library("XML")

options(stringsAsFactors = FALSE)

XML_DIR <- "xml-cleaned/"
DATA_DIR <- "../data"


### Conferencs: ######################################################

xmlLocation2data <- function(x) {
  c(country_code = xmlValue(x[["country"]][["code"]]),
    country_name = xmlValue(x[["country"]][["name"]]),
    city = xmlValue(x[["city"]][["name"]]),
    city_lat = xmlValue(x[["city"]][["latitude"]]),
    city_lon = xmlValue(x[["city"]][["longitude"]]),
    university = xmlValue(x[["university"]][["name"]]),
    department = xmlValue(x[["university"]][["department"]]))
}


xml2conferences <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- c(year = xmlValue(proc[["year"]]))
    location <- xmlLocation2data(proc[["conference"]][["location"]])
    date <- c(date_start = xmlValue(proc[["conference"]][["date"]][["start"]]),
              date_end = xmlValue(proc[["conference"]][["date"]][["end"]]))

    free(tree)

    c(year, location, date)
  }

  xmlfiles <- list.files(XML_DIR,
                         pattern = "isipta.*\\.xml",
                         full.names = TRUE)

  conferences <- lapply(xmlfiles, extract)
  conferences <- do.call(rbind, conferences)

  conferences[conferences == ""] <- NA

  conferences <- as.data.frame(conferences)
  conferences <- within(conferences, {
    year <- as.integer(year)
    date_start <- as.Date(date_start)
    date_end <- as.Date(date_end)
    country_name <- factor(country_name)
    country_code <- factor(country_code)
    city_lat <- as.numeric(city_lat)
    city_lon <- as.numeric(city_lon)
  })

  conferences
}


conferences <- xml2conferences()
save(conferences, file = sprintf("%s/conferences.RData", DATA_DIR))


### Papers: ##########################################################

xml2papers <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- xmlValue(proc[["year"]])
    papers <- getNodeSet(proc, "//paper",
                         fun = function(paper) {
                           id <- xmlValue(paper[["id"]])
                           title <- xmlValue(paper[["title"]])
                           keywords <- paste(xpathSApply(paper[["keywords"]],
                                                   ".//keyword",
                                                   xmlValue),
                                             collapse = ",")
                           abstract <- xmlValue(paper[["abstract"]])
                           url <- xmlValue(paper[["pdf"]])
                           c(year = year,
                             id = sprintf("%s%s", year, id),
                             title = title,
                             abstract = abstract,
                             url = url)
                         })

    free(tree)

    do.call(rbind, papers)
  }

  xmlfiles <- list.files(XML_DIR,
                         pattern = "isipta.*\\.xml",
                         full.names = TRUE)

  papers <- lapply(xmlfiles, extract)
  papers <- do.call(rbind, papers)
  papers <- as.data.frame(papers)
  papers <- within(papers, {
    year <- as.integer(year)
    id <- as.integer(id)
  })

  papers
}


papers <- xml2papers()
save(papers, file = sprintf("%s/papers.RData", DATA_DIR))


### Authors: #########################################################

xml2papers_authors <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- xmlValue(proc[["year"]])
    authors <- getNodeSet(proc, "//paper",
                          fun = function(paper) {
                            id <- xmlValue(paper[["id"]])

                            authors <- getNodeSet(paper[["authors"]], ".//author",
                                                  fun = function(x) {
                                                    xmlValue(x[["name"]])
                                                  })

                            data.frame(year = year,
                                       id = sprintf("%s%s", year, id),
                                       author = unlist(authors))
                          })

    free(tree)

    do.call(rbind, authors)
  }

  xmlfiles <- list.files(XML_DIR,
                         pattern = "isipta.*\\.xml",
                         full.names = TRUE)

  authors <- lapply(xmlfiles, extract)
  authors <- do.call(rbind, authors)
  authors <- as.data.frame(authors)
  authors <- within(authors, {
    year <- as.integer(year)
    id <- as.integer(id)
    author <- factor(author)
  })

  authors
}


papers_authors <- xml2papers_authors()
save(papers_authors, file = sprintf("%s/papers_authors.RData", DATA_DIR))


### Keywords: ########################################################

xml2keywords <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- xmlValue(proc[["year"]])
    keywords <- getNodeSet(proc, "//paper",
                           fun = function(paper) {
                             id <- xmlValue(paper[["id"]])

                             keywords <- xpathSApply(paper[["keywords"]],
                                                     ".//keyword",
                                                     xmlValue)
                             if(length(keywords)) {
                                data.frame(year = year,
                                          id = sprintf("%s%s", year, id),
                                          keyword = keywords)
                             } else {
                               NULL
                             }
                           })

    free(tree)

    do.call(rbind, keywords)
  }

  xmlfiles <- list.files(XML_DIR,
                         pattern = "isipta.*\\.xml",
                         full.names = TRUE)

  keywords <- lapply(xmlfiles, extract)
  keywords <- do.call(rbind, keywords)
  keywords <- as.data.frame(keywords)
  keywords <- within(keywords, {
    year <- as.integer(year)
    id <- as.integer(id)
    keyword <- iconv(keyword, from = "UTF-8", to = "latin1")
  })

  keywords
}

papers_keywords <- xml2keywords()
save(papers_keywords, file = sprintf("%s/papers_keywords.RData", DATA_DIR))


### Locations: #######################################################

xml2authors_locations <- function() {
  extract <- function(xmlfile) {
    tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
    proc <- xmlRoot(tree)

    year <- xmlValue(proc[["year"]])
    locations <- getNodeSet(proc, "//author",
                            fun = function(author) {
                              c(year = year,
                                author = xmlValue(author[["name"]]),
                                email = xmlValue(author[["email"]]),
                                xmlLocation2data(author[["location"]]))
                            })

    free(tree)

    do.call(rbind, locations)
  }

  xmlfiles <- list.files(XML_DIR,
                         pattern = "geoloc_authors.*\\.xml",
                         full.names = TRUE)

  locations <- lapply(xmlfiles, extract)
  locations <- do.call(rbind, locations)

  locations[locations == "NA"] <- NA
  locations[locations == "N/A"] <- NA

  locations <- as.data.frame(locations)
  locations <- within(locations, {
    year <- as.integer(year)
    author <- factor(author)
    country_name <- factor(country_name)
    country_code <- factor(country_code)
    city <- factor(city)
    city_lat <- as.numeric(city_lat)
    city_lon <- as.numeric(city_lon)
  })

  locations
}

authors_locations <- xml2authors_locations()
save(authors_locations, file = sprintf("%s/authors_locations.RData", DATA_DIR))


### General XML: #####################################################

xml2onefile <- function(filename) {
  mergeXmlByYear <- function(node, year) {
    
    file_isipta <- sprintf(paste0(XML_DIR, "isipta%s.xml"), year)
    file_location <- sprintf(paste0(XML_DIR, "geoloc_authors%s.xml"), year)
    
    xml_isipta <- xmlTreeParse(file_isipta, useInternalNodes = TRUE, encoding = "UTF-8")
    xml_location <- xmlTreeParse(file_location, useInternalNodes = TRUE, encoding = "UTF-8")
    authors_isipta <- getNodeSet(xml_isipta, "//author")
    
    authors_loc <- getNodeSet(xml_location, "//author")
    authors_loc_names <- sapply(authors_loc, function(x) {xmlValue(x[[1]])})
    
    for(i in seq_along(authors_isipta)){
      anode <- authors_isipta[[i]]
      bnode <- xmlClone(authors_loc[[which(authors_loc_names == xmlValue(anode[[1]]))]])
      replaceNodes(anode, bnode)
    }
    
    addChildren(node, xmlRoot(xml_isipta))

    free(xml_location)
    free(xml_isipta)
    NULL
  }
  
  procList <- newXMLNode("proceedingslist")
  years <- as.numeric(names(table(gsub("[^0-9]","",list.files(XML_DIR, pattern = ".*(\\d){4}\\.xml")))))
  for(year in years) {
    mergeXmlByYear(procList, year)
  }
  saveXML(procList, file = filename)
}

xml2onefile(filename = "../inst/xml/collected_proceedings.xml")


# ####################################################################
# ##### Documenting the generated data by autogenerated Rd-Files #####
# ##### Functionality is deprecated                              ##### 
# ####################################################################
# ### Helper Function ################################################
# MAN_DIR <- "../man"
# 
# customRdFormat <- function(x) {
#   fmt <- c("\\format{", 
#            paste("  A data frame with", NROW(x),
#                  "observations on the following",
#                  ifelse(NCOL(x) == 1, "variable.",paste(NCOL(x), "variables."))),
#            "  \\describe{")
#   for (i in names(x)) {
#     xi <- x[[i]]
#     fmt <- c(fmt, 
#              paste0("    \\item{\\code{", i, "}}{",
#                     if (inherits(xi, "ordered")) {
#                       lo <- levels(xi)
#                       ltu <- c(lo[c(1,length(lo))], names(sort(table(xi), decreasing = TRUE))[1:10])[1:10] 
#                       lo[!(lo %in% ltu)] <- " "
#                       nl <- strsplit(gsub("(, )+" ,"," , paste(lo, collapse = ",")),",")[[1]]
#                       paste0("an ", data.class(xi), " factor with levels", 
#                              (if(length(lo) > 10) {" (only extreme and 10 most frequent): "} else {": "}),
#                              paste0(ifelse(nchar(nl)>0, paste0("\\code{",nl,"}"), gsub(".*","\\ldots", nl)), collapse = " < ")) 
#                     } else if (inherits(xi, "factor")) {
#                       lo <- levels(xi)
#                       ltu <- names(sort(table(xi), decreasing = TRUE))[1:10]
#                       paste0("a factor with levels", (if(length(lo)>10) {" (only 10 most frequent): "} else {": "}),
#                              paste0("\\code{", lo[(lo %in% ltu)], "}" , collapse = ", "),
#                              (if(length(lo)>10) {", \\ldots"} else {""}))
#                     } else if (is.vector(xi)) {
#                       paste("a", data.class(xi), "vector")
#                     } else {
#                       paste("a", data.class(xi))
#                     }, "}"))
#   }
#   fmt <- c(fmt, "  }", "}")
#   fmt
# }
# 
# create_Rd <- function(object, title, description, seealso = NULL, references = NULL, examples = NULL) {
#   name <- as.character(substitute(object))
#   rd <- promptData(object, filename = NA, name = name)
# 
#   rd$title <- sprintf("\\title{%s}", title)
#   rd$description <- c("\\description{", description, "}")
#   rd$format <- customRdFormat(object)
# 
#   if ( !is.null(seealso) )
#     rd$seealso <- c("\\seealso{",
#                     paste("\\code{\\link{", seealso, "}}", sep = "", collapse = ", "),
#                     "}")
# 
#   rd$source <- c("\\source{",
#                  "Information scraped from the ISIPTA websites \\url{http://www.sipta.org/};",
#                  "",
#                  "For details of generation see \\url{https://github.com/paul-fink/ISIPTA/tree/master/package/generation} and its subdirectories",
#                  "}")
# 
#   rd$details <- NULL
#   if(!is.null(references)) {
#     rd$references <- c("\\references{",
#                        references,
#                        "}")
#   }
#   
#   rd$examples <- if(is.null(examples)) {
#     rd$examples[-3]
#   } else {
#     c("\\examples{",
#       sprintf("data(%s)", name),
#       "",
#       examples,
#       "}")
#   }
# 
#   ## Rd file:
#   cat(unlist(rd), file = sprintf("%s/%s.Rd", MAN_DIR, name), sep = "\n")
#   invisible(rd)
# }
#
# ### Conferencs: ####################################################
# create_Rd(object = conferences,
#           title = "ISIPTA conference facts",
#           description = "Dates, locations, etc. about the ISIPTA conferences.",
#           seealso = "papers",
#           examples = c("# Years", "table(conferences$year)"))
#
# ### Papers: ########################################################
# create_Rd(object = papers,
#           title = "ISIPTA papers",
#           description = "Information about the ISIPTA papers.",
#           seealso = c("papers_authors", "authors_locations", "papers_keywords"),
#           examples = c("# The title of the author's first paper",
#                        "papers[papers$id == 2013014,]$title"))
# 
# ### Authors: #######################################################
# create_Rd(object = papers_authors,
#           title = "Authors of the ISIPTA papers",
#           description = "Authors of the ISIPTA papers; the names are normalized to ASCII characters.",
#           seealso = c("papers", "authors_locations", "papers_keywords"),
#           examples = c("# Co-authors for paper with id 2013014",
#                        "papers_authors[papers_authors$id == 2013014,]",
#                        "",
#                        "# Table of all authors",
#                        "table(papers_authors$author)"))
# 
# ### Keywords: ######################################################
# create_Rd(object = papers_keywords,
#           title = "Keywords of the ISIPTA papers",
#           description = "Keywords of the ISIPTA papers. The keywords are not normalized.",
#           seealso = c("papers", "authors_locations", "papers_authors"))
# 
# ### Locations: #####################################################
# create_Rd(object = authors_locations,
#           title = "(Estimated) location of authors",
#           description = c("Location information about the ISIPTA authors.","\\cr",
#                           paste("The geocodes are obtained from the Data Science Toolkit collection",
#                                 "by the help of the \\code{geocode} function of the \\code{ggmap} package")),
#           seealso = c("papers_authors"),
#           references = paste("Kahle, D. and Wickham, H. (2013)  ggmap: Spatial Visualization with ggplot2.",
#                              "\\emph{The R Journal}, \\bold{5}(1), 144--161.",
#                              "\\url{http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf}"))

