library(bibtex)

bis_raw <- read.bib("isipta15bibtex.bib")
bis <- bis_raw[sapply(seq_along(bis_raw), function(i) {grepl("-ISIPTA15p",attributes(bis_raw[i])$names)}),]

paperid_2015 <- as.integer(paste0(2015, sapply(bis, function(x) {sprintf("%#03d",as.integer(gsub(".*/(\\d+)\\.pdf", "\\1", x$url)))})))

## update papers
load("../../../data/papers.RData")
papers2015 <- data.frame(year = as.integer(2015),
           id = paperid_2015,
           title = sapply(bis, function(i) {i$title}),
           abstract = as.character(sapply(bis, function(i) {gsub("\\s+"," ", i$abstract)})),
           url = sapply(bis, function(i) {i$url}),
           stringsAsFactors = FALSE, row.names = NULL
)
papers <- rbind(papers, papers2015)
save(papers, file = "../../../data/papers.RData")
print("Finished papers")

## update authors
load("../../../data/papers_authors.RData")
author2015_list <- lapply(seq_along(bis), function(i) {
  data.frame(year = as.integer(2015),
             id = paperid_2015[i],
             author = sapply(bis[i]$author, function(x) {
               paste(paste(x$given, collapse = " "), paste(x$family, collapse = " "))
               }),
             stringsAsFactors = FALSE
             )
})

author2015 <- do.call("rbind", author2015_list)
papers_authors <- rbind(papers_authors,author2015)

# normalize the inputs
papers_authors$author <- factor(as.character(papers_authors$author))
save(papers_authors, file = "../../../data/papers_authors.RData")
print("Finished papers_authors")

## update the author locations
load("../../../data/authors_locations.RData")
## add email adress

#unique_author2015 <- author2015$author[!duplicated(author2015$author)]
#easy <- unique_author2015[unique_author2015 %in% authors_locations$author]
#easyall <- authors_locations[as.character(authors_locations$author) %in% easy,]
#easy_uni <- unsplit(lapply(split(easyall, factor(easyall$author)), function(x) x[order(x$year,decreasing = TRUE),][1,]),levels(factor(easyall$author)))

#hard_uni <- cbind(year = 2015,
#      author = hard,
#      country_code = NA,
#      country_name = NA,
#      city = NA,
#      city_lat = NA,
#      city_lon = NA,
#      university = NA,
#      department =NA)
#locations2015 <- rbind(easy_uni,hard_uni)
#locations2015$year <- 2015
#write.csv(locations2015, "location2015_to_clean.csv",row.names = FALSE)

locations2015  <- read.csv("location2015_to_clean.csv")
authors_locations <- rbind(authors_locations,locations2015)

# normalize the inputs
authors_locations$author <- factor(as.character(authors_locations$author))
authors_locations$city <- factor(as.character(authors_locations$city))
authors_locations$country_code <- factor(as.character(authors_locations$country_code))
authors_locations$country_name <- factor(as.character(authors_locations$country_name))
save(authors_locations, file = "../../../data/authors_locations.RData")
print("Finished authors_locations")

## update conference
load("../../../data/conferences.RData")
conference2015 <- data.frame(year = as.integer(2015),
           country_code ="IT",
           country_name = "Italy",
           city = "Pescara",
           city_lat = 42.45840,
           city_lon = 14.20283,
           university = NA,
           department = NA,
           date_start = as.Date("2015-07-20"),
           date_end = as.Date("2015-07-24"),
           stringsAsFactors = FALSE
           )
conferences <- rbind(conferences,conference2015)

conferences$country_code <- factor(as.character(conferences$country_code))
conferences$country_name <- factor(as.character(conferences$country_name))
save(conferences, file = "../../../data/conferences.RData")
print("Finished conferences")

# update keywords
load("../../../data/papers_keywords.RData")
keywords2015_list <- lapply(seq_along(bis), function(i) {
  if(!is.null(bis[i]$keywords)) {
    data.frame(year = as.integer(2015),
             id = paperid_2015[i],
             keyword = tolower(trimws(strsplit(bis[i]$keywords,",")[[1]])),
             stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
})
keywords2015 <- do.call("rbind", keywords2015_list)
papers_keywords <- rbind(papers_keywords,keywords2015)
save(papers_keywords, file = "../../../data/papers_keywords.RData")
print("Finished papers_keywords")
