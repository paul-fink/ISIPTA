isEmpty <- function(x) {is.null(x) || is.na(x) || nchar(x) < 1}

isipta17 <- read.csv2("isipta17.csv", stringsAsFactors = FALSE)

paperid_2017 <- seq(from = 2017001, length = NROW(isipta17))

## update papers
load("../../../data/papers.RData")
papers2017 <- data.frame(year = as.integer(2017),
                         id = paperid_2017,
                         title = isipta17$Title,
                         abstract = NA,
                         url = NA,
                         stringsAsFactors = FALSE, row.names = NULL
)
papers <- rbind(papers, papers2017)
save(papers, file = "../../../data/papers.RData")
print("Finished papers")

## update authors
load("../../../data/papers_authors.RData")
author2017_list <- lapply(seq_len(NROW(isipta17)), function(i) {
  data.frame(year = as.integer(2017),
             id = paperid_2017[i],
             author = sapply(strsplit(isipta17$Author[i],",")[[1]], function(x) {
               gsub("(.*)\\s\\(.*\\)", "\\1", x)
             }),
             stringsAsFactors = FALSE, row.names = NULL
  )
})
author2017 <- do.call("rbind", author2017_list)
papers_authors <- rbind(papers_authors,author2017)
papers_authors$author <- factor(as.character(papers_authors$author))
save(papers_authors, file = "../../../data/papers_authors.RData")
print("Finished /papers_authors")

# author locations (stub)
load("../../../data/authors_locations.RData")
authorlocations2017 <- data.frame(year = as.integer(2017), 
                                  author = unique(author2017$author),
                                  country_code = NA,
                                  country_name = NA,
                                  city = NA,
                                  city_lat =NA,
                                  city_lon =NA,
                                  university = NA,
                                  department = NA,
                                  email = NA,
                                  stringsAsFactors = FALSE, row.names = NULL)
authors_locations <- rbind(authors_locations,authorlocations2017)

# normalize the inputs
authors_locations$author <- factor(as.character(authors_locations$author))
authors_locations$city <- factor(as.character(authors_locations$city))
authors_locations$country_code <- factor(as.character(authors_locations$country_code))
authors_locations$country_name <- factor(as.character(authors_locations$country_name))
save(authors_locations, file = "../../../data/authors_locations.RData")
print("Finished /authors_locations")

# conference information
load("../../../data/conferences.RData")
conference2017 <- conferences[conferences$year == 2003, ]
conference2017$year <- as.integer(2017)
conference2017$date_start <- as.Date("2017-07-10")
conference2017$date_start <- as.Date("2017-07-14")
conferences <- rbind(conferences,conference2017)

conferences$country_code <- factor(as.character(conferences$country_code))
conferences$country_name <- factor(as.character(conferences$country_name))
save(conferences, file = "../../../data/conferences.RData")
print("Finished conferences")

# update keywords
load("../../../data/papers_keywords.RData")
keywords2017_list <- lapply(seq_len(NROW(isipta17)), function(i) {
  if(!isEmpty(isipta17$Keywords[i])) {
    data.frame(year = as.integer(2017),
               id = paperid_2017[i],
               keyword = tolower(trimws(strsplit(isipta17$Keywords[i],",")[[1]])),
               stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
})
keywords2017 <- do.call("rbind", keywords2017_list)
papers_keywords <- rbind(papers_keywords,keywords2017)
save(papers_keywords, file = "../../../data/papers_keywords.RData")
print("Finished papers_keywords")
