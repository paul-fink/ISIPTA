### clean up  

# papers
load(file = "../../data/papers.RData")
# for the time being; should be read from XML
papers$abstract <- papers$url <- NA
save(papers, file = "../../data/papers.RData")

# paper_authors
load(file = "../../data/papers_authors.RData")
papers_authors$author <- as.character(papers_authors$author)
papers_authors[grepl("^Itz..k Gilboa$", papers_authors$author), "author"] <- "Itzhak Gilboa"
papers_authors[grepl("^Tu Duong LE DUY$", papers_authors$author), "author"] <- "Tu Duong Le Duy"
papers_authors$author <- factor(papers_authors$author)
save(papers_authors, file = "../../data/papers_authors.RData")

# authors_locations
load(file = "../../data/authors_locations.RData")
# for the time being; should be read from XML
authors_locations$email <- NA
# Itzhak gilboa was wrongly written in HTML page
authors_locations <- authors_locations[-grep("^Itzack Gilboa$", authors_locations$author),]
authors_locations$city <- as.character(authors_locations$city)
authors_locations[grepl("Broomell", authors_locations$author) & authors_locations$year == 2011,"city"] <- "University Park"
authors_locations[grepl("Coolen-Schrijner", authors_locations$author) & authors_locations$year == 2011,"city"] <- "Durham"
authors_locations[grepl("^Gavle$", authors_locations$city),"city"] <- "Gaevle"
authors_locations[grepl("^Gent$", authors_locations$city),"city"] <- "Ghent"
authors_locations$city <- factor(authors_locations$city)
authors_locations$author <- as.character(authors_locations$author)
authors_locations[grepl("^Tu Duong LE DUY$", authors_locations$author), "author"] <- "Tu Duong Le Duy"
authors_locations$author <- factor(authors_locations$author)
authors_locations$country_code <- factor(as.character(authors_locations$country_code))
authors_locations$country_name <- factor(as.character(authors_locations$country_name))
authors_locations$year <- as.integer(as.character(authors_locations$year))
rownames(authors_locations) <- seq_len(NROW(authors_locations)
save(authors_locations, file = "../../data/authors_locations.RData")

# conferences
load("../../data/conferences.RData")
conferences[conferences$year==2007,"department"] <- "Faculty of Mathematics and Physics"
save(conferences, file = "../../data/conferences.RData")

# papers_keywords
load("../../data/papers_keywords.RData")
# cleaning of missing
papers_keywords[is.na(papers_keywords$keyword) & papers_keywords$id==2011038,"keyword"] <- "multivariate polya distribution"
papers_keywords[is.na(papers_keywords$keyword) & papers_keywords$id==2005003,"keyword"] <- c("dempster-shafer's belief function theory",  "spohn's epistemic beliefs theory")

# correct some wrong formatted values
tocorrect1 <- papers_keywords[papers_keywords$id==2003011,]
tocorrect2 <- papers_keywords[papers_keywords$id==2003009,]
correct1 <- data.frame(year = as.integer(2003),
                       id = as.integer(2003011), 
                       keyword = trimws(do.call(c,lapply(tocorrect1$keyword, function(x) {strsplit(x,";")[[1]]})))
)
correct2 <- data.frame(year = as.integer(2003),
                       id = as.integer(2003009), 
                       keyword = trimws(strsplit(paste(tocorrect2$keyword, collapse = ", "),";")[[1]])
)
papers_keywords <- rbind(papers_keywords[1:(min(as.numeric(rownames(tocorrect1)))-1),],
                         correct1,
                         papers_keywords[(max(as.numeric(rownames(tocorrect1)))+1):(min(as.numeric(rownames(tocorrect2)))-1),],
                         correct2,
                         papers_keywords[(max(as.numeric(rownames(tocorrect2)))+1):NROW(papers_keywords),])
rownames(papers_keywords) <- 1:NROW(papers_keywords)
save(papers_keywords, file = "../../data/papers_keywords.RData")
