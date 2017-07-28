### Coauthors network on the world map.

library("ISIPTA.eProceedings")
library(plyr)
library(geosphere)
library(rworldmap)

data("authors_locations", package = "ISIPTA.eProceedings")
data("papers_authors", package = "ISIPTA.eProceedings")


### Coauthor pairs calculation: ######################################
coauthors_pairs <- ddply(papers_authors, .(id),
                         function(x) {
                           if ( nrow(x) > 1 ) {
                             authors <- sort(as.character(x$author))
                             pairs <- combn(authors, 2)
                             
                             data.frame(author1 =
                                          factor(pairs[1, ],
                                                 levels = levels(x$author)),
                                        
                                        author2 =
                                          factor(pairs[2, ],
                                                 levels = levels(x$author)),
                                        
                                        year = x$year[1],
                                        id = x$id[1])
                           }
                         })

coauthors_pairs <- within(coauthors_pairs, {
  year <- ordered(year)
  id <- factor(id)
})


## Extend with geolocations:
coauthors_pairs_loc <- merge(coauthors_pairs, authors_locations,
                         by.x = c("author2", "year"),
                         by.y = c("author", "year"),
                         sort = FALSE)

coauthors_pairs_loc <- merge(coauthors_pairs_loc, authors_locations,
                         by.x = c("author1", "year"),
                         by.y = c("author", "year"),
                         suffixes = c(".author2", ".author1"),
                         sort = FALSE)
coauthors_pairs_loc <- subset(coauthors_pairs_loc,
                              (!is.na(city_lon.author2) &
                               !is.na(city_lat.author2) &
                               !is.na(city_lon.author2) &
                               !is.na(city_lat.author2)))


### Visualization of the world map: ##################################

plot(getMap(), main = "Collaboration Map")
for ( i in seq(length = nrow(coauthors_pairs_loc)) ) {
  p1 <- c(x = coauthors_pairs_loc[i, "city_lon.author1"],
          y = coauthors_pairs_loc[i, "city_lat.author1"])

  p2 <- c(x = coauthors_pairs_loc[i, "city_lon.author2"],
          y = coauthors_pairs_loc[i, "city_lat.author2"])

  l <- gcIntermediate(p1, p2, n = 100,
                      breakAtDateLine = TRUE,
                      addStartEnd = TRUE)

  if ( is.list(l) )
    lapply(l, lines, col = 2, lwd = 1)
  else
    lines(l, col = 2, lwd = 1)
}
