### Number of contributions by year and country.

library("ISIPTA.eProceedings")
library(ggplot2)
library(plyr)
library(reshape2)
library(rworldmap)

data("papers_authors", package = "ISIPTA.eProceedings")
data("authors_locations", package = "ISIPTA.eProceedings")


papers_authors_locations <- merge(papers_authors,
                                  authors_locations, all = TRUE)

## Countries per paper:
papers_countries <-
  ddply(papers_authors_locations, .(id),
        function(x) {
          ac <- t(as.matrix(table(x$country_code)))

          data.frame(year = x$year[1],
                     id = x$id[1],
                     nauthors = nrow(x),
                     as.data.frame(ac))
        })


## Country contributions per paper:
papers_country_contributions <-
  cbind(papers_countries[, 1:3],
        papers_countries[, -(1:3)] / papers_countries[, 3])


## Country contributions per year:
t3 <- daply(papers_country_contributions, .(year),
            function(x) {
              colSums(x[, -(1:3)])
            })

t3


### Visualization by region and by year: #############################

data("countryRegions", package = "rworldmap")

t3melt <- melt(t3, varnames = c("year", "country_code"))
# countryRegions now has only ISO3 country codes,
# so we must convert the two-letter ISO2 to the three-letter ISO3 code
# isoToName() does not like character vectors.
# Sometimes more than one value is found, getting only the first
t3melt$country_code3 <- sapply(as.character(t3melt$country_code),
                               function(x) {
                                 isoToName(x, nameColumn = "ISO_A3")[1]
                                 })
t3melt$region <- countryRegions[match(t3melt$country_code3,
                                      countryRegions$ISO3), 
                                "GEO3major"]
t3melt$year <- ordered(t3melt$year)

t3melt <- ddply(t3melt, .(year, region), numcolwise(sum))

ggplot(t3melt, aes(year, value, group = region, colour = region)) +
  geom_point() + geom_line() + labs(x = "Year", y = "Papers") +
  labs(colour = "Region", title = "Papers by world region")



### Visualization of authors locations versus country contributions: #

## Loading of the data for authors
t2 <- table(authors_locations$year, authors_locations$country_code)
t2melt <- melt(t2, varnames = c("year", "country_code"))
t2melt$country_code3 <- sapply(as.character(t2melt$country_code),
                               function(x) {
                                 isoToName(x, nameColumn = "ISO_A3")[1]
                                 })
t2melt$region <- countryRegions[match(t2melt$country_code3,
                                      countryRegions$ISO3),
                                "GEO3major"]
t2melt$year <- ordered(t2melt$year)
t2melt <- ddply(t2melt, .(year, region), numcolwise(sum))


## Combining the information
t23melt <- rbind(cbind(t2melt, what = "Unique authors"),
                 cbind(t3melt, what = "Papers"))
t23melt$what <- factor(t23melt$what,
                       levels = c("Unique authors", "Papers"))

ggplot(t23melt, aes(year, value, group = region, colour = region)) +
  geom_point() + geom_line() + facet_grid(. ~ what) + labs(x ="Year") +
  labs(y = "Frequency of unique authors and papers", colour = "Region") +
  labs(title = "Comparison unique authors and papers by world region")
