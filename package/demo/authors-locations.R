### Number of unqiue authors by year and country.

library("ISIPTA.eProceedings")
library(colorspace)
library(ggplot2)
library(plyr)
library(reshape2)
library(rworldmap)

data("authors_locations", package = "ISIPTA.eProceedings")

#follow an author's movements
authors_locations[authors_locations$author == "Sebastien Destercke",]

t2 <- table(authors_locations$year, authors_locations$country_code)
t2



### Absolute numbers on the world map: ###############################

t2absolut <- data.frame(country_code = colnames(t2),
                        value = colSums(t2))

t2map <- joinCountryData2Map(t2absolut, joinCode = "ISO2",
                             nameJoinColumn = "country_code")

values <- seq(floor(min(t2absolut$value)), 
              ceiling(max(t2absolut$value)))
pal <- rev(sequential_hcl(length(values)-1, power = 1.2))


## World:
mapCountryData(t2map, nameColumnToPlot = "value",
               colourPalette = pal,
               catMethod = values,
               mapTitle = "Absolute frequency of contributors",
               addLegend = TRUE,
               oceanCol = gray(0.95),
               missingCountryCol = "white")

## Europe:
mapCountryData(t2map, nameColumnToPlot = "value",
               mapRegion = "europe",
               colourPalette = pal,
               catMethod = values,
               mapTitle = "Absolute frequency of contributors from Europe",
               addLegend = TRUE,
               oceanCol = gray(0.95),
               missingCountryCol = "white")



### Visualization by region and year: ################################

data("countryRegions", package = "rworldmap")

t2melt <- melt(t2, varnames = c("year", "country_code"))
# countryRegions now has only ISO3 country codes,
# so we must convert the two-letter ISO2 to the three-letter ISO3 code
# isoToName() does not like character vectors.
# Sometimes more than one value is found, getting only the first
t2melt$country_code3 <- sapply(as.character(t2melt$country_code),
                               function(x) {isoToName(x, nameColumn = "ISO_A3")[1]})
t2melt$region <- countryRegions[match(t2melt$country_code3,
                                      countryRegions$ISO3), "GEO3major"]
t2melt$year <- ordered(t2melt$year)

t2melt <- ddply(t2melt, .(year, region), numcolwise(sum))


ggplot(t2melt, aes(year, value, group = region, colour = region)) +
  geom_point() + geom_line() + labs(x = "Year", y = "Unique authors") +
  labs(colour = "Region", title = "Unique authors by region and year")

