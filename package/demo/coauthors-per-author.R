### Coauthors per author. See demo("coauthor-network") for a detailed
## analysis on the unique pairs of coauthors.

library("ISIPTA.eProceedings")
library(ggplot2)
library(plyr)

papers_ncoauthors <- ddply(papers_authors, .(id),
                          function(x) {
                            data.frame(year = x$year,
                                       id = x$id,
                                       author = x$author,
                                       ncoauthors = nrow(x) - 1)
                          })

papers_ncoauthors <- within(papers_ncoauthors, {
  year <- ordered(year)
  id <- factor(id)
})


### Numbers are proportional to authors per papers, or? ##############

with(papers_ncoauthors, table(year, ncoauthors))


ggplot(papers_ncoauthors, aes(factor(ncoauthors))) + geom_bar() +
  labs(x = "Coauthors", y = "Authors") +
  labs(title = "Frequency of Coauthorship")
ggplot(papers_ncoauthors, aes(factor(ncoauthors), fill = factor(year))) +
  geom_bar() + labs(x = "Coauthors", y = "Authors") +
  labs(title = "Frequency of Coauthorship", fill = "Year")
ggplot(papers_ncoauthors, aes(factor(year), fill = factor(ncoauthors))) +
  geom_bar() + labs(x = "Year", y = "Authors") +
  labs(title = "Frequency of Coauthorship", fill = "Coauthors")


### Overall distribtion: #############################################
papers_ncoauthors_overall <-
  ddply(papers_ncoauthors, .(author), numcolwise(sum))


### Distribution by year: ############################################

ggplot(papers_ncoauthors, aes(ordered(ncoauthors))) +
    geom_bar() + facet_grid(year ~ .) + labs(x = "Coauthors") +
  labs(y = "Authors", title = "Frequency of Coauthorship")

ggplot(papers_ncoauthors, aes(ordered(ncoauthors))) +
  geom_bar() + facet_grid(. ~ year) + labs(x = "Coauthors") +
  labs(y = "Authors", title = "Frequency of Coauthorship")
