### Authors per paper.

library("ISIPTA.eProceedings")
library(ggplot2)
library(plyr)
library(reshape2)

data("papers_authors", package = "ISIPTA.eProceedings")


papers_nauthors <-
  ddply(papers_authors, .(id),
        function(x) {
          data.frame(year = x$year[1],
                     id = x$id[1],
                     nauthors = nrow(x))
        })

papers_nauthors <- within(papers_nauthors, {
  nauthors <- ordered(nauthors)
  year <- ordered(year)

})


t4 <- table(papers_nauthors$year, papers_nauthors$nauthors)
t4



### Visualization of the frequencies: ################################

## Absolute numbers of authors per paper:
ggplot(papers_nauthors, aes(nauthors, fill = nauthors)) +
  geom_bar() + labs(x = "Authors", y = "Papers") +
  labs(title = "Authors per paper")


## Numbers of authors per paper per year:
ggplot(papers_nauthors, aes(nauthors, fill = year)) + geom_bar() +
  labs(x = "Authors per paper", y = "Papers", fill = "Year") +
  labs(title = "Paper distribution")

ggplot(papers_nauthors, aes(year, fill = nauthors)) + geom_bar() +
  labs(x = "Year", y = "Papers", fill = "Authors per paper") +
  labs(title = "Paper distribution")



### Visualization of the trend: ######################################

t4melt <- melt(t4, varnames = c("year", "nauthors"))
t4melt <- within(t4melt, {
  nauthors <- ordered(nauthors)
  year = ordered(year)
})


## ... grouped by year:
ggplot(t4melt, aes(nauthors, value, group = year, colour = year)) +
  geom_point() + geom_line() + labs(x = "Authors per paper") +
  labs(y = "Papers", colour = "Year") + 
  labs(title = "Development of authors per paper")


## ... grouped by number of authors:
ggplot(t4melt, aes(year, value, group = nauthors, colour = nauthors)) +
  geom_point() + geom_line() + labs(x = "Year", y = "Papers") +
  labs(colour = "Authors per paper") +
  labs(title = "Development of authors per paper")
