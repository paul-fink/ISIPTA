### Simple summary numbers by years.

library("ISIPTA.eProceedings")
library(ggplot2)
library(plyr)
library(reshape2)

data("papers", package = "ISIPTA.eProceedings")
data("papers_authors", package = "ISIPTA.eProceedings")


t1 <- data.frame(
  year = sort(unique(papers$year)),
  ## Number of papers:
  "Papers" = as.numeric(table(papers$year)),
  ## Number of paper authors:
  "Paper authors" = as.numeric(table(papers_authors$year)),
  ## Number of unique authors:
  "Unique authors" = as.numeric(daply(papers_authors, .(year),
                                      function(x) {
                                        nlevels(x$author[, drop = TRUE])
                                      })),
  check.names = FALSE
)
t1



### Visualization by year: ###########################################

t1melt <- melt(t1, id = "year")
t1melt$year <- ordered(t1melt$year)

ggplot(t1melt, aes(year, value, group = variable, colour = variable)) +
  geom_point() + geom_line() + labs(x = "Year", colour = "") + 
  labs(y = "Frequency of ...", title = "Summary")
