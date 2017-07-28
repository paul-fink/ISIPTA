### Regular contributors.

library("ISIPTA.eProceedings")
library(ggplot2)
library(plyr)
library(reshape2)

data("authors_locations", package = "ISIPTA.eProceedings")
data("papers", package = "ISIPTA.eProceedings")
data("papers_authors", package = "ISIPTA.eProceedings")


## converts the variable year into an ordered factor
authors_locations$year <- ordered(authors_locations$year)

conferences_contributors <-
  ddply(authors_locations, .(author),
        function(x) {
          data.frame(t(as.matrix(table(x$year))))
        })

colnames(conferences_contributors) <-
  c("author", sub("X", "ISIPTA", colnames(conferences_contributors)[-1]))


authors_ncontributions <-
  data.frame(author = conferences_contributors$author,
             ncontribs = rowSums(conferences_contributors[, -1]))
## these are not number of papers, 
## but number of times the author contributed paper at a ISIPTA!


### Contribution "distribution": #####################################

t5 <- table(authors_ncontributions$ncontribs)
t5


ggplot(melt(t5, varnames = c("ncontribs")),
       aes(ordered(ncontribs), value)) + geom_bar(stat = "identity") +
  labs(x = "Distinct ISIPTA conferences") + 
  labs(y = "Authors") + 
  labs(title = "Authors by contribution to distinct ISIPTA conferences")



### The "regular contributors": ######################################

nconferences <- nlevels(authors_locations$year)

subset(authors_ncontributions, ncontribs == nconferences)
subset(authors_ncontributions, ncontribs >= 6)



### Contributors flow: ###############################################

flow <- 2:nconferences
names(flow) <- paste(colnames(conferences_contributors)[-c(1, nconferences+1)],
                     colnames(conferences_contributors[, -c(1, 2)]), sep = "-")

contributors_flow <- sapply(flow,
                            function(i) {
                              i <- i + 1
                              as.logical(conferences_contributors[, i-1]) &
                              as.logical(conferences_contributors[, i])
                            })



## Number of authors contributing again the following conference:
colSums(contributors_flow)


## ... in relation to the maximum number of contributors to lose:
unique_authors <- as.numeric(daply(papers_authors, .(year),
                                   function(x) {
                                     nlevels(x$author[, drop = TRUE])
                                   }))

max_loss <- sapply(flow,
                   function(i) {
                     min(unique_authors[i-1], unique_authors[i])
                   })

colSums(contributors_flow) / max_loss

