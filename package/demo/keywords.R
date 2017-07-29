### Keywords.

library("ISIPTA.eProceedings")
library(plyr)
library(reshape2)
library(ggplot2)
library(wordcloud)

data("papers_keywords", package = "ISIPTA.eProceedings")


### Frequency of keywords ############################################
tabkeywords <- table(papers_keywords$keyword)[-1]
tabkeywords["NA"] <- 0
tabkeywords <- sort(tabkeywords, decreasing = TRUE)

## Overall
tabkeywords[tabkeywords > 1]

## number of keywords per paper
kwp <- ddply(papers_keywords, .(year,id), function(x) nrow(x))

## over all proceedings
ggplot(kwp, aes(V1)) + geom_bar(stat = "count") +
  labs(x = "Keywords per paper", y = "Absolute frequency") +
  labs(title = "Keywords per paper")

## over all proceedings per year
ggplot(kwp, aes(V1, fill = ordered(year))) +
  geom_bar(stat = "count", position = position_stack(reverse = TRUE)) +
  labs(fill = "Year", x = "Keywords per paper") +
  labs(y = "Absolute frequency", title = "Keywords per paper")


kwy <- ddply(papers_keywords, .(year), 
             function(x) {
               c(unique_keywords = length(unique(x$keyword)),
                 keywords = nrow(x),
                 papers = length(unique(x$id)))
               }
)
kwy$uniqueKeywordsPerPaper <- kwy$unique_keywords/kwy$papers
kwy$KeywordsPerPaper <- kwy$keywords/kwy$papers

## Absolute frequency of (unique) keywords per paper
kwy_melt1 <- melt(kwy, id.vars = "year", 
                  measure.vars = c("unique_keywords",
                                   "keywords"))
levels(kwy_melt1$variable) <- c("unique keywords", "keywords")

ggplot(kwy_melt1, 
       aes(ordered(year), value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  labs(x = "Year", y = "Frequency", fill = "") +
  labs(title = "Unique keywords")

## Average of (unique) keywords per paper
kwy_melt2 <- melt(kwy, id.vars = "year", 
                  measure.vars = c("uniqueKeywordsPerPaper",
                                   "KeywordsPerPaper"))
levels(kwy_melt2$variable) <- c("unique keywords", "keywords")

ggplot(kwy_melt2,  aes(year, value, colour = variable)) + 
  geom_point() + geom_line() + labs(x = "Year") +
  labs(y = "Proportion", colour = "") +
  labs(title = "(Unique) Keywords per paper") +
  scale_x_continuous(breaks = unique(kwy_melt2$year))


### Most frequent keywords development over time
papers_keywords_fac <- papers_keywords
papers_keywords_fac$keyword <- factor(papers_keywords_fac$keyword)

d1 <- ddply(papers_keywords_fac, .(year),
      function(x) {
        xtabs <- table(x$keyword)/length(unique(x$id))
        xtabs
      })
d1$'NA' <- d1$V1 <- NULL

ggplot(melt(d1[, sapply(d1, function(x) {max(x)>0.1})], 
            id.vars = "year"), aes(year, value, colour = variable)) +
  geom_point() + geom_line() + labs(x = "Year") +
  labs(y = "Frequency in conference papers", colour = "Keyword") +
  labs(title = "Most frequent keywords (at least once >10% at any conference)") +
  scale_y_continuous(labels = function(x) {paste0(x*100, "%")}) +
  scale_x_continuous(breaks = unique(d1$year))


### Wordcloud ########################################################

names(tabkeywords) <- gsub("(imprecise) (probability)", "\\1\n\\2" , names(tabkeywords))

# Printing keywords of at least 3 occurences
opar <- par(bg = "grey10")
wordcloud(freq = tabkeywords, words = names(tabkeywords),
          scale = c(2.5,0.5), min.freq = 3, 
          random.order = FALSE, colors = brewer.pal(9, "YlOrRd")[-(1:2)])
par(opar)
