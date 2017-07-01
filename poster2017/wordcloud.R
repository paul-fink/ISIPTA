# wordcloud
library(wordcloud)
library(RColorBrewer)

write.table(cbind(unique(sort(papers_keywords$keyword)),unique(sort(papers_keywords$keyword))), "keywords.txt", row.names = FALSE)
papers_keywords$keyword <- as.character(papers_keywords$keyword)
papers_keywords$sid <- seq_len(NROW(papers_keywords))

cleankeywords <- read.table("keywords.txt", col.names = c("keyword","normkeyword"), stringsAsFactors = FALSE)

mergekeywords <- merge(papers_keywords, cleankeywords, by = "keyword", sort = FALSE, all.x =TRUE)
mergekeywords <- mergekeywords[order(mergekeywords$sid),-4]


tabkeywords_early <- table(mergekeywords$normkeyword[mergekeywords$year < 2005])
tabkeywords_late <- table(mergekeywords$normkeyword[mergekeywords$year> 2011])

pale <- brewer.pal(9, "YlOrRd")[-(1:2)]

set.seed(12)
pdf("keywords_cloud_late.pdf")
opar <- par(bg = "black")
wordcloud(names(tabkeywords_late), tabkeywords_late, scale = c(1.9,0.2), min.freq = 1, random.order = FALSE, colors = pale)
par(opar)
dev.off()


set.seed(12)
pdf("keywords_cloud_early.pdf")
opar <- par(bg = "black")
wordcloud(names(tabkeywords_early), tabkeywords_early, scale = c(1.9,0.2), min.freq = 1, random.order = FALSE, colors = pale)
par(opar)
dev.off()


