### 5. wordclouds of Keywords

## to manually clean the keywords and normalize
#write.table(cbind(unique(sort(papers_keywords$keyword)),unique(sort(papers_keywords$keyword))), "keywords.txt", row.names = FALSE)
papers_keywords$keyword <- as.character(papers_keywords$keyword)
papers_keywords$sid <- seq_len(NROW(papers_keywords))

cleankeywords <- read.table("keywords.txt", col.names = c("keyword","normkeyword"), stringsAsFactors = FALSE)

mergekeywords <- merge(papers_keywords, cleankeywords, by = "keyword", sort = FALSE, all.x =TRUE)
mergekeywords <- mergekeywords[order(mergekeywords$sid),-4]

# distirubtion of keywords in last 3 and first 3 ISIPTAs
tabkeywords_early <- table(mergekeywords$normkeyword[mergekeywords$year < 2005])
tabkeywords_late <- table(mergekeywords$normkeyword[mergekeywords$year> 2011])


set.seed(12)
pdf("5-keywords-last3.pdf", width = 10, height = 10, family = "CM Roman", useDingbats = FALSE)
opar <- par(bg = "grey10")
wordcloud(names(tabkeywords_late), tabkeywords_late,
          scale = c(4.8,1), min.freq = 2, 
          random.order = FALSE, colors = palette7wc, font = 2)
par(opar)
dev.off()
embed_fonts("5-keywords-last3.pdf")

set.seed(12)
pdf("5-keywords-first3.pdf", width = 10, height = 10, family = "CM Roman", useDingbats = FALSE)
opar <- par(bg = "grey10")
wordcloud(names(tabkeywords_early), tabkeywords_early,
          scale = c(4.8,1), min.freq = 2,
          random.order = FALSE, colors = palette7wc, font = 2)
par(opar)
dev.off()
embed_fonts("5-keywords-first3.pdf")

length(tabkeywords_early)
length(tabkeywords_early)/sum(t1$papers[1:3])
rev(sort(tabkeywords_early))[1:5]

length(tabkeywords_late)
length(tabkeywords_late)/sum(rev(t1$papers)[1:3])
rev(sort(tabkeywords_late))[1:5]
