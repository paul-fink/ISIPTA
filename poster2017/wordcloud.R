# wordcloud
library(wordcloud)

keywords <- papers_keywords$keyword[papers_keywords$year> 2011]

tabkeywords <- table(keywords)
df_kewordcounts  <- data.frame(word = tabkeywords, freq = tabkeywords)

wordcloud(names(tabkeywords), tabkeywords, scale = c(1.9,0.1), min.freq = 1, random.order = TRUE)

library(webshot)
webshot::install_phantomjs()
library(htmlwidgets)


# Make the graph
my_graph = wordcloud2(tabkeywords, size=0.2)

# save it in html

saveWidget(my_graph,"tmp.html",selfcontained = F)

# and in pdf
webshot("tmp.html","fig_1.pdf", delay =10, vwidth = 900, vheight=900, zoom=0.5)#, zoom=10)
