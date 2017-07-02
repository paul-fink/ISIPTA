### 3. Papers per author

papersperauthor <- table(papers_authors$author)
# frequency table
table(papersperauthor)

# sorting authors according to most written papers
names(papersperauthor) <- paste0(names(papersperauthor), " (", seq_len(nlevels(papers_authors$author)),")")
rev(sort(papersperauthor[papersperauthor>10]))



