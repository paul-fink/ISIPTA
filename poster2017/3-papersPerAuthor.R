### 3. Papers per author

papersperauthor <- table(papers_authors$author)
# frequency table
table(papersperauthor)

pdf("3-papers-per-author.pdf", width = 8, height = 6.5, family = "CM Roman", useDingbats = FALSE)
print(ggplot(melt(table(papersperauthor), varnames = c("npapers")),
             aes(ordered(npapers), value)) + geom_bar(stat="identity") +
        xlab("Papers in ISIPTA") + ylab("Number of authors"))
dev.off()
embed_fonts("3-papers-per-author.pdf")

# sorting authors according to most written papers
names(papersperauthor) <- paste0(names(papersperauthor), " (", seq_len(nlevels(papers_authors$author)),")")
rev(sort(papersperauthor[papersperauthor>8]))


