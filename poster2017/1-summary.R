### 1. summary statistics of authors, papers

t1new <- t1
names(t1new) <- c("year", "Papers", "Paper authors", "Unique authors")
t1melt <- melt(t1new, id = "year")
t1melt$year <- ordered(t1melt$year)

pdf("1-summary.pdf", width = 6, height = 6, family = "CM Roman", useDingbats = FALSE)
print(ggplot(t1melt, aes(year, value, group = variable, colour = variable)) +
  geom_point() + geom_line() + bottomlegend + xlab("Year") + ylab("Number of ..."))
dev.off()
embed_fonts("1-summary.pdf")
