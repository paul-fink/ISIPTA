### 4. Authors per paper

t4newmelt <- melt(t4, varnames = c("year", "Authors"))
t4newmelt <- within(t4newmelt, {
  Authors <- ordered(Authors)
  year = ordered(year)
})

pdf("4-authors-per-paper.pdf", width = 6, height = 6, family = "CM Roman", useDingbats = FALSE)
print(ggplot(t4newmelt, aes(year, value, group = Authors, colour = Authors)) +
  geom_point() + geom_line() + xlab("Year") + ylab("Number of papers with ...") +
  theme(legend.position = 'bottom', legend.direction = 'horizontal'))
dev.off()
embed_fonts("4-authors-per-paper.pdf")
