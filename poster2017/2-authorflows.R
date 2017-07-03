### 2. Author flows

# Normalize with respect to the number of unique authors per year
authorflowmelt <- melt(data.frame(year = authorflow$year, lapply(authorflow[,-(1:2)], function(x) {x/authorflow$unique*100}), check.names = FALSE), id = "year")
authorflowmelt$year <- ordered(authorflowmelt$year)
authorflowmelt <- subset(authorflowmelt, !is.na(value))

pdf("2-authorflow.pdf", width = 6, height = 6, family = "CM Roman", useDingbats = FALSE)
print(ggplot(authorflowmelt, aes(year, value, group = variable, colour = variable)) +
  geom_point() + geom_line() + bottomlegend + xlab("Year") + ylab("Percentage of recurring authors") + ylim(0,100))
dev.off() 
embed_fonts("2-authorflow.pdf")
