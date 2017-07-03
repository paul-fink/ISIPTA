### 2. Author flows
# Normalize with respect to the number of unique authors per year
authorflow_percent <- data.frame(year = authorflow$year, lapply(authorflow[,-(1:2)], function(x) {x/authorflow$unique*100}), check.names = FALSE)

# average of new contributors
mean(authorflow_percent$'New', na.rm = TRUE)
# average of contributors of last conference
mean(authorflow_percent$'1-step', na.rm = TRUE)


authorflowmelt <- melt(authorflow_percent, id = "year")
authorflowmelt$year <- ordered(authorflowmelt$year)
authorflowmelt <- subset(authorflowmelt, !is.na(value))

pdf("2-authorflow.pdf", width = 6, height = 6, family = "CM Roman", useDingbats = FALSE)
print(ggplot(authorflowmelt, aes(year, value, group = variable, colour = variable)) +
  geom_point() + geom_line() + bottomlegend + xlab("Year") + ylab("Percentage of recurring authors") + ylim(0,100))
dev.off() 
embed_fonts("2-authorflow.pdf")
