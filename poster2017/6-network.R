### 6. ISIPTA network development from 1999 to 2017 including all ISIPTA authors

## common network seed and coordinates
networkseed <- 2017
set.seed(networkseed)
coords <- layout_with_fr(graph, niter = 2000)

### Big network of year 2017, highlighting also ECSQARU contribution
pdf("6-network2017-big.pdf", width = 18, height = 18, family = "CM Roman", useDingbats = FALSE)
op1 <- par(mar = c(1, 0, 0, 0))
plot(graph,
     vertex.size = 3.5,
     vertex.color = c("gray90", palette11[8])[(names(V(graph)) %in% current) + 1],
     vertex.label = rownames(vertices),
     vertex.label.family = "CM Roman",
     vertex.frame.color = c("gray90", palette11[8], palette11[11], palette11[11])[(names(V(graph)) %in% current) + 2 * (names(V(graph)) %in% ecsqaru) + 1],
     vertex.label.color = c("black", palette6[1])[(names(V(graph)) %in% founding) + 1],
     edge.color = palette11[3],
     layout = coords)

## legend 
points(-1, -0.96, pch = 21, bg = palette11[8], 
       col = palette11[8], cex = 1.8)
text(-0.96, -0.96,"current ISIPTA paper contributor", adj = c(0,0.5), cex = 1)

points(-1, -0.99, col = palette11[11], cex = 1.8, lwd = 2)
text(-0.96, -0.99, "current ECSQARU paper contributor (only for 2017)", adj = c(0,0.5), cex = 1)

points(-1, -1.02, pch = 21, bg = "gray90", col = "gray90", cex = 1.8)
text(-0.96, -1.02, "previous ISIPTA paper contributor", adj = c(0,0.5), cex = 1)

points(-1, -1.05, col = palette6[1], cex = 2.5, lwd = 1.5)
text(-1, -1.05, "1", col = palette6[1], adj = c(0.5,0.5), cex = 1)
text(-0.96, -1.05, "Paper contributor of first ISIPTA", adj = c(0,0.5), cex = 1)

text(-0.85, 0.85, "2017", adj = c(0.5,0.5), cex = 3, font = 2)

par(op1)
dev.off()
embed_fonts("6-network2017-big.pdf")

### Network of years 1999-2017 in small
for ( i in years ) {
  cyear <- as.integer(gsub("[A-Z]*(\\d{4})", "\\1", i))
  current <- as.character(subset(papers_authors, year == cyear, select = "author")[,1])
  ewidth <- coauthors_years[[i]]
  ecolor <- ifelse(coauthors_years[[i]] > 0, palette11[3], NA)
  vcolor <- ifelse(authors_years[[i]] > 0, c("gray95",palette11[9])[(names(V(graph)) %in% current) + 1], NA)
  fcolor <- ifelse(authors_years[[i]] > 0, c("black", palette6[1])[(names(V(graph)) %in% founding) + 1], NA)
  
  filename <- paste0("6-network", cyear, ".pdf")
  pdf(filename, width = 6, height = 6, family = "CM Roman", useDingbats = FALSE)
  op1 <- par(mar = c(1, 0, 0, 0))
  set.seed(networkseed)
  plot(graph,
       vertex.size = 3,
       vertex.label = NA,
       vertex.label.cex = 1.25,
       vertex.color = vcolor,
       vertex.frame.color = fcolor,
       edge.color = ecolor,
       edge.width = ewidth/2,
       layout = coords)
  text(-0.96, -1, cyear, adj = c(0,0.5), cex = 2, font = 2)
  par(op1)
  dev.off()
  embed_fonts(filename)
}

### network statistics
## diameter
diameter(graph, directed = FALSE)
# diameter path
paste0(names(get_diameter(graph, directed = FALSE)), " (", as.vector(get_diameter(graph, directed = FALSE)),")")

# degree
subgraphlist <- decompose(graph)
lapply(subgraphlist, function(x) length(V(x)))
maxnodegraph <- subgraphlist[[which.max(sapply(subgraphlist, function(x) length(V(x))))]]
mean(degree(maxnodegraph))

## neighbourhoods, i.e. number of co-authors
unique_coauthors <- data.frame(name = names(V(graph)), ncoauthors = ego_size(graph, order=1)-1)
coauthorsdist <- table(unique_coauthors$ncoauthors)

pdf("6-network-coauthors.pdf", width = 6, height = 5, family = "CM Roman", useDingbats = FALSE)
print(ggplot(melt(coauthorsdist, varnames = c("ncoauthors")),
       aes(ordered(ncoauthors), value)) + geom_bar(stat="identity") +
  xlab("... unique coauthors") + ylab("Number of authors with ..."))
dev.off()
embed_fonts("6-network-coauthors.pdf")

## authors with more than 11 coauthors
subset(unique_coauthors[order(unique_coauthors$ncoauthors),], ncoauthors >= 12)
