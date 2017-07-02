### 6. ISIPTA network development from 1999 to 2017 including all ISIPTA authors

## common network seed and coordinates
networkseed <- 2017
set.seed(networkseed)
coords <- layout_with_fr(graph, niter = 2000)

### Big network of year 2017, highlighting also ECSQARU contribution
pdf("6-network2017-big.pdf", width = 18, height = 18)
op1 <- par(mar = c(1, 0, 0, 0))
plot(graph,
     vertex.size = 3.5,
     vertex.color = c("gray90", palette11[8])[(names(V(graph)) %in% current) + 1],
     vertex.label = rownames(vertices),
     vertex.frame.color = c("gray90", palette11[8], palette11[11], palette11[11])[(names(V(graph)) %in% current) + 2 * (names(V(graph)) %in% ecsqaru) + 1],
     vertex.label.color = c("black", palette6[1])[(names(V(graph)) %in% founding) + 1],
     edge.color = palette11[3],
     layout = coords)

## legend 
points(-1, -1.01, pch = 21, bg = palette11[8], 
       col = palette11[8], cex = 1.8)
text(-0.96, -1.01,"current ISIPTA contribution", adj = c(0,0.5), cex = 1)

points(-1, -1.04, col = palette11[11], cex = 1.8, lwd = 2)
text(-0.96, -1.04, "current ECSQARU contribution", adj = c(0,0.5), cex = 1)

points(-1, -1.07, pch = 21, bg = "gray90", col = "gray90", cex = 1.8)
text(-0.96, -1.07, "previous ISIPTA contribution", adj = c(0,0.5), cex = 1)

text(-1, -1.1, "1", col = palette6[1], adj = c(0.5,0.5), cex = 1)
text(-0.96, -1.1, "ISIPTA founding member", adj = c(0,0.5), cex = 1)
par(op1)
dev.off()


### Network of years 1999-2017 in small
for ( i in years ) {
  cyear <- as.integer(gsub("[A-Z]*(\\d{4})", "\\1", i))
  current <- as.character(subset(papers_authors, year == cyear, select = "author")[,1])
  ewidth <- coauthors_years[[i]]
  ecolor <- ifelse(coauthors_years[[i]] > 0, palette11[3], NA)
  vcolor <- ifelse(authors_years[[i]] > 0, c("gray95",palette11[9])[(names(V(graph)) %in% current) + 1], NA)
  fcolor <- ifelse(authors_years[[i]] > 0, c("black", palette6[1])[(names(V(graph)) %in% founding) + 1], NA)
  
  pdf(paste0("6-network", cyear, ".pdf"), width = 6, height = 6)
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
  
  par(op1)
  dev.off()
}

### network statistics
## diameter
diameter(graph, directed = FALSE)
# diameter path
get_diameter(graph, directed = FALSE)
as.vector(get_diameter(graph, directed = FALSE))

## neighbourhoods, i.e. number of co-authors
unique_coauthors <- data.frame(name = names(V(graph)), ncoauthors = ego_size(graph, order=1)-1)
coauthorsdist <- table(unique_coauthors$ncoauthors)

pdf("6-network-coauthors.pdf", width = 6, height = 6)
ggplot(melt(coauthorsdist, varnames = c("ncoauthors")),
       aes(ordered(ncoauthors), value)) + geom_bar(stat="identity") +
  xlab("... unique coauthors") + ylab("Number of authors with ...")
dev.off()

## authors with more than 11 coauthors
subset(unique_coauthors[order(unique_coauthors$ncoauthors),], ncoauthors >= 12)
