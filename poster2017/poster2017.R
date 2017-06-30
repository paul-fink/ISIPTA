### first run coauthors-network.R!

founding <- as.character(subset(papers_authors, year == 1999, select = "author")[,1])
current <- as.character(subset(papers_authors, year == 2017, select = "author")[,1])


networkseed <- 2015
### big graph 2017 with rownames(vertices), i.e. rownumber as id
set.seed(networkseed)
#coords <- layout_(graph, with_graphopt(charge=0.01)) 
#coords <- layout_components(graph, layout = layout_with_fr)
coords <- layout_with_fr(graph, niter = 2000)
pdf("network2017.pdf", width=20, height=20)
op1 <- par(mar = c(1, 0, 0, 0))
plot(graph,
     vertex.size = 3.5,
     vertex.color = "gray90",
     vertex.label = rownames(vertices),
     vertex.frame.color = c("gray90","blue")[(names(V(graph)) %in% current) + 1],
     vertex.label.color = c("black","red")[(names(V(graph)) %in% founding) + 1],
     edge.color = "SkyBlue2",
     layout = coords)
#    layout = layout.fruchterman.reingold)
legend("topleft",
       legend = sort(unique(edgelist$width)),
       lwd = sort(unique(edgelist$width)),
       col = "SkyBlue2",
       bty = "n")
par(op1)
dev.off()





