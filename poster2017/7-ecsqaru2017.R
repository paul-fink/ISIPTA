### 7. Current network of 2017 including all ISIPTA and ECSQARU authors

#papesrs for ISIPTA
papers_authors2017 <- subset(papers_authors, year == 2017, select=c("id", "author"))
papers_authors2017$type <- "ISIPTA"

combinedpapers <- rbind(papers_authors2017, authors_ecsqaru)
#combinedpapers$author <- factor(combinedpapers$author)
  
combined_pairs_2017 <- ddply(combinedpapers, .(id), function(x) {
  if ( nrow(x) > 1 ) {
    authors <- sort(as.character(x$author))
    pairs <- combn(authors, 2)
      
    data.frame(author1 = factor(pairs[1, ], levels = levels(combinedpapers$author)),
               author2 = factor(pairs[2, ], levels = levels(combinedpapers$author)),
               type = unique(x$type),
               id = x$id[1])
    }
})

combined_npairs_2017 <- ddply(combined_pairs_2017, .(author1, author2),
                          function(x) {
                            data.frame(npairs = nrow(x),
                                       type = paste(unique(x$type),collapse = ",")
                            )
                          })
edgelist2017 <- within(combined_npairs_2017, {
  width <- npairs
  npairs <- NULL
})

# only relevant names
usedlvls <- levels(combinedpapers$author)[levels(combinedpapers$author) %in% droplevels(combinedpapers$author)]

# type=1:ISIPTA; type=2:ECSQARU; type=3:both
vertices2017 <- data.frame(name = usedlvls, label = sort(unique(as.numeric(combinedpapers$author))), type = usedlvls %in% (as.character(subset(combinedpapers, type=="ISIPTA")$author)) +
                             2*(usedlvls %in% (as.character(subset(combinedpapers, type=="ECSQARU")$author))))
graph2017 <- graph.data.frame(edgelist2017,
                              directed = FALSE,
                              vertices = vertices2017)
set.seed(201710)
coords2017 <- layout_with_fr(graph2017, niter = 2000)
pdf("7-networkECSQARU2017.pdf", width = 12, height = 12, family = "CM Roman", useDingbats = FALSE)
op1 <- par(mar = c(1, 0, 0, 0))
plot(graph2017,
     vertex.size = 3.5,
     vertex.color = c(NA, palette11[8])[(vertices2017$type!=2)+1],
     vertex.label = V(graph2017)$label,
     vertex.label.cex = 0.5,
     vertex.label.family = "CM Roman",
     vertex.frame.color = c(palette11[8], palette11[11])[(vertices2017$type!=1)+1],
     vertex.label.color = c("black", palette6[1])[(names(V(graph2017)) %in% founding) + 1],
     edge.color = palette11[3],
     layout = coords2017)

dev.off()
par(op1)
embed_fonts("7-networkECSQARU2017.pdf")
