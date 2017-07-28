### Coauthors network; i.e., authors as vertices and two vertices
### joined by an edge if the two authors have written a joint paper.

library("ISIPTA.eProceedings")
library(ggplot2)
library(igraph)
library(plyr)
library(reshape2)

data("authors_locations", package = "ISIPTA.eProceedings")
data("papers_authors", package = "ISIPTA.eProceedings")

### Calculate the regularity of contributons per author ##############
## For detailed look, see demo("regular-contributors")
authors_locations$year <- ordered(authors_locations$year)

conferences_contributors <-
  ddply(authors_locations, .(author),
        function(x) {
          data.frame(t(as.matrix(table(x$year))))
        })

colnames(conferences_contributors) <-
  c("author", sub("X", "ISIPTA", colnames(conferences_contributors)[-1]))


authors_ncontributions <-
  data.frame(author = conferences_contributors$author,
             ncontribs = rowSums(conferences_contributors[, -1]))


### Coauthor pairs calculation: ######################################
coauthors_pairs <- ddply(papers_authors, .(id),
                         function(x) {
                           if ( nrow(x) > 1 ) {
                             authors <- sort(as.character(x$author))
                             pairs <- combn(authors, 2)

                             data.frame(author1 =
                                        factor(pairs[1, ],
                                               levels = levels(x$author)),

                                        author2 =
                                        factor(pairs[2, ],
                                               levels = levels(x$author)),

                                        year = x$year[1],
                                        id = x$id[1])
                           }
                         })

coauthors_pairs <- within(coauthors_pairs, {
  year <- ordered(year)
  id <- factor(id)
})



## Reduce to the number of each pair:
coauthors_npairs <- ddply(coauthors_pairs, .(author1, author2),
                          function(x) {
                            c(npairs = nrow(x))
                          })



### Overall collaboration graph: #####################################

## Edgelist; width of the edge is the number of joint papers:
edgelist <- within(coauthors_npairs, {
  width <- npairs
  npairs <- NULL
})



## Vertices:
vertices <- data.frame(name = levels(edgelist$author1))



## Graph:
graph <- graph.data.frame(edgelist,
                          directed = FALSE,
                          vertices = vertices)



### Visualization of the graph: ######################################

set.seed(1234)
coords <- layout_with_fr(graph = graph, niter = 2000)
plot(graph,
     vertex.size = 5,
     vertex.color = "gray90",
     vertex.label = rownames(vertices),
     vertex.frame.color = "gray90",
     vertex.label.color = "black",
     edge.color = "SkyBlue2",
     layout = coords,
     main = sprintf("Complete collaboration network (%s)",
                    max(levels(coauthors_pairs$year))))

legend("topleft",
       legend = sort(unique(edgelist$width)),
       lwd = sort(unique(edgelist$width)),
       col = "SkyBlue2",
       bty = "n")



### Average path length, i.e., the degrees of separation: ############

average.path.length(graph)



### The longest shortest path, i.e., the diameter: ###################

diameter(graph)

V(graph)[get.diameter(graph)]



### Distance distributions: ##########################################

distances <- shortest.paths(graph)
dimnames(distances) <- list(V(graph)$name, V(graph)$name)



### Personal distributions of the "regular contributors":
regulars <- subset(authors_ncontributions,
                   ncontribs == nlevels(coauthors_pairs$year))$author

regulars_dists <-
  distances[, match(regulars, colnames(distances)), drop = FALSE]

regulars_dists_melt <- melt(regulars_dists)
regulars_dists_melt <- regulars_dists_melt[regulars_dists_melt$value < Inf,]
regulars_dists_melt <- regulars_dists_melt[regulars_dists_melt$value > 0,]

## Distribution of shortest distances for regular contributors
table(regulars_dists_melt$value, regulars_dists_melt$Var2)

ggplot(melt(regulars_dists_melt, id.vars = c("Var1", "Var2")),
       aes(ordered(value))) + geom_bar(fill = "SkyBlue2") +
  facet_grid(. ~ Var2) + labs(x = "Distances") + 
  labs(title = "Distribution of distances of persons with paper at all ISIPTAs")
# Barbara Vantaggi is apparently not part of the largest cluster in the graph,
# otherwise she would have higher distances


### Evolution of the network over time: ##############################

## Vertices, i.e., coauthors, by years:
coauthors_years <- ddply(coauthors_pairs, .(author1, author2),
                         function(x) {
                           as.data.frame(t(as.matrix(table(x$year))))
                         })

colnames(coauthors_years) <- c("author1", "author2",
                               sprintf("ISIPTA%s",
                                       levels(coauthors_pairs$year)))

coauthors_years <- cbind(coauthors_years[, 1:2],
                         t(apply(coauthors_years[, -c(1:2)], 1, cumsum)))


## Edges, i.e., authors, by years:
authors_years <- cbind(conferences_contributors[, 1, drop = FALSE],
                       t(apply(conferences_contributors[, -c(1)], 1, cumsum)))


### Graphs over time:

years <- levels(coauthors_pairs$year)
years <- sapply(years, grep,
                colnames(coauthors_years), value = TRUE)


op <- par(mfrow = c(2, ceiling(length(years)/2)))

for ( i in years ) {

  ewidth <- coauthors_years[[i]]
  ecolor <- ifelse(coauthors_years[[i]] > 0, "SkyBlue2", NA)
  vcolor <- ifelse(authors_years[[i]] > 0, "black", NA)
  fcolor <- ifelse(authors_years[[i]] > 0, "black", NA)

  op1 <- par(mar = c(1, 0, 0, 0))
  set.seed(1234)
  plot(graph,
       vertex.size = 3,
       vertex.label = NA,
       vertex.color = vcolor,
       vertex.frame.color = fcolor,
       edge.color = ecolor,
       edge.width = ewidth,
       layout = layout.fruchterman.reingold)

  mtext(i, side = 1, line = 0)
  par(op1)
}
par(op)
