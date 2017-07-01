library(xml2)

normalize_author_name <- function(name) {
  isequal<- function(ref_name, name, dist = 0.3) {
    length(agrep(ref_name, name, max.distance = dist)) == 1
  }
  
  if ( isequal("Serafin Moral", name) )
    return("Serafin Moral")
  if ( isequal("Joaquin Abellan", name, 0.4) )
    return("Joaquin Abellan")
  if ( isequal("Damjan Skulj", name) )
    return("Damjan Skulj")
  if ( isequal("Peter Harremoes", name) )
    return("Peter Harremoes")
  if ( isequal("Frederic Pichon", name) )
    return("Frederic Pichon")
  if ( isequal("Christofer Waldenstrom", name) )
    return("Christofer Waldenstrom")
  if ( isequal("Andres Cano", name) )
    return("Andres Cano")
  if ( isequal("Manuel Gomez", name) )
    return("Manuel Gomez")
  if ( isequal("Volker Kraetschmer", name) )
    return("Volker Kraetschmer")
  if ( isequal("Cedric Baudrit", name) )
    return("Cedric Baudrit")
  if ( isequal("Raphael Giraud", name) )
    return("Raphael Giraud")
  if ( isequal("Vladislav Bina", name) )
    return("Vladislav Bina")
  if ( isequal("Sebastian Maass", name) )
    return("Sebastian Maass")
  if ( isequal("Michele Cohen", name) )
    return("Michele Cohen")
  if ( isequal("Ines Couso", name) )
    return("Ines Couso")
  if ( isequal("Jioina Vejnarova", name) )
    return("Jirina Vejnarova")
  if ( isequal("Jacinto Martin", name) )
    return("Jacinto Martin")
  if ( isequal("Javier Hernandez", name) )
    return("Javier Hernandez")
  if ( isequal("Jose Pablo Arias", name) )
    return("Jose Pablo Arias")
  if ( isequal("Antonio Salmeron", name) )
    return("Antonio Salmeron")
  if ( isequal("Marcus Poggi de Aragao", name) )
    return("Marcus Poggi de Aragao")
  if ( isequal("Michele Vanmaele", name) )
    return("Michele Vanmaele")
  if ( isequal("Andres R. Masegosa", name) )
    return("Andres R. Masegosa")
  if ( isequal("Helmut Kuechenhoff", name) )
    return("Helmut Kuechenhoff")
  if ( isequal("Christophe Berenguer", name) )
    return("Christophe Berenguer")
  if ( isequal("Denis Maua", name) )
    return("Denis Maua")
  if ( isequal("Sebastien Destercke", name) )
    return("Sebastien Destercke")
  if ( isequal("Luciano Sanchez", name) )
    return("Luciano Sanchez")
  if ( isequal("Fabio Gagliardi Cozman", name) )
    return("Fabio Cozman")
  if ( isequal("Jasper De Bock", name) )
    return("Jasper de Bock")
  if ( isequal("Lev V. Utkin", name) )
    return("Lev Utkin")
  if ( isequal("Arthur Van Camp", name) )
    return("Arthur van Camp")
  if (isequal("Nicolaj Sondberg-Jeppesen", name) )
    return("Nicolaj Sondberg-Jeppesen")
  if( isequal("Regis Sabbadin", name) )
    return ("Regis Sabbadin")
  if( isequal("Eric Lefevre", name) )
    return ("Eric Lefevre")
  if( isequal("Matti Jaervisalo", name) )
    return ("Matti Jaervisalo")
  if( isequal("Vaclav Kratochvil", name) )
    return ("Vaclav Kratochvil")
  name
}

# papers for ECSQARU
divs <- xml_find_all(read_html("http://www2.idsia.ch/cms/isipta-ecsqaru/accepted-papers-ecsqaru/"), ".//div[@class='omsc-toggle-title']")
authorslist <- sapply(strsplit(gsub(" and ", ", ", gsub("(.*)\\..+","\\1",xml_text(divs)), fixed = TRUE), ", "), function(x) {sapply(x, normalize_author_name)})
authorslist_df <- lapply(seq_along(authorslist), function(x) {
  data.frame(id=as.integer(x), author=authorslist[[x]], type="ECSQARU", stringsAsFactors = FALSE)
})
authors_ecsqaru <- do.call("rbind", authorslist_df)

#papesrs for ISIPTA
papers_authors2017 <- subset(papers_authors, year == 2017, select=c("id", "author"))
papers_authors2017$type <- "ISIPTA"

combinedpapers <- rbind(authors_ecsqaru, papers_authors2017)
combinedpapers$author <- factor(combinedpapers$author)
  
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
#type=1:ISIPTA; type=2:ECSQARU; type=3:both
vertices2017 <- data.frame(name = levels(combinedpapers$author), type = levels(combinedpapers$author) %in% (as.character(subset(combinedpapers, type=="ISIPTA")$author)) +
                             2*(levels(combinedpapers$author) %in% (as.character(subset(combinedpapers, type=="ECSQARU")$author))))
graph2017 <- graph.data.frame(edgelist2017,
                              directed = FALSE,
                              vertices = vertices2017)
coords2017 <- layout_with_fr(graph2017, niter = 2000)

pdf("2017-network.pdf", width=7, height=7)
op1 <- par(mar = c(1, 0, 0, 0))
plot(graph2017,
     vertex.size = 5,
     vertex.color = c("gray90", brewer.pal(11, "RdYlBu")[8])[(vertices2017$type!=2)+1],
     vertex.label = rownames(vertices2017),
     vertex.label.cex = 0.5,
     vertex.frame.color = c("gray90", brewer.pal(11, "RdYlBu")[11])[(vertices2017$type!=1)+1],
     vertex.label.color = c("black",brewer.pal(6, "RdYlBu")[1])[(names(V(graph2017)) %in% founding) + 1],
     edge.color = brewer.pal(11, "RdYlBu")[3],
     layout = coords2017)

text(-1,1, "8", col=brewer.pal(6, "RdYlBu")[1], cex = 0.5)
text(-0.95,1, "ISIPTA founding member", adj = c(0,0.5), cex = 0.5)

points(-1,0.95, pch = 21, bg = brewer.pal(11, "RdYlBu")[8], 
       col = brewer.pal(11, "RdYlBu")[8], cex = 1.1)
text(-0.95,0.95,"ISIPTA", adj = c(0,0.5), cex = 0.5)

points(-1,0.90, col = brewer.pal(11, "RdYlBu")[11], cex=1.1)
text(-0.95,0.90,"ECSQARU", adj = c(0,0.5), cex = 0.5)

dev.off()
par(op1)
