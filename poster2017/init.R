### first run coauthors-network.R!

normalize_author_name <- function(name) {
  isequal<- function(ref_name, name, dist = 4) {
    length(agrep(name, ref_name, max.distance = dist)) == 1
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
  if ( isequal("Andrey Bronevich", name)) 
    return("Andrew Bronevich")
  if ( isequal("Andres Cano", name, 0.01) )
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
  if ( isequal("Sebastien Destercke", name) )
    return("Sebastien Destercke")
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


# Author flows
## Number of authors contributing in conference i that have contributed in conference i-1 or i-2:
contributors_flow2 <- sapply(3:nconferences,
                             function(i) {
                               i <- i + 1 # as first column is "author"
                               as.logical(conferences_contributors[, i]) &
                                 (as.logical(conferences_contributors[, i-1]) |
                                    as.logical(conferences_contributors[, i-2]))
                             })
colnames(contributors_flow2) <- paste(colnames(conferences_contributors[, -c(1:3)]), "2", sep = "-")

## Number of authors contributing in conference i that have contributed in conference i-1, i-2 or i-3:
contributors_flow3 <- sapply(4:nconferences,
                             function(i) {
                               i <- i + 1 # as first column is "author"
                               as.logical(conferences_contributors[, i]) &
                                 (as.logical(conferences_contributors[, i-1]) |
                                    as.logical(conferences_contributors[, i-2]) |
                                    as.logical(conferences_contributors[, i-3]))
                             })
names(contributors_flow3) <- paste(colnames(conferences_contributors[, -c(1:4)]), "3", sep = "-")

# Recurring and new authors 
newauthor <- apply(authors_years, 1, function(x) sum(x[-1] == 0) + 1 )
newauthor <- data.frame(author = authors_years$author, confnumber = newauthor)
newyear <- newauthor$confnumber * 2 + 1997
newauthor <- data.frame(newauthor, year = newyear)



authorflow <- data.frame(year = t1$year,
                         unique = t1$unique_authors,
                         newauthor = as.numeric(table(newauthor$year)),
                         flow1 = c(NA, colSums(contributors_flow)),
                         flow2 = c(NA, NA, colSums(contributors_flow2)),
                         flow3 = c(NA, NA, NA, colSums(contributors_flow3)))
rownames(authorflow) <- NULL
authorflow[1,2] <- NA
names(authorflow) <- c("year", "unique", "New", "1-step", "2-step", "3-step")


# papers/authors of ECSQARU 2017
divs <- xml_find_all(read_html("http://www2.idsia.ch/cms/isipta-ecsqaru/accepted-papers-ecsqaru/"), ".//div[@class='omsc-toggle-title']")
authorslist <- sapply(strsplit(gsub(" and ", ", ", gsub("(.*)\\..+","\\1",xml_text(divs)), fixed = TRUE), ", "), function(x) {sapply(x, normalize_author_name)})
authorslist_df <- lapply(seq_along(authorslist), function(x) {
  data.frame(id = as.integer(x),
             author = authorslist[[x]],
             type = "ECSQARU",
             stringsAsFactors = FALSE)
})
authors_ecsqaru <- do.call("rbind", authorslist_df)

# different stati of authors 
founding <- as.character(subset(papers_authors, year == 1999, select = "author")[,1])
current <- as.character(subset(papers_authors, year == 2017, select = "author")[,1])
ecsqaru <- as.character(authors_ecsqaru$author)

# running variable for year
years <- sapply(levels(coauthors_pairs$year), grep, colnames(coauthors_years), value = TRUE)

# color from palettes used throughout graphics generation
palette7wc <- brewer.pal(9, "YlOrRd")[-(1:2)]
palette11 <- brewer.pal(11, "RdYlBu")
palette6 <- brewer.pal(6, "RdYlBu")

# legends
bottomlegend <- theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank())
rightlegend <- theme(legend.title = element_blank())


