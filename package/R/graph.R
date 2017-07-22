#####################################################################
### Functions looking for nodes in ISIPTA collaboration network   ###
#####################################################################

#' Find authors within the ISIPTA collaboration network
#' 
#' \code{nodesById} looks for matching nodes by ID, 
#' \code{nodesByPattern} by name pattern and 
#' \code{findSingleNode} for a single one by means of the previous
#' functions.
#' 
#' @param id integer, node ID as shown in the graph (see
#'   \code{demo("coauthors-network")}).
#'
#' @return \code{nodesById} returns a \code{data.frame} with the
#'   corresponding author names. If no matching node is found,
#'   the returned \code{data.frame} has 0 rows.
#'
#' @seealso \code{\link{ISIPTA_author}} for looking up
#'   information about an author
#'
#' @examples
#' ## Find author by internal node id
#' nodesById(81:83)
#'
#' @export

nodesById <- function(id){

  # sanity check
  if(all(id %in% seq_along(igraph::V(getCached("graph"))$name))) {
    # nodeid is valid index
    return(data.frame(name = igraph::V(getCached("graph"))$name[id],
                      nodeid = id, stringsAsFactors = FALSE))
  }
  # return empty data.frame
  data.frame(name = character(0L), nodeid = integer(0L),
             stringsAsFactors = FALSE)
  
}


#' @param pattern character, containing patten of an author name. If
#'   \code{length(pattern) > 1} only the first element is used.
#'
#' @return \code{nodesByPattern} returns a \code{data.frame} with each
#'   author matching the \code{pattern} and the corresponding graph
#'   node id (see \code{demo("coauthors-network")}. If no matching
#'   node is found, the returned \code{data.frame} has 0 rows.
#'
#' @examples
#' ## Find author by (partial) name
#' nodesByPattern("Paul")
#'
#' @rdname nodesById
#' 
#' @export

nodesByPattern <- function(pattern) {
  
  id <- grep(pattern, igraph::V(getCached("graph"))$name)
  
  if(length(id)) {
    # Name pattern was found
    return(data.frame(
      name = igraph::V(getCached("graph"))$name[id],
      nodeid = as.integer(id),
      stringsAsFactors = FALSE
    ))
  }
  
  # return empty data.frame
  data.frame(name = character(0L), nodeid = integer(0L),
             stringsAsFactors = FALSE)

}


#' @param node character of the pattern of an author name or
#'   integer of internal node id.
#'
#' @return \code{findSingleNode} returns a \code{data.frame} 
#'   with variable names \code{name} and \code{nodeid} holding
#'   the author name and the corresponding graph node id.
#'   If the call to \code{nodesByPattern} or 
#'   \code{nodesById} does not return a single node
#'   an error is raised.
#'   
#' @details 
#'   In contrast to \code{nodesByPattern} and
#'   \code{nodesById} an object is only returned if the
#'   input \code{node} to \code{findSingleNode} uniquely matches
#'   a node in the graph.
#'   
#' @examples
#' ## Find only one author by (partial) name
#' findSingleNode("Fink")
#' 
#' ## Find only one author by nodeid
#' findSingleNode(251)
#'
#' @rdname nodesById
#' 
#' @export

findSingleNode <- function(node) {
  
  authornodes <- if(is.character(node)){
    nodesByPattern(node)
  } else {
    nodesById(node)
  }
  if(nrow(authornodes) == 0) {
    stop("no author found for this pattern/node id")
  }
  if(nrow(authornodes) > 1) {
    stop("more than one authors found; only one author allowed")
  }
  authornodes
}



#####################################################################
### Summary functions for authors                                 ###
#####################################################################

#' Summarize authors within the ISIPTA proceedings
#' 
#' Summarizing the information about an ISIPTA author, containing
#' coauthors and contributed papers and conferences contributed to.
#' 
#' @param node character of the pattern of an author name or
#'   integer of internal node id. It will be passed on to
#'   \code{findSingleNode}.
#'
#' @return \code{ISIPTA_author} returns a list with information on
#'   the author of class \code{ISIPTA_author};
#'   \code{print.ISIPTA_author} nicely prints the result.
#'
#' @examples
#' sumGero <- ISIPTA_author("Gero Walter")
#' sumGero
#'
#' @seealso
#'   \code{\link{findSingleNode}},
#'   \code{\link{ISIPTA_coauthors}},
#'   \code{\link{ISIPTA_path}}
#'
#' @export

ISIPTA_author <- function(node) {
  
  ## Make R CMD check happy:
  author <- ncoauthors <- author1 <- author2 <- npapers <- NULL

  ## Load all required from cache
  papers <- getCached("papers")
  paper_authors <- getCached("papers_authors")
  author_npapers <- getCached("authors_npapers_overall")
  paper_ncoauthors <- getCached("papers_ncoauthors_overall")
  coauthor_npairs <- getCached("coauthors_npairs")
  conf_contribs <- getCached("conferences_contributors")
  
  ret <- list()

  ## Author:
  ret$author <- findSingleNode(node)
  aname <- ret$author$name
  
  ## Contributions:
  contribs <- paper_authors[
    paper_authors$author == aname, "id"]

  ret$papers <-
    lapply(contribs, function(x) {
      y <- list()
      y$paper <- papers[
        papers$id == x, ]
      y$authors <- paper_authors[
        paper_authors$id == x, "author", drop = FALSE]
      y$authors <- do.call(rbind, lapply(y$authors$author,
                                         nodesByPattern))
      y
    })

  ret$conferences <- unlist(conf_contribs[
    conf_contribs$author == aname, -1])

  ret$ncoauthors <-  as.numeric(paper_ncoauthors[
    paper_ncoauthors$author == aname, "ncoauthors"])


  ret$ncoauthors_unique <- nrow(coauthor_npairs[
    coauthor_npairs$author1 == aname | 
      coauthor_npairs$author2 == aname, ])
  
  ret$npapers <- as.numeric(author_npapers[
    author_npapers$author == aname, "npapers"])


  structure(ret, class = "ISIPTA_author")
}


#' @param x object of class \code{ISIPTA_author}.
#' @param show.papers logical, indicating whether or not paper
#'   details should be displayed.
#' @param ... further arguments, currently ignored.
#' 
#' @rdname ISIPTA_author
#'   
#' @examples
#' ## Showing detailed information of papers too
#' print(sumGero, show.papers = TRUE)
#' 
#' @method print ISIPTA_author
#' 
#' @export

print.ISIPTA_author <- function(x, show.papers = FALSE, ...) {

  pauthor <- function(x) {
    sprintf("%s (%s)", x["name"], x["nodeid"])
  }

  pnumbers <- function() {
    cbind(c("  Papers",
            "  Coauthors",
            "  Unique coauthors",
            "  Conferences"),
          c(x$npapers,
            x$ncoauthors,
            x$ncoauthors_unique,
            sum(x$conferences)))
  }

  names(x$conferences) <-
    sub("ISIPTA", "", names(x$conferences))

  cat(sprintf("ISIPTA summary for %s\n",
              pauthor(as.matrix(x$author)[1, ])), sep = "\n")

  cat("Number of ...:\n")
  cat(formatDL(pnumbers(), width = getOption("width")), sep = "\n")

  cat("\nContributions:\n")
  print(x$conferences == 1)

  if ( show.papers ) {
    cat("\nPapers:\n")
    for ( paper in x$papers ) {
      txt <- sprintf("%s. %s. %s.",
                     paper$paper$title,
                     paste(apply(paper$authors, 1, pauthor),
                           collapse = ", "),
                     paper$paper$year)
      txt <- strwrap(txt, width = 0.7 * getOption("width"))
      txt <- paste(txt, collapse = "\n")
      cat(txt, "\n\n")
    }
  }
  invisible(x)
}


#####################################################################
### Summary functions for coauthors                               ###
#####################################################################

#' Summarize the coauthors
#' 
#' List direct coauthors for an ISIPTA author and her/his 
#' neighbourhood authors.
#' 
#' @param node character of the pattern of an author name or
#'   integer of internal node id to be used as the center.
#'   If the call to \code{\link{nodesByPattern}} or 
#'   \code{\link{nodesById}} does not return a single node
#'   an error is raised.
#' @param order non-negative numeric for the order of the
#'   neighbourhood. It is coerced to an integer. An order of '0'
#'   is the author herself/himself and an order of '1' returns
#'   additionally her/his direct coauthors.
#'   
#' @return An object of class "ISIPTA_coauthors" is a 
#'   \code{data.frame} containing the following components:
#'     \item{name}{author name}
#'     \item{nodeid}{respective node id}
#'     \item{order}{neighbourhood order}
#'   The rows are ordered in ascending neighbourhood order.
#'
#' @examples
#' ## Find all coauthors of Paul Fink
#' ISIPTA_coauthors("Paul Fink")
#'   
#' ## Find all coauthors of coauthors of Paul Fink
#' ISIPTA_coauthors(251, order = 2)
#'   
#' @seealso
#'   \code{\link{findSingleNode}},
#'   \code{\link{ISIPTA_author}},
#'   \code{\link{ISIPTA_path}}
#'   
#' @export

ISIPTA_coauthors <- function(node, order = 1) {
  
  authorid <- findSingleNode(node)$nodeid
  
  graph <- getCached("graph")
  
  coauthorsgraph <- nodesById(as.numeric(
    igraph::ego(graph,
                order = order,
                nodes = authorid)[[1]]
  ))
  
  # calulating the respective neighbourhood order
  nsize <- unlist(lapply(seq(from = 0, to = order),
                         function(no) {
                           igraph::ego_size(graph,
                                            order = no,
                                            nodes = authorid)
                           })
                  )
  coauthorsgraph$order <- c(0, rep(seq_len(order), 
                                   times = diff(nsize)))

  class(coauthorsgraph) = c("ISIPTA_coauthors", "data.frame")
  coauthorsgraph
}


#' @param x object of class \code{ISIPTA_coauthors}.
#' @param ... further arguments, currently ignored.
#' 
#' @rdname ISIPTA_coauthors
#' 
#' @method print ISIPTA_coauthors
#' 
#' @export

print.ISIPTA_coauthors <- function(x, ...) {

  pauthor <- function(x) {
    sprintf("%s (%s)", x[1], x[2])
  }

  cat(sprintf("ISIPTA coauthor neighbourhood up to order %s for:\n%s\n",
              max(x$order), pauthor(x[1,]), sep = "\n"))
  
  for (i in 2:nrow(x)) {
    cat(sprintf("|- %s%s\n",
                paste0(rep("-- ", times = x$order[i]-1),
                       collapse = ""),
                pauthor(x[i, ]), sep = "\n"))
  }
}


#####################################################################
### Summary functions for paths                                   ###
#####################################################################

#' Find path between authors
#' 
#' Display the path between two authors in the ISIPTA collaboration
#' network.
#' 
#' @param from,to character of the pattern of an author name or
#'   integer of internal node id to be used as the center.
#'   If the call to \code{\link{nodesByPattern}} or 
#'   \code{\link{nodesById}} does not return a single node
#'   an error is raised.
#'  
#' @return An object of class "ISIPTA_path" is a 
#'   \code{data.frame} containing the following components:
#'     \item{name}{author name}
#'     \item{nodeid}{respective node id}
#'   The rows are ordered in the way that first row is
#'   'from' author and last row is 'to' author.
#'  
#' @details 
#'   In case there exists no path in the network between the 
#'   authors in 'from' and 'to' then a warning is issued and the
#'   returned \code{data.frame} contains zero rows.
#'   
#' @examples 
#' ## Path from author 251 (Paul Fink) to Gero Walter (117)
#' ISIPTA_path(from = 251, to = "Gero Walter")
#'   
#' @seealso
#'   \code{\link{findSingleNode}},
#'   \code{\link{ISIPTA_author}},
#'   \code{\link{ISIPTA_coauthors}}
#'   
#' @export 

ISIPTA_path <- function(from, to) {
  
  fromId <- findSingleNode(from)$nodeid
  toId <- findSingleNode(to)$nodeid
 
  nodePath <- suppressWarnings(
    igraph::shortest_paths(getCached("graph"),
                           from = fromId,
                           to = toId)$vpath)
  path <- nodesById(as.numeric(nodePath[[1]]))
  if(nrow(path) == 0) {
    warning("no path between 'to' and 'from'")
  }
  
  class(path) <- c("ISIPTA_path", "data.frame")
  path
}


#' @param x object of class \code{ISIPTA_path}.
#' @param ... further arguments, currently ignored.
#' 
#' @rdname ISIPTA_path
#' 
#' @method print ISIPTA_path
#' 
#' @export

print.ISIPTA_path <- function(x, ...) {
  
  pauthor <- function(x) {
    sprintf("%s (%s)", x[1], x[2])
  }
  
  xlength <- nrow(x)
  
  ## Only do printing for actual path
  if(xlength > 0) {
    cat(sprintf("ISIPTA path from %s to %s is %s",
                pauthor(x[1,]), 
                pauthor(x[xlength,]), 
                if(xlength == 1) {
                  "direct (same person)."
                } else if(xlength == 2) {
                  "direct (coauthors)."
                } else {
                  "via:"
                }),"\n")
    if(xlength > 2) {
      for (i in 2:(xlength - 1)) {
        cat(sprintf("  %s\n", pauthor(x[i, ])))
      }
    }
  }
  invisible(x)
}
