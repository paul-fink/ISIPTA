# graph can be 'grpah' for pure ISIPTA


getIdISIPTA <- function(name) {
  found <- grep(name, names(V(graph)))
  if(length(found)) {
    names(found) <- names(V(graph))[found]
    return(found)
  }
  cat(sprintf("Person with name like '%s' not found\n", name))
  invisible(integer(0))
}

getNameISIPTA <- function(id) {
  found <- names(V(graph))[id]
  if(!is.na(found)) {
    names(id) <- found
    return(id)
  }
  cat(sprintf("No person with id '%i'\n", id))
  invisible(integer(0))
}


getIdECSQARU <- function(name) {
  found <- grep(name, names(V(graph2017)))
  if(length(found)) {
    id <- V(graph2017)$label[found]
    names(id) <- names(V(graph2017))[found]
    return(id)
  }
  cat(sprintf("Person with name like '%s' not found\n", name))
  invisible(integer(0))
}


getNameECSQARU <- function(id) {
  found <- names(V(graph2017))[V(graph2017)$label == id]
  if(!is.na(found)) {
    names(id) <- found
    return(id)
  }
  cat(sprintf("No person with id '%i'\n", id))
  invisible(integer(0))
}
