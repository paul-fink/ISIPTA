#' @name ISIPTA_CACHE
#' 
#' @title Caching mechanism
#' 
#' @description The cache is used to store all objects created with the
#' demos to allow for further inspection of the data. The created cache 
#' is used by the functions working on the collaboration network.
#' 
#' @seealso
#'   \code{\link{ISIPTA_author}},
#'   \code{\link{ISIPTA_coauthors}},
#'   \code{\link{ISIPTA_path}}
NULL


#' @return
#'   \code{ISIPTA_cache_demo} returns an environment.
#'   See Details.
#'   
#' @details 
#'   The returned environment of \code{ISIPTA_cache_demo}
#'   is an ancestor of the empty environment to avoid
#'   name clashes and contains the environment of the demos.
#'   Within the demo environment all object created 
#'   in the demos are stored.
#' 
#' @rdname ISIPTA_CACHE

ISIPTA_cache_demo <- function() {
  envir <- new.env(parent = emptyenv())
  file <- system.file("extdata", "demo-cache.RData", package = "ISIPTA.eProceedings")
  #file <- "./../inst/extdata/demo-cache.RData" # if called in ISIPTA/R
  #file <- "./inst/extdata/demo-cache.RData"     # if called in ISIPTA
  load(file, envir = envir)
  envir
}

## Created the cache
ISIPTA_CACHE <- ISIPTA_cache_demo()

#' @param name an object name to look for 
#'   in ISIPTA cache, given as character.
#' 
#' @return
#'   \code{getCached} returns the object of given name
#'   within ISIPTA cache.
#' 
#' @rdname ISIPTA_CACHE

getCached <- function(name) {
  get(name, envir = ISIPTA_CACHE$demos)
}


#' @inheritParams getCached
#'
#' @return \code{isCached} returns \code{TRUE} if object
#'   with querried name is found within ISIPTA cache and
#'   else \code{FALSE}.
#'
#' @rdname ISIPTA_CACHE

isCached <- function(name) {
  exists(name, envir = ISIPTA_CACHE$demos)
}


#' @param value a value to be assigned to \code{name}.
#' @inheritParams getCached
#'
#' @return If there was an object previously stored
#'   under \code{name}, then \code{setCached} returns it
#'   and otherwise \code{NULL} is returned. 
#'
#' @rdname ISIPTA_CACHE

setCached <- function(name, value) {
  ret <- NULL
  if(isCached(name)) {
    ret <- getCached(name)
  }
  assign(name, value, envir = ISIPTA_CACHE$demos)
  ret
}
