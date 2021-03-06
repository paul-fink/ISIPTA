
source("base-scraper.R")
source("xml.R")


scrap_paper <- function(url) {
  ## Raw values:
  raw <- readLines(url, encoding = "iso-8859-1")
  #raw <- iconv(raw, "latin1", "UTF-8")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE,
                        encoding = "iso-8859-1")

  paperid <- sub(".*/(\\d+)\\.html$", "\\1", url)
  authors <- xmlValue(getNodeSet(site, "/html/body/h2[2]/font")[[1]])
  title <- xmlValue(getNodeSet(site, "/html/body/h1/font")[[1]])
  abstract <- xmlValue(getNodeSet(site, "/html/body/p[4]")[[1]])
  keywords <- xmlValue(getNodeSet(site, "/html/body/p[5]")[[1]])
  pdf <- xmlAttrs(getNodeSet(site, "/html/body/ul/li[2]/a")[[1]])
  emails <- getNodeSet(site, "//table/tr",
                       fun = function(x) {
                         c(name = xmlValue(x[[1]]),
                           email = xmlValue(x[[2]]))
                       })

  free(site)


  ## Clean XML structure:
  xmlPaper(paperid,
           clean_title(title),
           clean_keywords(keywords),
           clean_abstract(abstract),
           clean_pdf(pdf),
           clean_authors(authors, emails))
}


scrap_proceedings <- function(url, year, date, location) {
  ## Papers URLs:
  site <- htmlTreeParse(url, useInternalNodes = TRUE,
                        encoding = "iso-8859-1")

  paper_urls <- getNodeSet(site, "//a/font[@color='555500']/..",
                           fun = xmlAttrs)
  paper_urls <- paste(url, paper_urls, sep = "")

  free(site)


  ## Scrap each paper:
  proc <- xmlProceedings(year, date, location)
  for ( url in paper_urls )
    proc$addNode(scrap_paper(url))

  proc
}



### ISIPTA 2007:

i07 <- scrap_proceedings("http://www.sipta.org/isipta07/proceedings/",
                         "2007",
                         c("2007-07-16", "2009-07-19"),
                         c(country_name = "Czech Republic",
                           country_code = "CZ",
                           city = "Prague",
                           city_lat = "50.08804",
                           city_lon = "14.42076",
                           university = "Charles University",
                           department = "Faculty of Mathematicsand Physics"))

saveXML(i07$value(), file = "../xml/isipta2007.xml")



### ISIPTA 2009:

i09 <- scrap_proceedings("http://www.sipta.org/isipta09/proceedings/",
                         "2009",
                         c("2009-07-14", "2009-07-18"),
                         c(country_name = "United Kingdom",
                           country_code = "GB",
                           city = "Durham",
                           city_lat = "54.77639",
                           city_lon = "-1.57587",
                           university = "Durham University",
                           department = "Department of Mathematical Sciences"))

saveXML(i09$value(), file = "../xml/isipta2009.xml")
