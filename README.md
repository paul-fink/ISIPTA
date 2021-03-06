
<!-- README.md is generated from README.Rmd. Please edit only the latter -->
ISIPTA.eProceedings
===================

A data R package with provides information about the imprecise probability community based on ISIPTA (electronic) proceedings <http://www.sipta.org/>. The data were gathered automatically from the respective proceedings' webpages and afterwards a manual error correction and consistency checking was performed.

A previous version of the package can be found at <http://mjaeugster.github.com/ISIPTA>.

Installation
------------

To easily install the current version of the package, the 'devtools' package is required:

``` r
install.packages("devtools")
devtools::install_github("paul-fink/ISIPTA/package")
```

Usage
-----

The package contains the following data.frame object which are loadable with the `data()` function:

-   `authors_locations`: Details of the authors' location at time of the conference
-   `conferences`: Details about the conferences
-   `papers`: Details about the papers (without authors and keywords)
-   `papers_authors`: Authors of papers
-   `papers_keywords`: Keywords of papers

Additionally, the file `collected_proceedings.xml` in the `xml` directory contains all information in one place.

The package contains several demos, displaying the data. For the list of availabe demos, use the following command:

``` r
demo(package = "ISIPTA.eProceedings")
```

Supplementary material
----------------------

The directories `poster2015` and `poster2017` contain the R code to generate the graphics on the posters which were presented at ISIPTA '15 and ISIPTA '17, respectively.

In the directory `generation` within the `package` directory, the code on how the data were gathered is supplied.
Please note, that only running the scripts will give different results to what is currently availabe, as the cleaning step was performed manually afterwards.
