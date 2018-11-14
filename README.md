# Demystifying Automation on Simple Tasks [![Generic badge](https://img.shields.io/badge/release-v1.3.9-blue.svg)](https://github.com/pik-piam/demystas/releases/)

## Background

As data analysts, there are a myriad of daily tasks which seem simple, but yet hide complex computational requirements. These are the kind of challenges that we encounter that are not entirely part of our main objective, but need to be overcome nonetheless. These include, but are not limited to, conducting complicated string searches, querying spatial information and sometimes just organizing messy data.

This package aims to offer useful functions for data analysts which might help in making these "on-the-fly" tasks lighter and more automated.

## Installation via GitHub

To install via GitHub, clone this repository and navigate to its main directory:

```shell
$ git clone https://github.com/pik-piam/demystas && cd demystas
```
Within the main directory, open up a local R console using `RStudio` and execute the following:

```r
> if(!require(devtools)) install.packages("devtools")

> devtools::install(build_vignettes = TRUE)
```

**Note: For optimal building and viewing of vignettes, we recommend an installation via GitHub** 

## Installation via PIK-CRAN

For installation via PIK-CRAN, an additional repository must be added in R:

```r
> options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```

The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r
> install.packages("demystas")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r
> update.packages()
```

Voila, the package is yours! Feel free to test and develop it.

## Miscellaneous

To review a comprehensive vignette for this package, execute the following:

```r
> vignette("demystas")
```

To review help pages for our functions, execute the following:

```r
> ??demystas
```

In case the package is no longer needed, simply uninstall it by running this code in an R console:

```r
> remove.packages("demystas")
```

## Contribution

In order to contribute to the development of this package on GitHub, we would recommend reviewing some guidelines here: [CONTRIB.md](/doc/CONTRIB.md)

## Author

Atreya Shankar, Potsdam Institute for Climate Impact Research (PIK)
