# Demystifying Automation on Simple Tasks

## Background

As data analysts, there are a myriad of daily tasks which seem simple, but yet hide complex computational requirements. These are the kind of challenges that we encounter that are not entirely part of our main objective, but need to be overcome nonetheless. These include, but are not limited to, conducting complicated string searches, querying spatial information and sometimes just organizing messy data.

This package aims to offer useful functions for data analysts which might help in making these "on-the-fly" tasks lighter and more automated.

## Installation

### A. Classic Installation

1. Clone this repository and navigate into its main directory

```shell
$ git clone https://github.com/AtreyaSh/demystas && cd demystas
```
2. Install the package and build its vignettes

```shell
$ R -e "if(!require(devtools)) install.packages('devtools'); devtools::install(build_vignettes = TRUE)"
```
### B. Quick Installation

Simply open up a local R console and execute the following:

```r
if(!require(devtools)) install.packages("devtools")

devtools::install_github("AtreyaSh/demystas", build_vignettes = TRUE)
```

Voila, the package is yours! Feel free to test it and contribute to development.

In case the package is no longer needed, simply uninstall it by running this code in an R console:

```r
remove.packages("demystas")
```

## Author

Atreya Shankar, Potsdam Institute for Climate Impact Research (PIK)
