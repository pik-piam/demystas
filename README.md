# Demystifying Automation on Simple Tasks

## Background

As data analysts, there are a myriad of daily tasks which seem simple, but yet hide complex computational requirements. These are the kind of challenges that we encounter that are not entirely part of our main objective, but need to be overcome nonetheless. These include, but are not limited to, conducting complicated string searches, querying spatial information and sometimes just organizing messy data.

This package aims to offer useful functions for data analysts which might help in making these "on-the-fly" tasks lighter and more automated.

## Installation

Simply open up a local R console and execute the following:

```r
> if(!require(devtools)) install.packages("devtools")

> devtools::install_github("AtreyaSh/demystas", build_vignettes = TRUE)
```

Voila, the package is yours! Feel free to test and develop it.

To review our vignettes and help pages for our functions, simply execute the following:

```r
> ??demystas
```

In case the package is no longer needed, simply uninstall it by running this code in an R console:

```r
> remove.packages("demystas")
```

## Contribution

In order to contribute to the development of this package on GitHub, we would recommend reviewing some guidelines here: [CONTRIB.md](https://github.com/AtreyaSh/demystas/blob/master/vignettes/CONTRIB.md)

## Author

Atreya Shankar, Potsdam Institute for Climate Impact Research (PIK)
