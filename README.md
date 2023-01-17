# pk.model.diagram

<!-- badges: start -->
[![R-CMD-check](https://github.com/billdenney/pk.model.diagram/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/pk.model.diagram/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/billdenney/pk.model.diagram/branch/main/graph/badge.svg)](https://app.codecov.io/gh/billdenney/pk.model.diagram?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/pk.model.diagram)](https://CRAN.R-project.org/package=pk.model.diagram)
<!-- badges: end -->

The goal of pk.model.diagram is to ...

## Installation

You can install the development version of pk.model.diagram like so:

``` r
remotes::install_github("billdenney/pk.model.diagram")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pk.model.diagram)
one_cmt_states <- model_network_states(c("depot", "central", "elimination"), type = "pk")
one_cmt_edges <- 
  model_network_edges(
    from=c("depot", "central"),
    to = c("central", "elimination")
  )
```
