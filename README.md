# pk.model.diagram

<!-- badges: start -->
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
