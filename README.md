
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stocksShiny

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of stocksShiny is to be able to download metrics from different
tickers of the S&P500 and gain insights about returns, volatility and
other indicators.

## Installation

You can install the development version of stocksShiny like so:

``` r
devtools::install_github('jpdugo/stocksShiny')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(stocksShiny)
## run the app
stocksShiny::run_app()
#> Loading required package: shiny
#> 
#> Listening on http://127.0.0.1:4737
```

<img src="man/figures/README-example-1.png" width="100%" />
