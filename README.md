
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ritalic

<!-- badges: start -->
<!-- badges: end -->

**ritalic** is an R package which gives you access to
[ITALIC](https://italic.units.it/) mediated data via its [REST
API](https://www.italic.units.it/?procedure=api).

**ITALIC** the Information System on Italian Lichens, makes available
information and resources about the lichens known to occur in Italy. It
is maintained and updated by the Research Unit of Professor Pier Luigi
Nimis, at the University of Trieste (NE Italy), Department of Life
Sciences. Most of the data are derived from the Checklist of the Lichens
of Italy by Nimis (2016), but nomenclatural and distributional data are
being continuously updated online, and complete identification keys for
some areas of the country, as well as for genera or groups of genera,
are published online for testing.

## Installation

You can install the development version of ritalic from
[GitHub](https://github.com/Mattciao96/ritalic) with:

``` r
# install.packages("devtools")
devtools::install_github("Mattciao96/ritalic")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ritalic)
#> Loading required package: httr
#> Loading required package: jsonlite
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
