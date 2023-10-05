# fars

## Peer-graded Assignment: Building an R Package
The purpose of this assignment is creating, writing, documenting, and testing an R package with releasing that package on GitHub.  

For this assessment

1. write a vignette to include in your package using knitr and R Markdown

2. write at least one test written using testthat

3. put the package on GitHub

4. set up the repository so that the package can be checked and built on Travis

5. Once your package has built on Travis and the build is passing with no errors, warnings, or notes you should add your Travis badge to the README.md file of your package repository.

### Note: 
```
Travis is no longer free and Github is the alternative of Travis. Therefore, I used Github Action for CI.
```

### Github Action CI Test Status
<!-- badges: start -->

[![R-CMD-check](https://github.com/csubrata/fars/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/csubrata/fars/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->



## Installation

You can install the development version of fars from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("csubrata/fars")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fars)
## basic example code
fars_state_data <- fars_map_state(10, 2013)
```
