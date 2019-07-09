
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkginfo

> Tools for retrieving package information

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/pkginfo)](https://cran.r-project.org/package=pkginfo)
[![Travis-CI Build
Status](https://travis-ci.org/rsquaredacademy/pkginfo.svg?branch=master)](https://travis-ci.org/rsquaredacademy/pkginfo)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/pkginfo?branch=master&svg=true)](https://ci.appveyor.com/project/aravindhebbali/pkginfo)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

## Overview

`pkginfo` will allow users to retrieve information about a R package
from [CRAN](https://cran.r-project.org/), [GitHub](https://github.com/),
[Travis CI](https://travis-ci.org/),
[Appveyor](https://ci.appveyor.com/), [Codecov](https://codecov.io/) &
[Stack Overflow](https://stackoverflow.com/). While it was not developed
keeping any specific end user in mind, package developers might find it
more useful as they can monitor their R packages using a shiny app.
`pkginfo` uses
[cranlogs](https://cranlogs.r-pkg.org/),[crandb](https://github.com/r-hub/crandb)
and [cchecksapi](https://github.com/ropenscilabs/cchecksapi) under the
hood.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rsquaredacademy/pkginfo")
```

## Shiny App

Launch the shiny app with:

``` r
pkginfo_dashboard()
```

## Usage

### GitHub Info

``` r
myRepo <- GitHubRepo$new("olsrr", "rsquaredacademy")
myRepo$get_stats()
#> # A tibble: 1 x 3
#>   stars issues forks
#>   <int>  <int> <int>
#> 1    77     26    11
myRepo$get_issues()
#> # A tibble: 26 x 4
#>    date       number author        title                                   
#>    <date>      <int> <chr>         <chr>                                   
#>  1 2019-06-20    133 aravindhebba~ Cook's distance chart threshold         
#>  2 2018-12-27    129 aravindhebba~ Bonferroni outlier test                 
#>  3 2018-12-24    126 aravindhebba~ Heteroskedasticity corrected covariance~
#>  4 2018-12-24    125 aravindhebba~ Test linear hypothesis                  
#>  5 2018-12-24    124 aravindhebba~ Ceres plot                              
#>  6 2018-12-22    123 aravindhebba~ Power transformations                   
#>  7 2018-12-22    122 aravindhebba~ Box Tidwell transformation              
#>  8 2018-12-22    121 aravindhebba~ Box Cox transformation                  
#>  9 2018-12-21    120 aravindhebba~ Wald test                               
#> 10 2018-12-21    119 aravindhebba~ Reset test                              
#> # ... with 16 more rows
myRepo$get_branches()
#> # A tibble: 2 x 1
#>   branches
#>   <chr>   
#> 1 develop 
#> 2 master
myRepo$get_releases()
#> # A tibble: 9 x 4
#>   tag    date       title         prerelease
#>   <chr>  <date>     <chr>         <lgl>     
#> 1 v0.5.2 2018-11-23 olsrr 0.5.2   FALSE     
#> 2 v0.5.1 2018-05-04 v0.5.1        FALSE     
#> 3 v0.5.0 2018-03-26 olsrr 0.5.0   FALSE     
#> 4 0.4.2  2018-01-15 olsrr 0.4.2   FALSE     
#> 5 0.4.1  2017-12-22 olsrr 0.4.1   FALSE     
#> 6 0.4.0  2017-12-05 olsrr 0.4.0   FALSE     
#> 7 0.3.0  2017-09-03 olsrr 0.3.0   FALSE     
#> 8 0.2.0  2017-06-05 olsrr 0.2.0   FALSE     
#> 9 0.1.0  2017-05-11 First Release FALSE
```

### CRAN Info

``` r
myPackage <- CranPackage$new("olsrr")
myPackage$get_version()
#> [1] "0.5.2"
myPackage$get_r_dep()
#> [1] ">= 3.3"
myPackage$get_cran_check_results()
#> # A tibble: 12 x 4
#>    os               r      status url                                      
#>    <chr>            <chr>  <chr>  <chr>                                    
#>  1 linux-x86_64-de~ devel  OK     https://www.R-project.org/nosvn/R.check/~
#>  2 linux-x86_64-de~ devel  OK     https://www.R-project.org/nosvn/R.check/~
#>  3 linux-x86_64-fe~ devel  OK     https://www.R-project.org/nosvn/R.check/~
#>  4 linux-x86_64-fe~ devel  OK     https://www.R-project.org/nosvn/R.check/~
#>  5 windows-ix86+x8~ devel  OK     https://www.R-project.org/nosvn/R.check/~
#>  6 linux-x86_64     patch~ OK     https://www.R-project.org/nosvn/R.check/~
#>  7 solaris-x86      patch~ OK     https://www.R-project.org/nosvn/R.check/~
#>  8 linux-x86_64     relea~ OK     https://www.R-project.org/nosvn/R.check/~
#>  9 windows-ix86+x8~ relea~ OK     https://www.R-project.org/nosvn/R.check/~
#> 10 osx-x86_64       relea~ OK     https://www.R-project.org/nosvn/R.check/~
#> 11 windows-ix86+x8~ oldrel OK     https://www.R-project.org/nosvn/R.check/~
#> 12 osx-x86_64       oldrel OK     https://www.R-project.org/nosvn/R.check/~
myPackage$get_urls()
#> # A tibble: 3 x 2
#>   website urls                                           
#>   <chr>   <chr>                                          
#> 1 Bugs    https://github.com/rsquaredacademy/olsrr/issues
#> 2 Others  https://olsrr.rsquaredacademy.com/             
#> 3 GitHub  https://github.com/rsquaredacademy/olsrr
```

### Build Status

``` r
get_status_travis("olsrr", "rsquaredacademy")
#> [1] "Failure"
get_status_appveyor("olsrr", "rsquaredacademy")
#> [1] "failed"
get_code_coverage("olsrr", "rsquaredacademy")
#> [1] "82.83485"
```

### Stack Overflow Questions

``` r
get_so_questions("dplyr")
#> # A tibble: 30 x 6
#>    date       title               owner   answered views link              
#>    <date>     <chr>               <chr>   <lgl>    <int> <chr>             
#>  1 2019-07-09 Adding multiple co~ Moohan  FALSE        5 https://stackover~
#>  2 2017-12-27 dplyr Summarise im~ stacki~ TRUE        63 https://stackover~
#>  3 2019-07-09 Transform table pe~ "thoma~ FALSE       24 https://stackover~
#>  4 2019-07-09 Plotting graphs li~ Moon    FALSE       16 https://stackover~
#>  5 2019-07-09 dplyr: change vari~ MCS     FALSE       22 https://stackover~
#>  6 2019-07-09 Extracting part of~ Nart R~ FALSE       42 https://stackover~
#>  7 2019-07-09 Filtering on a Col~ Jonath~ TRUE        33 https://stackover~
#>  8 2019-07-02 Weird grouping err~ massis~ FALSE       43 https://stackover~
#>  9 2019-07-07 List of data frame~ Emman   TRUE        53 https://stackover~
#> 10 2019-07-09 Is there a functio~ Hanif ~ TRUE        30 https://stackover~
#> # ... with 20 more rows
```

## Community Guidelines

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
