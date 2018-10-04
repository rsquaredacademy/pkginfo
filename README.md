
<!-- README.md is generated from README.Rmd. Please edit that file -->
pkginfo
=======

> Tools for retrieving package information

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pkginfo)](https://cran.r-project.org/package=pkginfo) [![Travis-CI Build Status](https://travis-ci.org/rsquaredacademy/pkginfo.svg?branch=master)](https://travis-ci.org/rsquaredacademy/pkginfo) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/pkginfo?branch=master&svg=true)](https://ci.appveyor.com/project/rsquaredacademy/pkginfo) ![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rsquaredacademy/pkginfo")
```

Usage
-----

#### Download Statistics

``` r
get_downloads("olsrr")
#> # A tibble: 1 x 4
#>   latest last_week last_month total
#>    <dbl>     <dbl>      <dbl> <dbl>
#> 1    102       902       3216 30340
```

#### CRAN Results

``` r
check_cran_results("olsrr")
#>                               Flavor Version Tinstall Tcheck Ttotal Status
#> 1  r-devel-linux-x86_64-debian-clang   0.5.1    29.35 275.40 304.75     OK
#> 2    r-devel-linux-x86_64-debian-gcc   0.5.1    25.38 216.24 241.62     OK
#> 3  r-devel-linux-x86_64-fedora-clang   0.5.1       NA     NA 368.53     OK
#> 4    r-devel-linux-x86_64-fedora-gcc   0.5.1       NA     NA 368.09     OK
#> 5        r-devel-windows-ix86+x86_64   0.5.1    92.00 379.00 471.00     OK
#> 6             r-patched-linux-x86_64   0.5.1    23.67 257.41 281.08     OK
#> 7              r-patched-solaris-x86   0.5.1       NA     NA 469.50     OK
#> 8             r-release-linux-x86_64   0.5.1    28.39 257.47 285.86     OK
#> 9      r-release-windows-ix86+x86_64   0.5.1    72.00 539.00 611.00     OK
#> 10              r-release-osx-x86_64   0.5.1       NA     NA     NA     OK
#> 11      r-oldrel-windows-ix86+x86_64   0.5.1    52.00 535.00 587.00     OK
#> 12               r-oldrel-osx-x86_64   0.5.1       NA     NA     NA     OK
#>    Flags
#> 1     NA
#> 2     NA
#> 3     NA
#> 4     NA
#> 5     NA
#> 6     NA
#> 7     NA
#> 8     NA
#> 9     NA
#> 10    NA
#> 11    NA
#> 12    NA
```

#### Travis Build Status

``` r
check_travis("rsquaredacademy", "olsrr")
#> [1] "Success"
```

#### Appveyor Build Status

``` r
check_appveyor("rsquaredacademy", "olsrr")
#> [1] "success"
```

#### Code Coverage

``` r
check_coverage("rsquaredacademy", "olsrr")
#> [1] "84.38919"
```

#### GitHub Statistics

``` r
get_github_info("rsquaredacademy", "olsrr")
#> # A tibble: 1 x 3
#>   stars issues forks
#>   <int>  <int> <int>
#> 1    51      5     9
```

Community Guidelines
--------------------

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
