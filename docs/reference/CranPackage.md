# Retrieve package information

Retrieve package information

## Format

An R6 class.

## Usage

    myPackage <- CranPackage$new("package_name")
    myPackage$get_downloads()
    myPackage$get_cran_check_results()
    myPackage$get_title()
    myPackage$get_description()
    myPackage$get_version()
    myPackage$get_r_dep()
    myPackage$get_imports()
    myPackage$get_suggest()
    myPackage$get_publish_date()
    myPackage$get_license()
    myPackage$get_authors()
    myPackage$get_maintainer()
    myPackage$get_urls()

## Arguments

- package_name:

  Name of the R packge.

## Details

To create `CranPackage` objects, you need to use
`CranPackage$new("package_name")`.

`myPackage$get_downloads()` will return the downloads of the package
from the RStudio CRAN mirror for the last day, last week, last month and
total downloads.

`myPackage$get_cran_check_results()` will return the CRAN check results
of the package.

`myPackage$get_title()` will return the title of the package.

`myPackage$get_description()` will return the description of the
package.

`myPackage$get_version()` will return the version of the package.

`myPackage$get_r_dep()` will return the R dependency of the package.

`myPackage$get_imports()` will return the R packages imported by the
package.

`myPackage$get_suggests()` will return the R packages suggested by the
package.

`myPackage$get_publish_date()` will return the date the package was
published on CRAN.

`myPackage$get_license()` will return the license under which the
package has been released.

`myPackage$get_authors()` will return the names of the authors of the
package.

`myPackage$get_maintainer()` will return the name of the maintainer of
the package.

`myPackage$get_urls()` will return the URLs associated with the package.

## Examples

``` r
if (FALSE) { # \dontrun{
# initialize object
myPackage <- CranPackage$new("dplyr")

# package title
myPackage$get_title()

# package version on CRAN
myPackage$get_version()

# R dependency version
myPackage$get_r_dep()

# packages imported
myPackage$get_imports()

} # }
```
