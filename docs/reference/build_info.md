# Retrieve build information.

Returns build status from Travis CI and Appveyor, and code coverage from
codecov.

## Usage

``` r
get_status_travis(repo_name, user_name = NULL)

get_status_appveyor(repo_name, user_name = NULL)

get_code_coverage(repo_name, user_name = NULL)
```

## Arguments

- repo_name:

  Name of the package.

- user_name:

  GitHub user or organization name.

## Examples

``` r
if (FALSE) { # \dontrun{
# get travis ci build status
get_status_travis("dplyr", "tidyverse")

# get appveyor build status
get_status_appveyor("dplyr", "tidyverse")

# get codecov code coverage
get_code_coverage("dplyr", "tidyverse")
} # }
```
