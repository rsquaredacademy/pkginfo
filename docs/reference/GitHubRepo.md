# Retrieve GitHub information

Retrieve GitHub information

## Format

An R6 class.

## Usage

    myRepo <- GitHubRepo$new("repo_name", "user_name")
    myRepo$get_stats()
    myRepo$get_branches()
    myRepo$get_issues()
    myRepo$get_labels()
    myRepo$get_milestones()
    myRepo$get_coc()
    myRepo$get_license()
    myRepo$get_pull_requests()
    myRepo$get_releases()
    myRepo$get_travis_status()
    myRepo$get_appveyor_status()
    myRepo$get_coverage()

## Arguments

- user_name:

  GitHub user or organization name.

- repo_name:

  Name of the GitHub repository.

## Details

To create `GitHubRepo` objects, you need to use
`GitHubRepo$new("repo_name", "user_name")`.

`myRepo$get_stats()` will return the number of stars, forks and issues
of the package.

`myRepo$get_branches()` will return the name of the branches of the
package.

`myRepo$get_issues()` will return the list of open issues.

`myRepo$get_labels()` will return the name and color labels used in
issues filed in the package.

`myRepo$get_milestones()` will return the details of milestones
associated with the package.

`myRepo$get_coc()` will return the code of conduct of the package.

`myRepo$get_license()` will return license of the package.

`myRepo$get_pull_requests()` will return all the open pull requests.

`myRepo$get_releases()` will return all the releases of the package on
GitHub.

`myRepo$get_travis_status()` will return the build status of the package
from Travis CI.

`myRepo$get_appveyor_status()` will return the build status of the
package from Appveyor.

`myRepo$get_coverage()` will return code coverage of the package from
Codecov.

## Examples

``` r
if (FALSE) { # \dontrun{
# initialize object
myRepo <- GitHubRepo$new("dplyr", "tidyverse")

# get stats
myRepo$get_stats()

# get branches
myRepo$get_branches()

# get travis ci build status
myRepo$get_travis_status()

# get codecov code coverage
myRepo$get_coverage()
} # }
```
