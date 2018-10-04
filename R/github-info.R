#' GitHub info
#'
#' Displays GitHub related information of a package.
#'
#' @param repo_name Name of the repository.
#' @param package_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_github_info("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_github_info <- function(repo_name, package_name) {

  pkg    <- pkg_github(repo_name, package_name)
  stars  <- pkg$stargazers_count
  issues <- pkg$open_issues_count
  forks  <- pkg$forks_count

  tibble(
    stars = stars,
    issues = issues,
    forks = forks
  )

}

#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#'
pkg_github <- function(repo_name, package_name) {

  pkg_name <- glue("/repos/", repo_name, "/", package_name)
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  fromJSON(content(resp, "text"), simplifyVector = FALSE)

}


