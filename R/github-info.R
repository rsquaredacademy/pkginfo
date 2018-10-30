#' GitHub info
#'
#' Displays GitHub related information of a package.
#'
#' @param user_name Name of the repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_github_info("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_github_info <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg    <- pkg_github(user_name, repo_name)
  stars  <- pkg$stargazers_count
  issues <- pkg$open_issues_count
  forks  <- pkg$forks_count

  tibble(
    stars  = stars,
    issues = issues,
    forks  = forks
  )

}

#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#'
pkg_github <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue("/repos/", user_name, "/", repo_name)
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  fromJSON(content(resp, "text"), simplifyVector = FALSE)

}

#' GitHub branches
#'
#' Returns names of GitHub branches.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_branches("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_branches <- function(user_name, repo_name) {

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/branches")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
  map_chr(out, "name")

}
