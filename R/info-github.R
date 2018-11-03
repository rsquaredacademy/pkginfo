#' Retrieve GitHub repository information
#'
#' @section Usage:
#' \preformatted{
#' myRepo <- GitHubRepo$new("user_name", "repo_name")
#' myRepo$get_stats()
#' myRepo$get_branches()
#' myRepo$get_issues()
#' myRepo$get_labels()
#' myRepo$get_milestones()
#' myRepo$get_coc()
#' myRepo$get_license()
#' myRepo$get_pull_requests()
#' myRepo$get_releases()
#' myRepo$get_travis_status()
#' myRepo$get_appveyor_status()
#' myRepo$get_coverage()
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{user_name}{Username of the GitHub repository owner.}  
#'   \item{repo_name}{Name of the GitHub repository.}
#' }
#'
#' @section Details:
#'
#' To create \code{GitHubRepo} objects, you need to use \code{GitHubRepo$new("user_name", "repo_name")}.
#'
#' \code{myRepo$get_stats()} will return the number of stars, forks and issues of the package.
#'
#' \code{myRepo$get_branches()} will return the name of the branches of the package.
#'
#' \code{myRepo$get_issues()} will return the list of open issues.
#'
#' \code{myRepo$get_labels()} will return the name and color labels used in issues filed in the package.
#'
#' \code{myRepo$get_milestones()} will return the details of milestones associated with the package.
#'
#' \code{myRepo$get_coc()} will return the code of conduct of the package.
#'
#' \code{myRepo$get_license()} will return license of the package.
#'
#' \code{myRepo$get_pull_requests()} will return all the open pull requests.
#'
#' \code{myRepo$get_releases()} will return all the releases of the package on GitHub.
#'
#' \code{myRepo$get_travis_status()} will return the build status of the package from Travis CI.
#'
#' \code{myRepo$get_appveyor_status()} will return the build status of the package from Appveyor.
#'
#' \code{myRepo$get_coverage()} will return code coverage of the package from Codecov.
#'
#' @examples
#' \dontrun{
#' myRepo <- GitHubRepo$new("tidyverse", "dplyr")
#' myRepo$get_stats()
#' myRepo$get_branches()
#' myRepo$get_travis_status()
#' myRepo$get_coverage()
#' }
#'
#' @name GitHubRepo
#' @docType class
#' @format An R6 class.
#' @export
#'
NULL

GitHubRepo <- R6::R6Class("GitHubRepo",
  public = list(
    user_name = NULL,
    repo_name = NULL,
    initialize = function(user_name = NA, repo_name = NA) {
      self$user_name <- user_name
      self$repo_name <- repo_name
    },
    get_stats = function() {
      get_gh_stats(self$user_name, self$repo_name)
    },
    get_branches = function() {
      get_gh_branches(self$user_name, self$repo_name)
    },
    get_issues = function() {
      get_gh_issues(self$user_name, self$repo_name)
    },
    get_labels = function() {
      get_gh_labels(self$user_name, self$repo_name)
    },
    get_milestones = function() {
      get_gh_milestones(self$user_name, self$repo_name)
    },
    get_coc = function() {
      get_gh_coc(self$user_name, self$repo_name)
    },
    get_license = function() {
      get_gh_license(self$user_name, self$repo_name)
    },
    get_pull_requests = function() {
      get_gh_pr(self$user_name, self$repo_name)
    },
    get_releases = function() {
      get_gh_releases(self$user_name, self$repo_name)
    },
    get_travis_status = function() {
      get_status_travis(self$user_name, self$repo_name)
    },
    get_appveyor_status = function() {
      get_status_appveyor(self$user_name, self$repo_name)
    },
    get_coverage = function() {
      get_code_coverage(self$user_name, self$repo_name)
    }
  )
)


#' Stars, forks and issues
#'
#' Returns number of stars, forks and open issues of a GitHub repository.
#'
#' @param user_name Name of the repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_gh_stats("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_stats <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg    <- pkg_github(user_name, repo_name)
  stars  <- pkg$stargazers_count
  issues <- pkg$open_issues_count
  forks  <- pkg$forks_count

  tibble::tibble(
    stars  = stars,
    issues = issues,
    forks  = forks
  )

}

pkg_github <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue::glue("/repos/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url)
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

}

#' Branches
#'
#' Returns names of branches.
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

	check_repo(user_name, repo_name)

  out <- connect_api(user_name, repo_name, "branches")
  tibble::tibble(branches = purrr::map_chr(out, "name"))

}

#' Issues
#'
#' Returns issues of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_issues("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_issues <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out            <- connect_api(user_name, repo_name, "issues")
  issue_number   <- purrr::map_int(out, "number")
  issue_title    <- purrr::map_chr(out, "title")
  issue_body     <- purrr::map_chr(out, "body")

  issue_date <- 
    out %>%
    purrr::map_chr("created_at") %>%
    as.Date()

  issue_user <-
    out %>%
	  purrr::map("user") %>%
	  purrr::map_chr("login")


  tibble::tibble(
    date        = issue_date,
    number      = issue_number,
    author      = issue_user,
    title       = issue_title
  )

}

#' Labels
#'
#' Returns labels of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_labels("tidyverse", "dplyr")
#' }
#'
#'
#' @export
#'
get_gh_labels <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out         <- connect_api(user_name, repo_name, "labels")
  label_name  <- purrr::map_chr(out, "name")
  label_color <- purrr::map_chr(out, "color")

  tibble::tibble(
    name  = label_name,
    color = label_color
  )

}

#' Milestones
#'
#' Returns milestones of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_milestones("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_milestones <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out      <- connect_api(user_name, repo_name, "milestones")
  m_title  <- purrr::map_chr(out, "title")
  m_body   <- purrr::map_chr(out, "description")
  m_open   <- purrr::map_int(out, "open_issues")
  m_closed <- purrr::map_int(out, "closed_issues")
  m_start  <- as.Date(purrr::map_chr(out, "created_at"))
  due      <- magrittr::extract2(out, 1)$due_on
  m_due    <- ifelse(is.null(due), NA, as.Date(due))

  tibble::tibble(
    title         = m_title,
    tart_date    = m_start,
    due_date      = m_due,
    description   = m_body,
    open_issues   = m_open,
    closed_issues = m_closed
  )

}

#' Code of conduct
#'
#' Returns code of conduct of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_coc("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_coc <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  pkg_name <- glue::glue("/repos/", user_name, "/", repo_name, "/community/code_of_conduct")
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url, httr::add_headers(Accept = "application/vnd.github.scarlet-witch-preview+json"))
  out      <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  cat(out$body)

}


#' License
#'
#' Returns license of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_license("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_license <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

	out <- connect_api(user_name, repo_name, "license")
  if (length(out) == 2) {
    cat("This repository does not have a license file.")
  } else {
    if (pingr::is_online()) {
      utils::browseURL(out$html_url)
    } else {
      cat("Please ensure your internet connection is working.")
    }
  }

}

#' Pull requests
#'
#' Returns pull requests of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_pr("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_pr <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out         <- connect_api(user_name, repo_name, "pulls")
  pull_number <- purrr::map_int(out, "number")
  pull_start  <- as.Date(purrr::map_chr(out, "created_at"))
  pull_title  <- purrr::map_chr(out, "title")
  pull_status <- purrr::map_chr(out, "state")

  tibble::tibble(
    number = pull_number,
    date   = pull_start,
    title  = pull_title,
    status = pull_status
  )

}

#' Releases
#'
#' Returns releases of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_releases("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_releases <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out               <- connect_api(user_name, repo_name, "releases")
  release_tag       <- purrr::map_chr(out, "tag_name")
  release_title     <- purrr::map_chr(out, "name")
  pre_release       <- purrr::map_lgl(out, "prerelease")
  release_published <- as.Date(purrr::map_chr(out, "published_at"))


  tibble::tibble(
    tag        = release_tag,
    date       = release_published,
    title      = release_title,
    prerelease = pre_release
  )

}

#' Travis build status
#'
#' Return the latest travis build status.
#'
#' @param user_name Name of the GitHub repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_status_travis("rsquaredacademy", "olsrr")
#' }
#'
#' @export
#'
get_status_travis <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue::glue("repos/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://api.travis-ci.org", path = pkg_name)
  resp     <- httr::GET(url)

  httr::content(resp, "parsed") %>%
    xml2::as_list() %>%
    magrittr::use_series('Projects') %>%
    magrittr::use_series('Project') %>%
    attributes() %>%
    magrittr::use_series('lastBuildStatus') %>%
    magrittr::extract(1)

}

#' Appveyor build status
#'
#' Return the latest appveyor build status.
#'
#' @param user_name Name of the GitHub repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_status_appveyor("rsquaredacademy", "olsrr")
#' }
#'
#' @export
#'
get_status_appveyor <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue::glue("/api/projects/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://ci.appveyor.com", path = pkg_name)
  resp     <- httr::GET(url)
  result   <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  result$build$status

}

#' Code coverage
#'
#' Return the code coverage of the package.
#'
#' @param user_name Name of the GitHub repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_code_coverage("rsquaredacademy", "olsrr")
#' }
#'
#' @export
#'
get_code_coverage <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue::glue("/api/gh/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://codecov.io", path = pkg_name)
  resp     <- httr::GET(url)
  result   <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  result$commit$totals$c

}

connect_api <- function(user_name, repo_name, node) {

	pkg_name <- glue::glue("/repos/", user_name, "/", repo_name, "/", node)
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url)
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

}


check_repo <- function(user_name, repo_name) {
  
  if (curl::has_internet()) {
    
    repo_url <- glue::glue("https://github.com/", user_name)
    
    repo_status <-
      repo_url %>%
      httr::GET() %>%
      httr::status_code()
    
    if (repo_status != 200) {
      stop("Please check the repository name.", call. = FALSE)
    }
    
    pkg_url <- glue::glue("https://github.com/", user_name, "/", repo_name)
    
    pkg_status <-
      pkg_url %>%
      httr::GET() %>%
      httr::status_code()
  
    if (pkg_status != 200) {
      stop("Please check the package name.", call. = FALSE)
    }
    
  } else {
    stop("Please check your internet connection.", call. = FALSE)
  }
  
}
