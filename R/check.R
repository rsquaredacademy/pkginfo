#' Check CRAN results
#'
#' Return latest CRAN build results.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' \dontrun{
#' check_cran_results("dplyr")
#' }
#'
#' @export
#'
check_cran_results <- function(package_name) {

  check_result(package_name)

  url <- glue::glue(
    "https://cran.r-project.org/web/checks/check_results_", repo_name, ".html"
  )

  xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    magrittr::extract2(1)

}

#' Check travis build status
#'
#' Return the latest travis build status.
#'
#' @param user_name Name of the GitHub repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' check_travis("rsquaredacademy", "olsrr")
#' }
#'
#' @export
#'
check_travis <- function(user_name, repo_name) {

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

#' Check appveyor build status
#'
#' Return the latest appveyor build status.
#'
#' @param user_name Name of the GitHub repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' check_appveyor("rsquaredacademy", "olsrr")
#' }
#'
#' @export
#'
check_appveyor <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue::glue("/api/projects/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://ci.appveyor.com", path = pkg_name)
  resp     <- httr::GET(url)
  result   <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  result$build$status

}

#' Check code coverage
#'
#' Return the code coverage of the package.
#'
#' @param user_name Name of the GitHub repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' check_coverage("rsquaredacademy", "olsrr")
#' }
#'
#' @export
#'
check_coverage <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue::glue("/api/gh/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://codecov.io", path = pkg_name)
  resp     <- httr::GET(url)
  result   <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  result$commit$totals$c

}
 
check_result <- function(repo_name) {
  
  if (curl::has_internet()) {
    
    url <- glue::glue(
      "https://cran.r-project.org/web/checks/check_results_", repo_name, 
      ".html"
    )
    
    status <-
      url %>%
      httr::GET() %>%
      httr::status_code()
    
    if (status != 200) {
      stop("Please check the package name.", call. = FALSE)
    }
    
  } else {
    stop("Please check your internet connection.", call. = FALSE)
  }
  
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
