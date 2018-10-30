#' Check CRAN results
#'
#' Return latest CRAN build results.
#'
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' check_cran_results("dplyr")
#' }
#'
#' @importFrom pingr is_online
#' @importFrom glue glue
#' @importFrom magrittr extract2 %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#'
#' @export
#'
check_cran_results <- function(repo_name) {

  check_result()

  url <- glue(
    "https://cran.r-project.org/web/checks/check_results_", repo_name, ".html"
  )

  read_html(url) %>%
    html_nodes("table") %>%
    html_table() %>%
    extract2(1)

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
#' @importFrom httr modify_url GET content
#' @importFrom magrittr use_series extract
#' @importFrom xml2 as_list
#'
#'
#' @export
#'
check_travis <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue("repos/", user_name, "/", repo_name)
  url      <- modify_url("https://api.travis-ci.org", path = pkg_name)
  resp     <- GET(url)

  content(resp, "parsed") %>%
    as_list() %>%
    use_series('Projects') %>%
    use_series('Project') %>%
    attributes() %>%
    use_series('lastBuildStatus') %>%
    extract(1)

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
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
check_appveyor <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue("/api/projects/", user_name, "/", repo_name)
  url      <- modify_url("https://ci.appveyor.com", path = pkg_name)
  resp     <- GET(url)
  result   <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
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

  pkg_name <- glue("/api/gh/", user_name, "/", repo_name)
  url      <- modify_url("https://codecov.io", path = pkg_name)
  resp     <- GET(url)
  result   <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
  result$commit$totals$c

}


#' @importFrom curl has_internet
#' @importFrom httr GET status_code
#' 
check_result <- function(repo_name) {
  
  if (has_internet()) {
    
    url <- glue(
      "https://cran.r-project.org/web/checks/check_results_", repo_name, 
      ".html"
    )
    
    status <-
      url %>%
      GET() %>%
      status_code()
    
    if (status != 200) {
      stop("Please check the package name.", call. = FALSE)
    }
    
  } else {
    stop("Please check your internet connection.", call. = FALSE)
  }
  
}

check_repo <- function(user_name, repo_name) {
  
  if (has_internet()) {
    
    repo_url <- glue("https://github.com/", user_name)
    
    repo_status <-
      repo_url %>%
      GET() %>%
      status_code()
    
    if (repo_status != 200) {
      stop("Please check the repository name.", call. = FALSE)
    }
    
    pkg_url <- glue("https://github.com/", user_name, "/", repo_name)
    
    pkg_status <-
      pkg_url %>%
      GET() %>%
      status_code()
  
    if (pkg_status != 200) {
      stop("Please check the package name.", call. = FALSE)
    }
    
  } else {
    stop("Please check your internet connection.", call. = FALSE)
  }
  
}
