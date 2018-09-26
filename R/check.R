#' Check CRAN results
#'
#' Return latest CRAN build results.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' check_cran_results("dplyr")
#'
#' @importFrom pingr is_online
#' @importFrom glue glue
#' @importFrom magrittr extract2 %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#'
#' @export
#'
check_cran_results <- function(package_name) {

  if (!is_online()) {
    stop("Please ensure that you are connected to the internet.", call. = FALSE)
  }

  url <- glue(
    "https://cran.r-project.org/web/checks/check_results_", package_name, ".html"
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
#' @param repo_name Name of the GitHub repository.
#' @param package_name Name of the package.
#'
#' @examples
#' check_travis("rsquaredacademy", "olsrr")
#'
#' @importFrom httr modify_url GET content
#' @importFrom magrittr use_series extract
#' @importFrom xml2 as_list
#'
#'
#' @export
#'
check_travis <- function(repo_name, package_name) {

  if (!is_online()) {
    stop("Please ensure that you are connected to the internet.", call. = FALSE)
  }

  pkg_name <- glue("repos/", repo_name, "/", package_name)
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
#' @param repo_name Name of the GitHub repository.
#' @param package_name Name of the package.
#'
#' @examples
#' check_appveyor("rsquaredacademy", "olsrr")
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
check_appveyor <- function(repo_name, package_name) {

  if (!is_online()) {
    stop("Please ensure that you are connected to the internet.", call. = FALSE)
  }

  pkg_name <- glue("/api/projects/", repo_name, "/", package_name)
  url      <- modify_url("https://ci.appveyor.com", path = pkg_name)
  resp     <- GET(url)
  result   <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
  result$build$status

}

#' Check code coverage
#'
#' Return the code coverage of the package.
#'
#' @param repo_name Name of the GitHub repository.
#' @param package_name Name of the package.
#'
#' @examples
#' check_coverage("rsquaredacademy", "olsrr")
#'
#' @export
#'
check_coverage <- function(repo_name, package_name) {

  if (!is_online()) {
    stop("Please ensure that you are connected to the internet.", call. = FALSE)
  }

  pkg_name <- glue("/api/gh/", repo_name, "/", package_name)
  url      <- modify_url("https://codecov.io", path = pkg_name)
  resp     <- GET(url)
  result   <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
  result$commit$totals$c

}
