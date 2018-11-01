#' Retrieve package information
#'
#' @section Usage:
#' \preformatted{
#' myPackage <- CranPackage$new("package_name")
#' myPackage$get_downloads()
#' myPackage$get_results()
#' myPackage$get_title()
#' myPackage$get_description()
#' myPackage$get_version()
#' myPackage$get_r_dep()
#' myPackage$get_imports()
#' myPackage$get_suggest()
#' myPackage$get_publish_date()
#' myPackage$get_license()
#' myPackage$get_authors()
#' myPackage$get_maintainer()
#' myPackage$get_urls()
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{package_name}{Name of the R packge.}
#' }
#'
#' @section Details:
#'
#' To create \code{CranPackage} objects, you need to use \code{CranPackage$new("package_name")}.
#'
#' \code{myPackage$get_downloads()} will return the downloads of the package
#' from the RStudio CRAN mirror for the last day, last week, last month and
#' total downloads.
#'
#' \code{myPackage$get_check_results()} will return the CRAN check results of
#' the package.
#'
#' \code{myPackage$get_title()} will return the title of the package.
#'
#' \code{myPackage$get_description()} will return the description of the package.
#'
#' \code{myPackage$get_version()} will return the version of the package.
#'
#' \code{myPackage$get_r_dep()} will return the R dependency of the package.
#'
#' \code{myPackage$get_imports()} will return the R packages imported by the package.
#'
#' \code{myPackage$get_suggests()} will return the R packages suggested by the package.
#'
#' \code{myPackage$get_publish_date()} will return the date the package was published on CRAN.
#'
#' \code{myPackage$get_license()} will return the license under which the package has been released.
#'
#' \code{myPackage$get_authors()} will return the names of the authors of the package.
#'
#' \code{myPackage$get_maintainer()} will return the name of the maintainer of the package.
#'
#' \code{myPackage$get_urls()} will return the URLs associated with the package.
#'
#' @examples
#' \dontrun{
#' myPackage <- CranPackage$new("dplyr")
#' myPackage$get_title()
#' myPackage$get_version()
#' myPackage$get_r_deps()
#' myPackage$get_imports()
#' }
#'
#' @name CranPackage
#' @docType class
#' @format An R6 class.
#' @export
#'
NULL

CranPackage <- R6::R6Class("CranPackage",
  public = list(
    package_name = NULL,
    initialize = function(package_name = NA) {
      self$package_name <- package_name
    },
    get_downloads = function() {
      get_cran_downloads(self$package_name)
    },
    get_title = function() {
      get_cran_title(self$package_name)
    },
    get_description = function() {
      get_cran_desc(self$package_name)
    },
    get_version = function() {
      get_cran_version(self$package_name)
    },
    get_r_dep = function() {
      get_cran_r_dep(self$package_name)
    },
    get_imports = function() {
      get_cran_imports(self$package_name)
    },
    get_suggests = function() {
      get_cran_suggests(self$package_name)
    },
    get_publish_date = function() {
      get_cran_pub_date(self$package_name)
    },
    get_license = function() {
      get_cran_license(self$package_name)
    },
    get_authors = function() {
      get_cran_authors(self$package_name)
    },
    get_maintainer = function() {
      get_cran_maintainer(self$package_name)
    },
    get_urls = function() {
      get_cran_urls(self$package_name)
    },
    get_check_results = function() {
      get_cran_results(self$package)
    }
  )
)


#' Downloads
#'
#' Package downloads from RStudio CRAN mirror.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_cran_downloads("dplyr")
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
get_cran_downloads <- function(package_name) {

  check_cran(package_name)
  count <- NULL

  latest <- lubridate::today() - 2
  last_day <- cranlogs::cran_downloads(package_name, from = latest, to = latest) %>%
    dplyr::select(count) %>%
    sum()

  last_week <- cranlogs::cran_downloads(package_name, "last-week") %>%
    dplyr::select(count) %>%
    sum()

  last_month <- cranlogs::cran_downloads(package_name, "last-month") %>%
    dplyr::select(count) %>%
    sum()

  overall <- cranlogs::cran_downloads(package_name, from = "2012-10-01", to = latest) %>%
    dplyr::select(count) %>%
    sum()

  tibble::tibble(
    latest = last_day,
    last_week = last_week,
    last_month = last_month,
    total = overall
  )


}


#' Check results
#'
#' Return latest CRAN build results.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_cran_results("dplyr")
#' }
#'
#' @export
#'
get_cran_results <- function(package_name) {

  check_cran(package_name)

  url <- glue::glue(
    "https://cran.r-project.org/web/checks/check_results_", repo_name, ".html"
  )

  xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    magrittr::extract2(1)

}


#' Title
#'
#' Retrieve the title of the package from CRAN.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_title("dplyr")
#' }
#'
#' @export
#'
get_cran_title <- function(package_name) {

  check_cran(package_name)

  url <- glue::glue("https://cran.r-project.org/package=", package_name)

  xml2::read_html(url) %>%
    rvest::html_nodes("h2") %>%
    rvest::html_text()

}

#' Description
#'
#' Retrieve the description of the package from CRAN.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_desc("dplyr")
#' }
#'
#' @export
#'
get_cran_desc <- function(package_name) {

  check_cran(package_name)

  url <- glue::glue("https://cran.r-project.org/package=", package_name)

  xml2::read_html(url) %>%
    rvest::html_nodes("p") %>%
    rvest::html_text() %>%
    magrittr::extract(1)

}


#' Version
#'
#' Retrieve the latest version of the package from CRAN.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_version("dplyr")
#' }
#'
#' @export
#'
get_cran_version <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "Version:") %>%
    magrittr::use_series(X2)

}


#' Dependency
#'
#' Retrieve the R version on which the package depends.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_r_dep("dplyr")
#' }
#'
#' @export
#'
get_cran_r_dep <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "Depends:") %>%
    magrittr::use_series(X2)

}

#' Imports
#'
#' Retrieve the list of packages imported.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_imports("dplyr")
#' }
#'
#' @export
#'
get_cran_imports <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "Imports:") %>%
    magrittr::use_series(X2) %>%
    stringr::str_split(pattern = ", ") %>%
    unlist() %>%
    tibble::tibble() %>%
    magrittr::set_colnames("imports")


}

#' Suggests
#'
#' Retrieve the list of packages suggested.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_suggests("dplyr")
#' }
#'
#' @export
#'
get_cran_suggests <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "Suggests:") %>%
    magrittr::use_series(X2) %>%
    stringr::str_split(pattern = ", ") %>%
    unlist() %>%
    tibble::tibble() %>%
    magrittr::set_colnames("suggests")


}

#' Published date
#'
#' Retrieve the latest date on which the package was published to CRAN.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_pub_date("dplyr")
#' }
#'
#' @export
#'
get_cran_pub_date <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "Published:") %>%
    magrittr::use_series(X2)

}

#' License
#'
#' Retrieve the license type of the package.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_license("dplyr")
#' }
#'
#' @export
#'
get_cran_license <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "License:") %>%
    magrittr::use_series(X2)

}

#' Authors
#'
#' Retrieve the list of authors of the package.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_authors("dplyr")
#' }
#'
#' @export
#'
get_cran_authors <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "Author:") %>%
    magrittr::use_series(X2) %>%
    stringr::str_split(",\n  ") %>%
    unlist() %>%
    tibble::tibble() %>%
    magrittr::set_colnames("name")

}

#' Maintainer
#'
#' Retrieve the details of the maintainer of the package.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_maintainer("dplyr")
#' }
#'
#' @export
#'
get_cran_maintainer <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "Maintainer:") %>%
    magrittr::use_series(X2)

}

#' URL
#'
#' Retrieve the list of URLs associated with the package.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_cran_urls("dplyr")
#' }
#'
#' @export
#'
get_cran_urls <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  bugs <-
    package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "BugReports:") %>%
    magrittr::use_series(X2) %>%
    magrittr::extract(1)

  website <-
    package_name %>%
    get_cran_table() %>%
    dplyr::filter(X1 == "URL:") %>%
    magrittr::use_series(X2) %>%
    stringr::str_split(pattern = ", ") %>%
    unlist()

  tibble::tibble(urls = c(bugs, website)) %>%
    dplyr::mutate(website = dplyr::case_when(
        stringr::str_detect(urls, pattern = "issues") ~ "Bugs",
        stringr::str_detect(urls, pattern = "github") ~ "GitHub",
        stringr::str_detect(urls, pattern = "gitlab") ~ "GitLab",
        stringr::str_detect(urls, pattern = "r-forge") ~ "R-Forge",
        TRUE ~ "Others"
      )
    ) %>%
    dplyr::select(website, urls)

}

get_cran_table <- function(package_name) {

  check_cran(package_name)

  url <- glue::glue(
    "https://cran.r-project.org/package=", package_name
  )

  xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    magrittr::extract2(1)

}


check_cran <- function(package_name) {
  
  if (curl::has_internet()) {
    
    url <- glue::glue("https://cran.r-project.org/package=", package_name)
    
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

