#' Retrieve title
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

#' Retrieve description
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


#' Retrieve version
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


#' Retrieve dependency
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

#' Retrieve imports
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

#' Retrieve suggests
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

#' Retrieve published date
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

#' Retrieve license
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

#' Retrieve authors
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

#' Retrieve maintainer
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

#' Retrieve url
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