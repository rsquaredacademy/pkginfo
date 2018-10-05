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
#' @importFrom rvest html_text
#'
#' @export
#'
get_cran_title <- function(package_name) {

  check_cran()

  url <- glue("https://cran.r-project.org/package=", package_name)

  read_html(url) %>%
    html_nodes("h2") %>%
    html_text()

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

  check_cran()

  url <- glue("https://cran.r-project.org/package=", package_name)

  read_html(url) %>%
    html_nodes("p") %>%
    html_text() %>%
    extract(1)

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
#' @importFrom dplyr filter
#'
#' @export
#'
get_cran_version <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    filter(X1 == "Version:") %>%
    use_series(X2)

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
    filter(X1 == "Depends:") %>%
    use_series(X2)

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
#' @importFrom stringr str_split
#' @importFrom magrittr set_colnames
#'
#' @export
#'
get_cran_imports <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  package_name %>%
    get_cran_table() %>%
    filter(X1 == "Imports:") %>%
    use_series(X2) %>%
    str_split(pattern = ", ") %>%
    unlist() %>%
    tibble() %>%
    set_colnames("imports")


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
    filter(X1 == "Suggests:") %>%
    use_series(X2) %>%
    str_split(pattern = ", ") %>%
    unlist() %>%
    tibble() %>%
    set_colnames("suggests")


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
    filter(X1 == "Published:") %>%
    use_series(X2)

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
    filter(X1 == "License:") %>%
    use_series(X2)

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
    filter(X1 == "Author:") %>%
    use_series(X2) %>%
    str_split(",\n  ") %>%
    unlist() %>%
    tibble() %>%
    set_colnames("name")

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
    filter(X1 == "Maintainer:") %>%
    use_series(X2)

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
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_detect
#'
#' @export
#'
get_cran_urls <- function(package_name) {

  X1 <- NULL
  X2 <- NULL

  bugs <-
    package_name %>%
    get_cran_table() %>%
    filter(X1 == "BugReports:") %>%
    use_series(X2) %>%
    extract(1)

  website <-
    package_name %>%
    get_cran_table() %>%
    filter(X1 == "URL:") %>%
    use_series(X2) %>%
    str_split(pattern = ", ") %>%
    unlist()

  tibble(urls = c(bugs, website)) %>%
    mutate(website = case_when(
        str_detect(urls, pattern = "issues") ~ "Bugs",
        str_detect(urls, pattern = "github") ~ "GitHub",
        str_detect(urls, pattern = "gitlab") ~ "GitLab",
        str_detect(urls, pattern = "r-forge") ~ "R-Forge",
        TRUE ~ "Others"
      )
    ) %>%
    select(website, urls)

}

get_cran_table <- function(package_name) {

  check_cran()

  url <- glue(
    "https://cran.r-project.org/package=", package_name
  )

  read_html(url) %>%
    html_nodes("table") %>%
    html_table() %>%
    extract2(1)

}


check_cran <- function(package_name) {
  
  if (has_internet()) {
    
    url <- glue("https://cran.r-project.org/package=", package_name)
    
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