#' Retrieve package information
#'
#' @section Usage:
#' \preformatted{
#' myPackage <- CranPackage$new("package_name")
#' myPackage$get_downloads()
#' myPackage$get_cran_check_results()
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
#' \code{myPackage$get_cran_check_results()} will return the CRAN check results of
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
#' # initialize object
#' myPackage <- CranPackage$new("dplyr")
#'
#' # package title
#' myPackage$get_title()
#'
#' # package version on CRAN
#' myPackage$get_version()
#'
#' # R dependency version
#' myPackage$get_r_dep()
#'
#' # packages imported
#' myPackage$get_imports()
#'
#' }
#' @name CranPackage
#' @docType class
#' @format An R6 class.
#' @export
#'
NULL

CranPackage <- R6::R6Class("CranPackage",
                           public = list(
                             package_name = NULL,
                             pkg_details  = NULL,
                             initialize = function(package_name = NA) {
                               self$package_name <- package_name
                               self$pkg_details  <- get_pkg_details(self$package_name)
                             },
                             get_downloads = function() {
                               get_pkg_downloads(self$package_name)
                             },
                             get_title = function() {
                               get_pkg_title(self$pkg_details)
                             },
                             get_description = function() {
                               get_pkg_desc(self$pkg_details)
                             },
                             get_version = function() {
                               get_pkg_version(self$pkg_details)
                             },
                             get_r_dep = function() {
                               get_pkg_r_dep(self$pkg_details)
                             },
                             get_imports = function() {
                               get_pkg_imports(self$pkg_details)
                             },
                             get_suggests = function() {
                               get_pkg_suggests(self$pkg_details)
                             },
                             get_publish_date = function() {
                               get_pkg_publish_date(self$pkg_details)
                             },
                             get_license = function() {
                               get_pkg_license(self$pkg_details)
                             },
                             get_authors = function() {
                               get_pkg_authors(self$pkg_details)
                             },
                             get_maintainer = function() {
                               get_pkg_maintainer(self$pkg_details)
                             },
                             get_urls = function() {
                               get_pkg_urls(self$pkg_details)
                             },
                             get_cran_check_results = function() {
                               get_pkg_cran_check_results(self$package_name)
                             }
                           )
)


#' Package details
#'
#' Extracts package details from crandb API.
#'
#' @param package_name Name of the R package.
#'
#' @examples
#' \dontrun{
#' get_pkg_details("dplyr")
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
get_pkg_details <- function(package_name) {

  check_cran(package_name)

  base_url    <- "http://crandb.r-pkg.org"
  pkg_url     <- httr::modify_url(base_url, path = package_name)
  resp        <- httr::GET(pkg_url)
  resp_status <- httr::status_code(resp)

  if (resp_status == 200) {
    resp %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = TRUE)
  } else {
    stop("Please check the package name.")
  }

}

#' Package downloads
#'
#' Package downloads from RStudio CRAN mirror.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' \dontrun{
#' # last day, week, month and total downloads
#' get_pkg_downloads("dplyr")
#'
#' }
#' @export
#'
get_pkg_downloads <- function(package_name) {

  check_cran(package_name)
  count <- NULL

  latest <- lubridate::today() - 2
  last_day <-
    cranlogs::cran_downloads(package_name, from = latest, to = latest) %>%
    dplyr::select(count) %>%
    sum()

  last_week <-
    cranlogs::cran_downloads(package_name, "last-week") %>%
    dplyr::select(count) %>%
    sum()

  last_month <-
    cranlogs::cran_downloads(package_name, "last-month") %>%
    dplyr::select(count) %>%
    sum()

  overall <-
    cranlogs::cran_downloads(package_name, from = "2012-10-01", to = latest) %>%
    dplyr::select(count) %>%
    sum()

  tibble::tibble(
    latest = last_day,
    last_week = last_week,
    last_month = last_month,
    total = overall
  )


}


#' CRAN check results
#'
#' Return latest CRAN check results.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' \dontrun{
#' # CRAN check results
#' get_pkg_cran_check_results("dplyr")
#'
#' }
#' @export
#'
get_pkg_cran_check_results <- function(package_name) {

  check_cran(package_name)

  base_url <- 'https://cloud.r-project.org/web/checks/'
  url <- paste0(base_url, 'check_results_', package_name, '.html')
  html <- rvest::read_html(url)

  pkg_checks <-
    html %>%
    rvest::html_element("table") %>%
    rvest::html_table()

  split_flavor <-
    pkg_checks %>%
    dplyr::pull(Flavor) %>%
    stringr::str_split(pattern = "-", n = 3)

  tibble::tibble(
    os     = purrr::map_chr(split_flavor, 3),
    r      = purrr::map_chr(split_flavor, 2),
    status = dplyr::pull(pkg_checks, Status)
  ) %>%
    dplyr::arrange(os)

}

#' CRAN package details
#'
#' Extracts and formats package details such as title, description, version,
#' R version dependency, imports, suggests, published date, license, authors,
#' maintainer and associated urls from crandb API.
#'
#' @param pkg_details An object of class \code{pkg_details}.
#'
#' @examples
#' \dontrun{
#' # retrieve package details from crandb api
#' pkgdetails <- get_pkg_details("dplyr")
#'
#' # package title
#' get_pkg_title(pkgdetails)
#'
#' # package description
#' get_pkg_desc(pkgdetails)
#'
#' # package version
#' get_pkg_version(pkgdetails)
#'
#' # R dependency
#' get_pkg_r_dep(pkgdetails)
#'
#' # packages imported
#' get_pkg_imports(pkgdetails)
#'
#' # packages suggested
#' get_pkg_suggests(pkgdetails)
#'
#' # latest CRAN publish date
#' get_pkg_publish_date(pkgdetails)
#'
#' # package license
#' get_pkg_license(pkgdetails)
#'
#' # package authors
#' get_pkg_authors(pkgdetails)
#'
#' # package maintainer
#' get_pkg_maintainer(pkgdetails)
#'
#' # urls associated with the package
#' get_pkg_urls(pkgdetails)
#'
#' }
#' @name package_info
NULL

#' @rdname package_info
#' @export
#'
get_pkg_title <- function(pkg_details) {
  magrittr::use_series(pkg_details, Title)
}

#' @rdname package_info
#' @export
#'
get_pkg_desc <- function(pkg_details) {
  magrittr::use_series(pkg_details, Description)
}

#' @rdname package_info
#' @export
#'
get_pkg_version <- function(pkg_details) {
  magrittr::use_series(pkg_details, Version)
}


#' @rdname package_info
#' @export
#'
get_pkg_r_dep <- function(pkg_details) {

  pkg_details %>%
    magrittr::use_series(Depends) %>%
    magrittr::use_series(R)

}

#' @rdname package_info
#' @export
#'
get_pkg_imports <- function(pkg_details) {

  pkg_details %>%
    magrittr::use_series(Imports) %>%
    unlist() %>%
    names()

}

#' @rdname package_info
#' @export
#'
get_pkg_suggests <- function(pkg_details) {

  pkg_details %>%
    magrittr::use_series(Suggests) %>%
    unlist() %>%
    names()
}

#' @rdname package_info
#' @export
#'
get_pkg_publish_date <- function(pkg_details) {
  pkg_details %>%
    magrittr::use_series("Date/Publication") %>%
    lubridate::date()
}

#' @rdname package_info
#' @export
#'
get_pkg_license <- function(pkg_details) {
  magrittr::use_series(pkg_details, License)
}

#' @rdname package_info
#' @export
#'
get_pkg_authors <- function(pkg_details) {

  pkg_details %>%
    magrittr::use_series(Author) %>%
    stringr::str_split(",\n") %>%
    unlist() %>%
    tibble::tibble() %>%
    magrittr::set_colnames("author_details") %>%
    dplyr::mutate(
      author = stringr::str_split(author_details, pattern = "\\[") %>%
        purrr::map_chr(1) %>%
        stringr::str_trim(side = "right"),
      role = stringr::str_split(author_details, pattern = "\\[") %>%
        purrr::map_chr(2) %>%
        stringr::str_split(pattern = "\\]") %>%
        purrr::map_chr(1)

    ) %>%
    dplyr::select(author, role)
}

#' @rdname package_info
#' @export
#'
get_pkg_maintainer <- function(pkg_details) {

  details <-
    pkg_details %>%
    magrittr::use_series(Maintainer) %>%
    stringr::str_replace(pattern = ">", replacement = "") %>%
    stringr::str_trim() %>%
    stringr::str_split(pattern = "<") %>%
    unlist()

  tibble::tibble(
    name  = details[1],
    email = details[2]
  )

}

#' @rdname package_info
#' @export
#'
get_pkg_urls <- function(pkg_details) {

  bugs <- magrittr::use_series(pkg_details, BugReports)
  docs <- magrittr::use_series(pkg_details, URL)

  detect_sep <-
    pkg_details %>%
    magrittr::use_series(URL) %>%
    stringr::str_detect("\n")

  if (detect_sep) {
    docs_site <- stringr::str_split(docs, pattern = ",\n")
  } else {
    docs_site <- stringr::str_split(docs, pattern = ",")
  }

  docu <-
    docs_site %>%
    unlist() %>%
    stringr::str_trim()

  tibble::tibble(urls = c(bugs, docu)) %>%
    dplyr::mutate(website = dplyr::case_when(
      stringr::str_detect(urls, pattern = "issues") ~ "Bugs",
      stringr::str_detect(urls, pattern = "github") ~ "GitHub",
      stringr::str_detect(urls, pattern = "gitlab") ~ "GitLab",
      stringr::str_detect(urls, pattern = "r-forge") ~ "R-Forge",
      TRUE ~ "Others")
    ) %>%
    dplyr::select(website, urls)

}


check_cran <- function(package_name) {

  if (curl::has_internet()) {

    url <- paste0("https://cran.r-project.org/package=", package_name)

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

check_package_name <- function(package_name) {

  if (curl::has_internet()) {

    url <- paste0("https://cran.r-project.org/package=", package_name)

    status <-
      url %>%
      httr::GET() %>%
      httr::status_code()

    if (status != 200) {
      FALSE
    } else {
      TRUE
    }

  } else {
    stop("Please check your internet connection.", call. = FALSE)
  }

}
