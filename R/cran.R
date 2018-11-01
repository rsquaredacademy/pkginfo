#' Retrieve package information
#'
#' @section Usage:
#' \preformatted{
#' myPackage <- CranPackage$new("package_name")
#' myPackage$get_downloads()
#' myPackage$get_check_results()
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
      get_downloads(self$package_name)
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
      get_check_results(self$package)
    }
  )
)
