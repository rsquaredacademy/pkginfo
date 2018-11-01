#' \code{pkginfo} package
#'
#' Retrieve pakcage information.
#'
#' See the README on
#' \href{https://github.com/rsquaredacademy/pkginfo}{GitHub}
#'
#' @docType package
#' @name pkginfo
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "repo_name", "urls", "user_name"))
}
