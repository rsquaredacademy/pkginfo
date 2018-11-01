#' Downloads
#'
#' Package downloads from cranlogs.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_downloads("dplyr")
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
get_downloads <- function(package_name) {

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
