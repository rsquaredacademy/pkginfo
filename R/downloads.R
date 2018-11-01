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
#' @importFrom cranlogs cran_downloads
#' @importFrom lubridate today
#' @importFrom dplyr select
#' @importFrom tibble tibble
#'
#' @export
#'
get_downloads <- function(package_name) {

  check_cran(package_name)
  count <- NULL

  latest <- today() - 2
  last_day <- cran_downloads(package_name, from = latest, to = latest) %>%
    select(count) %>%
    sum()

  last_week <- cran_downloads(package_name, "last-week") %>%
    select(count) %>%
    sum()

  last_month <- cran_downloads(package_name, "last-month") %>%
    select(count) %>%
    sum()

  overall <- cran_downloads(package_name, from = "2012-10-01", to = latest) %>%
    select(count) %>%
    sum()

  tibble(
    latest = last_day,
    last_week = last_week,
    last_month = last_month,
    total = overall
  )


}
