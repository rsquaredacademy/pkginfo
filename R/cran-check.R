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
