#' @title Launch shiny dashboard
#' @description Launches shiny dashboard.
#' @examples
#' \dontrun{
#' pkginfo_dashboard()
#' }
#' @export
#'
pkginfo_dashboard <- function() {
  shiny::runApp(appDir = system.file("application", package = "pkginfo"))
}

