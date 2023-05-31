#' @title Launch shiny dashboard
#' @description Launches shiny dashboard.
#' @examples
#' \dontrun{
#' pkginfo_dashboard()
#' }
#' @export
#'
pkginfo_dashboard <- function() {
  kview <- options(kableExtra_view_html = FALSE)
  on.exit(options(kview))
  shiny::runApp(appDir = system.file("application", package = "pkginfo"))
}

