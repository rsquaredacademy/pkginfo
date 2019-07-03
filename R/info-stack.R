#' Stack Overflow questions
#'
#' Display package related questions on Stack Overflow.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' # get stack overflow questions
#' get_so_questions("dplyr")
#'
#' @export
#'
get_so_questions <- function(package_name) {

	url <- paste0("https://api.stackexchange.com/2.2/search?order=desc&sort=activity&tagged=",
		package_name, "&site=stackoverflow")

	resp <- httr::GET(url)
	out  <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

	date_creation <-
	  purrr::map_int(out$items, "creation_date") %>%
	  as.POSIXct(origin = "1970-01-01", tz = "UTC") %>%
	  as.Date()

	title    <- purrr::map_chr(out$items, "title")
	answered <- purrr::map_lgl(out$items, "is_answered")
	views    <- purrr::map_int(out$items, "view_count")
	owner    <- purrr::map_chr(purrr::map(out$items, "owner"), "display_name")
	qlink    <- purrr::map_chr(out$items, "link")

	tibble::tibble(
		date     = date_creation,
		title    = title,
		owner    = owner,
		answered = answered,
		views    = views,
		link     = qlink
	)

}
