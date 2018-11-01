#' GitHub info
#'
#' Displays GitHub related information of a package.
#'
#' @param user_name Name of the repository.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_github_info("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_github_info <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg    <- pkg_github(user_name, repo_name)
  stars  <- pkg$stargazers_count
  issues <- pkg$open_issues_count
  forks  <- pkg$forks_count

  tibble::tibble(
    stars  = stars,
    issues = issues,
    forks  = forks
  )

}

pkg_github <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue::glue("/repos/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url)
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

}

#' Branches
#'
#' Returns names of branches.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_branches("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_branches <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out <- connect_api("branches")
  purrr::map_chr(out, "name")

}

#' Issues
#'
#' Returns issues of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_issues("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_issues <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out            <- connect_api("issues")
  issue_date     <- as.Date(purrr::map_chr(out, "created_at"))
  issue_number   <- purrr::map_int(out, "number")
  issue_title    <- purrr::map_chr(out, "title")
  issue_assignee <- magrittr::extract2(out, 1)$assignee$login
  issue_body     <- purrr::map_chr(out, "body")

  tibble::tibble(
    date        = issue_date,
    number      = issue_number,
    title       = issue_title,
    assignee    = issue_assignee,
    description = issue_body
  )

}

#' Labels
#'
#' Returns labels of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_labels("tidyverse", "dplyr")
#' }
#'
#'
#' @export
#'
get_gh_labels <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out         <- connect_api("labels")
  label_name  <- purrr::map_chr(out, "name")
  label_color <- purrr::map_chr(out, "color")

  tibble::tibble(
    name  = label_name,
    color = label_color
  )

}

#' Milestones
#'
#' Returns milestones of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_milestones("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_milestones <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out      <- connect_api("milestones")
  m_title  <- purrr::map_chr(out, "title")
  m_body   <- purrr::map_chr(out, "description")
  m_open   <- purrr::map_int(out, "open_issues")
  m_closed <- purrr::map_int(out, "closed_issues")
  m_start  <- as.Date(purrr::map_chr(out, "created_at"))
  due      <- magrittr::extract2(out, 1)$due_on
  m_due    <- ifelse(is.null(due), NA, as.Date(due))

  tibble::tibble(
    title         = m_title,
    start_date    = m_start,
    due_date      = m_due,
    description   = m_body,
    open_issues   = m_open,
    closed_issues = m_closed
  )

}

#' Code of conduct
#'
#' Returns code of conduct of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_coc("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_coc <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  pkg_name <- glue::glue("/repos/", user_name, "/", repo_name, "/community/code_of_conduct")
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url, httr::add_headers(Accept = "application/vnd.github.scarlet-witch-preview+json"))
  out      <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  cat(out$body)

}


#' License
#'
#' Returns license of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_license("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_license <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

	out <- connect_api("license")
  if (length(out) == 2) {
    cat("This repository does not have a license file.")
  } else {
    if (pingr::is_online()) {
      utils::browseURL(out$html_url)
    } else {
      cat("Please ensure your internet connection is working.")
    }
  }

}

#' Pull requests
#'
#' Returns pull requests of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_pr("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_pr <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out         <- connect_api("pulls")
  pull_number <- purrr::map_int(out, "number")
  pull_start  <- as.Date(purrr::map_chr(out, "created_at"))
  pull_title  <- purrr::map_chr(out, "title")
  pull_status <- purrr::map_chr(out, "state")

  tibble::tibble(
    number = pull_number,
    date   = pull_start,
    title  = pull_title,
    status = pull_status
  )

}

#' Releases
#'
#' Returns releases of repository.
#'
#' @param user_name User or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_releases("tidyverse", "dplyr")
#' }
#'
#' @export
#'
get_gh_releases <- function(user_name, repo_name) {

	check_repo(user_name, repo_name)

  out               <- connect_api("releases")
  release_tag       <- purrr::map_chr(out, "tag_name")
  release_title     <- purrr::map_chr(out, "name")
  pre_release       <- purrr::map_lgl(out, "prerelease")
  release_published <- as.Date(purrr::map_chr(out, "published_at"))


  tibble::tibble(
    tag        = release_tag,
    date       = release_published,
    title      = release_title,
    prerelease = pre_release
  )

}

connect_api <- function(node) {

	pkg_name <- glue::glue("/repos/", user_name, "/", repo_name, "/", node)
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url)
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

}

