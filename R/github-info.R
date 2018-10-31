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

  tibble(
    stars  = stars,
    issues = issues,
    forks  = forks
  )

}

#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#'
pkg_github <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- glue("/repos/", user_name, "/", repo_name)
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  fromJSON(content(resp, "text"), simplifyVector = FALSE)

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
#' @importFrom purrr map_chr
#'
#' @export
#'
get_gh_branches <- function(user_name, repo_name) {

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/branches")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
  map_chr(out, "name")

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
#' @importFrom purrr map_int
#'
#' @export
#'
get_gh_issues <- function(user_name, repo_name) {

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/issues")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)

  issue_date   <- as.Date(map_chr(out, "created_at"))
  issue_number <- map_int(out, "number")
  issue_title  <- map_chr(out, "title")
  issue_assignee <- extract2(out, 1)$assignee$login
  issue_body <- map_chr(out, "body")

  tibble(
    date = issue_date,
    number = issue_number,
    title = issue_title,
    assignee = issue_assignee,
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

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/labels")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)

  label_name <- map_chr(out, "name")
  label_color <- map_chr(out, "color")

  tibble(
    name = label_name,
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

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/milestones")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)

  m_title  <- map_chr(out, "title")
  m_body   <- map_chr(out, "description")
  m_open   <- map_int(out, "open_issues")
  m_closed <- map_int(out, "closed_issues")
  m_start  <- as.Date(map_chr(out, "created_at"))
  due      <- extract2(out, 1)$due_on
  m_due    <- ifelse(is.null(due), NA, as.Date(due))

  tibble(
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

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/community/code_of_conduct")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url, add_headers(Accept = "application/vnd.github.scarlet-witch-preview+json"))
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)

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

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/license")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url, add_headers(Accept = "application/vnd.github.scarlet-witch-preview+json"))
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (length(out) == 2) {
    cat("This repository does not have a license file.")
  } else {
    if (pingr::is_online()) {
      browseURL(out$html_url)
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

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/pulls")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url, add_headers(Accept = "application/vnd.github.scarlet-witch-preview+json"))
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)

  pull_number <- map_int(out, "number")
  pull_start  <- as.Date(map_chr(out, "created_at"))
  pull_title  <- map_chr(out, "title")
  pull_status <- map_chr(out, "state")

  tibble(
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

  pkg_name <- glue("/repos/", user_name, "/", repo_name, "/releases")
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  out      <- fromJSON(content(resp, "text"), simplifyVector = FALSE)

  release_tag <- map_chr(out, "tag_name")
  release_title <- map_chr(out, "name")
  pre_release <- map_lgl(out, "prerelease")
  release_published <- as.Date(map_chr(out, "published_at"))


  tibble(
    tag = release_tag,
    date = release_published,
    title = release_title,
    prerelease = pre_release
  )

}

