#' Retrieve GitHub repository information
#'
#' @section Usage:
#' \preformatted{
#' myRepo <- GitHubRepo$new("repo_name", "user_name")
#' myRepo$get_stats()
#' myRepo$get_branches()
#' myRepo$get_issues()
#' myRepo$get_labels()
#' myRepo$get_milestones()
#' myRepo$get_coc()
#' myRepo$get_license()
#' myRepo$get_pull_requests()
#' myRepo$get_releases()
#' myRepo$get_travis_status()
#' myRepo$get_appveyor_status()
#' myRepo$get_coverage()
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{user_name}{GitHub user or organization name.}
#'   \item{repo_name}{Name of the GitHub repository.}
#' }
#'
#' @section Details:
#'
#' To create \code{GitHubRepo} objects, you need to use \code{GitHubRepo$new("repo_name", "user_name")}.
#'
#' \code{myRepo$get_stats()} will return the number of stars, forks and issues of the package.
#'
#' \code{myRepo$get_branches()} will return the name of the branches of the package.
#'
#' \code{myRepo$get_issues()} will return the list of open issues.
#'
#' \code{myRepo$get_labels()} will return the name and color labels used in issues filed in the package.
#'
#' \code{myRepo$get_milestones()} will return the details of milestones associated with the package.
#'
#' \code{myRepo$get_coc()} will return the code of conduct of the package.
#'
#' \code{myRepo$get_license()} will return license of the package.
#'
#' \code{myRepo$get_pull_requests()} will return all the open pull requests.
#'
#' \code{myRepo$get_releases()} will return all the releases of the package on GitHub.
#'
#' \code{myRepo$get_travis_status()} will return the build status of the package from Travis CI.
#'
#' \code{myRepo$get_appveyor_status()} will return the build status of the package from Appveyor.
#'
#' \code{myRepo$get_coverage()} will return code coverage of the package from Codecov.
#'
#' @examples
#' \dontrun{
#' myRepo <- GitHubRepo$new("dplyr", "tidyverse")
#' myRepo$get_stats()
#' myRepo$get_branches()
#' myRepo$get_travis_status()
#' myRepo$get_coverage()
#' }
#'
#' @name GitHubRepo
#' @docType class
#' @format An R6 class.
#' @export
#'
NULL

GitHubRepo <- R6::R6Class("GitHubRepo",
  public = list(
    repo_name = NULL,
    user_name = NULL,
    initialize = function(repo_name = NA, user_name = NULL) {
      self$user_name <- user_name
      self$repo_name <- repo_name
    },
    get_stats = function() {
      get_gh_stats(self$repo_name, self$user_name)
    },
    get_branches = function() {
      get_gh_branches(self$repo_name, self$user_name)
    },
    get_issues = function() {
      get_gh_issues(self$repo_name, self$user_name)
    },
    get_labels = function() {
      get_gh_labels(self$repo_name, self$user_name)
    },
    get_milestones = function() {
      get_gh_milestones(self$repo_name, self$user_name)
    },
    get_coc = function() {
      get_gh_coc(self$repo_name, self$user_name)
    },
    get_license = function() {
      get_gh_license(self$repo_name, self$user_name)
    },
    get_pull_requests = function() {
      get_gh_pr(self$repo_name, self$user_name)
    },
    get_releases = function() {
      get_gh_releases(self$repo_name, self$user_name)
    },
    get_travis_status = function() {
      get_status_travis(self$repo_name, self$user_name)
    },
    get_appveyor_status = function() {
      get_status_appveyor(self$repo_name, self$user_name)
    },
    get_coverage = function() {
      get_code_coverage(self$repo_name, self$user_name)
    }
  )
)


#' Stars, forks and issues
#'
#' Returns number of stars, forks and open issues of a GitHub repository.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_gh_stats("dplyr", "tidyverse")
#' }
#'
#' @export
#'
get_gh_stats <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
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

}

pkg_github <- function(user_name, repo_name) {

  check_repo(user_name, repo_name)

  pkg_name <- paste0("/repos/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url)
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

}

#' Branches
#'
#' Returns names of branches.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_branches("dplyr", "tidyverse")
#' }
#'
#' @export
#'
get_gh_branches <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  out <- connect_api(user_name, repo_name, "branches")
	  tibble::tibble(branches = purrr::map_chr(out, "name"))
	}

}

#' Issues
#'
#' Returns issues of repository.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_issues("dplyr", "tidyverse")
#' }
#'
#' @export
#'
get_gh_issues <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  out            <- connect_api(user_name, repo_name, "issues")
	  issue_number   <- purrr::map_int(out, "number")
	  issue_title    <- purrr::map_chr(out, "title")
	  issue_body     <- purrr::map_chr(out, "body")

	  issue_date <-
	    out %>%
	    purrr::map_chr("created_at") %>%
	    as.Date()

	  issue_user <-
	    out %>%
		  purrr::map("user") %>%
		  purrr::map_chr("login")


	  tibble::tibble(
	    date        = issue_date,
	    number      = issue_number,
	    author      = issue_user,
	    title       = issue_title
	  )	
	}

}

#' Labels
#'
#' Returns labels of repository.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_labels("dplyr", "tidyverse")
#' }
#'
#'
#' @export
#'
get_gh_labels <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  out         <- connect_api(user_name, repo_name, "labels")
	  label_name  <- purrr::map_chr(out, "name")
	  label_color <- purrr::map_chr(out, "color")

	  tibble::tibble(
	    name  = label_name,
	    color = label_color
	  )	
	}

}

#' Milestones
#'
#' Returns milestones of repository.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_milestones("dplyr", "tidyverse")
#' }
#'
#' @export
#'
get_gh_milestones <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  out      <- connect_api(user_name, repo_name, "milestones")
	  m_title  <- purrr::map_chr(out, "title")
	  m_body   <- purrr::map_chr(out, "description")
	  m_open   <- purrr::map_int(out, "open_issues")
	  m_closed <- purrr::map_int(out, "closed_issues")
	  m_start  <- as.Date(purrr::map_chr(out, "created_at"))
	  due      <- magrittr::extract2(out, 1)$due_on
	  m_due    <- ifelse(is.null(due), NA, as.Date(due))

	  tibble::tibble(
	    title         = m_title,
	    tart_date    = m_start,
	    due_date      = m_due,
	    description   = m_body,
	    open_issues   = m_open,
	    closed_issues = m_closed
	  )	
	}

}

#' Code of conduct
#'
#' Returns code of conduct of repository.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_coc("dplyr", "tidyverse")
#' }
#'
#' @export
#'
get_gh_coc <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  pkg_name <- paste0("/repos/", user_name, "/", repo_name, "/community/code_of_conduct")
	  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
	  resp     <- httr::GET(url, httr::add_headers(Accept = "application/vnd.github.scarlet-witch-preview+json"))
	  out      <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

	  cat(out$body)	
	}

}


#' License
#'
#' Returns license of repository.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_license("dplyr", "tidyverse")
#' }
#'
#' @export
#'
get_gh_license <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

		out <- connect_api(user_name, repo_name, "license")
	  if (length(out) == 2) {
	    cat("This repository does not have a license file.")
	  } else {
	    if (curl::has_internet()) {
	      utils::browseURL(out$html_url)
	    } else {
	      cat("Please ensure your internet connection is working.")
	    }
	  }	
	}

}

#' Pull requests
#'
#' Returns pull requests of repository.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_pr("dplyr", "tidyverse")
#' }
#'
#' @export
#'
get_gh_pr <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  out         <- connect_api(user_name, repo_name, "pulls")
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

}

#' Releases
#'
#' Returns releases of repository.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the repository.
#'
#' @examples
#' \dontrun{
#' get_gh_releases("dplyr", "tidyverse")
#' }
#'
#' @export
#'
get_gh_releases <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  out               <- connect_api(user_name, repo_name, "releases")
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

}

#' Travis build status
#'
#' Return the latest travis build status.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_status_travis("dplyr", "rsquaredacademy")
#' }
#'
#' @export
#'
get_status_travis <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  pkg_name <- paste0("repos/", user_name, "/", repo_name)
	  url      <- httr::modify_url("https://api.travis-ci.org", path = pkg_name)
	  resp     <- httr::GET(url)

	  httr::content(resp, "parsed") %>%
	    xml2::as_list() %>%
	    magrittr::use_series('Projects') %>%
	    magrittr::use_series('Project') %>%
	    attributes() %>%
	    magrittr::use_series('lastBuildStatus') %>%
	    magrittr::extract(1)
		}

}

#' Appveyor build status
#'
#' Return the latest appveyor build status.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_status_appveyor("dplyr", "rsquaredacademy")
#' }
#'
#' @export
#'
get_status_appveyor <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

  check_repo(user_name, repo_name)

  pkg_name <- paste0("/api/projects/", user_name, "/", repo_name)
  url      <- httr::modify_url("https://ci.appveyor.com", path = pkg_name)
  resp     <- httr::GET(url)
  result   <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  result$build$status

}

#' Code coverage
#'
#' Return the code coverage of the package.
#'
#' @param user_name GitHub user or organization name.
#' @param repo_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_code_coverage("dplyr", "rsquaredacademy")
#' }
#'
#' @export
#'
get_code_coverage <- function(repo_name, user_name = NULL) {

	if (is.null(user_name)) {
		user_name <- get_gh_username(repo_name)
	}

	if (is.null(user_name)) {
		NULL
	} else {
		check_repo(user_name, repo_name)

	  pkg_name <- paste0("/api/gh/", user_name, "/", repo_name)
	  url      <- httr::modify_url("https://codecov.io", path = pkg_name)
	  resp     <- httr::GET(url)
	  result   <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
	  result$commit$totals$c
	}

}

connect_api <- function(user_name, repo_name, node) {

	pkg_name <- paste0("/repos/", user_name, "/", repo_name, "/", node)
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url)
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

}


check_repo <- function(user_name, repo_name) {

  if (curl::has_internet()) {

    repo_url <- paste0("https://github.com/", user_name)

    repo_status <-
      repo_url %>%
      httr::GET() %>%
      httr::status_code()

    if (repo_status != 200) {
      stop("Please check the repository name.", call. = FALSE)
    }

    pkg_url <- paste0("https://github.com/", user_name, "/", repo_name)

    pkg_status <-
      pkg_url %>%
      httr::GET() %>%
      httr::status_code()

    if (pkg_status != 200) {
      stop("Please check the package name.", call. = FALSE)
    }

  } else {
    stop("Please check your internet connection.", call. = FALSE)
  }

}

#' GitHub username
#'
#' Returns the GitHub user or organization name.
#'
#' @param package_name Name of the package.
#'
#' @examples
#' \dontrun{
#' get_gh_username("olsrr")
#' }
#'
#' @export
#'
get_gh_username <- function(package_name) {

	check_cran(package_name)

  all_urls <-
    package_name %>%
    get_pkg_details() %>%
    get_pkg_urls()

  check_git <-
    all_urls %>%
    dplyr::filter(stringr::str_detect(urls, "github")) %>%
    nrow()

  if (check_git > 0) {
    
    urls <-
      all_urls %>%
      dplyr::filter(stringr::str_detect(urls, "github")) %>%
      dplyr::pull(urls) %>%
      dplyr::first()

  	locate_slash <- stringr::str_locate_all(urls, "/")

		start <-
		  locate_slash %>%
		  get_location(position = 3) %>%
		  magrittr::add(1)

		end <-
		  locate_slash %>%
		  get_location(position = 4) %>%
		  magrittr::subtract(1)

		stringr::str_sub(urls, start, end)
  } else {
    message("There is no associated GitHub repository for this package.")
  }

}


get_location <- function(string, position) {
  string %>%
    magrittr::extract2(1) %>%
    magrittr::extract(position, 1) %>%
    unname()
}
