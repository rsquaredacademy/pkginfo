% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info-cran.R
\name{package_info}
\alias{package_info}
\alias{get_pkg_title}
\alias{get_pkg_desc}
\alias{get_pkg_version}
\alias{get_pkg_r_dep}
\alias{get_pkg_imports}
\alias{get_pkg_suggests}
\alias{get_pkg_publish_date}
\alias{get_pkg_license}
\alias{get_pkg_authors}
\alias{get_pkg_maintainer}
\alias{get_pkg_urls}
\title{CRAN package details}
\usage{
get_pkg_title(pkg_details)

get_pkg_desc(pkg_details)

get_pkg_version(pkg_details)

get_pkg_r_dep(pkg_details)

get_pkg_imports(pkg_details)

get_pkg_suggests(pkg_details)

get_pkg_publish_date(pkg_details)

get_pkg_license(pkg_details)

get_pkg_authors(pkg_details)

get_pkg_maintainer(pkg_details)

get_pkg_urls(pkg_details)
}
\arguments{
\item{pkg_details}{An object of class \code{pkg_details}.}
}
\description{
Extracts and formats package details such as title, description, version,
R version dependency, imports, suggests, published date, license, authors,
maintainer and associated urls from crandb API.
}
\examples{
\dontrun{
# retrieve package details from crandb api
pkgdetails <- get_pkg_details("dplyr")

# package title
get_pkg_title(pkgdetails)

# package description
get_pkg_desc(pkgdetails)

# package version
get_pkg_version(pkgdetails)

# R dependency
get_pkg_r_dep(pkgdetails)

# packages imported
get_pkg_imports(pkgdetails)

# packages suggested
get_pkg_suggests(pkgdetails)

# latest CRAN publish date
get_pkg_publish_date(pkgdetails)

# package license
get_pkg_license(pkgdetails)

# package authors
get_pkg_authors(pkgdetails)

# package maintainer
get_pkg_maintainer(pkgdetails)

# urls associated with the package
get_pkg_urls(pkgdetails)

}
}
