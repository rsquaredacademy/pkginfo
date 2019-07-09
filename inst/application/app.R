library(magrittr)

ui <- shinydashboard::dashboardPage(skin = "blue",
	shinydashboard::dashboardHeader(title = "pkginfo"),
	shinydashboard::dashboardSidebar(
		shinydashboard::sidebarMenu(
			id = "tabs",
			shinydashboard::menuItem("Welcome", tabName = "welcome", icon = shiny::icon("th")),
			shinydashboard::menuItem("Overview", tabName = "basic_info", icon = shiny::icon("th")),
			shinydashboard::menuItem("Downloads", tabName = "downloads", icon = shiny::icon("th")),
			shinydashboard::menuItem("Indicators", tabName = "build_status", icon = shiny::icon("th")),
			shinydashboard::menuItem("CRAN Check", tabName = "cran_check", icon = shiny::icon("th")),
			shinydashboard::menuItem("Issues", tabName = "issues", icon = shiny::icon("th")),
			shinydashboard::menuItem("Releases", tabName = "releases", icon = shiny::icon("th")),
			shinydashboard::menuItem("Branches", tabName = "branches", icon = shiny::icon("th")),
			shinydashboard::menuItem("Dependencies", tabName = "deps", icon = shiny::icon("th")),
			shinydashboard::menuItem("Pull Requests", tabName = "pr", icon = shiny::icon("th")),
			shinydashboard::menuItem("Stack Overflow", tabName = "so", icon = shiny::icon("th")),
			shinydashboard::menuItem("Exit", tabName = "exit", icon = shiny::icon("power-off"))
			)
		),
	shinydashboard::dashboardBody(
		shinydashboard::tabItems(
			shinydashboard::tabItem(tabName = "welcome",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::br(),
						shiny::textInput("repo_name", "Package/Repo Name", value = NULL),
						shiny::actionButton("check_package_name", "Verify Package Name"),
						shiny::br(),
						shiny::br()
						)
					),
				shiny::fluidRow(
					shiny::uiOutput("package_check")
					)
				),
			shinydashboard::tabItem(tabName = "basic_info",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("Overview")
					)
				),
				shiny::br(),
				shiny::br(),
				shiny::fluidRow(
					shiny::uiOutput('out_basic_title') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_desc') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_version') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_pub') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_maintainer') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_maintainer_email') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_cran') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_bugs') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_github') %>%
						shinycssloaders::withSpinner(),
					shiny::uiOutput('out_basic_website') %>%
						shinycssloaders::withSpinner()
				)
			),
			shinydashboard::tabItem(tabName = "downloads",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("CRAN Downloads"),
						shiny::br(),
						shiny::tableOutput("cran_downloads") %>%
						shinycssloaders::withSpinner()
						)
					),
				shiny::fluidRow(
					shiny::column(6, align = 'right',
						shiny::dateInput("start_date", "From")
						),
					shiny::column(6, align = 'left',
						shiny::dateInput("end_date", "To")
						)
					),
				shiny::fluidRow(
					column(2),
					column(8, align = 'center',
						plotOutput("downloads_plot") %>%
						shinycssloaders::withSpinner()
						),
					column(2)
					),
				shiny::br(),
				shiny::br(),
				shiny::br()
				),
			shinydashboard::tabItem(tabName = "build_status",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("Indicators")
						)
					),
				shiny::fluidRow(
					shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("travisBox")),
					shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("appveyorBox")),
					shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("coverageBox"))
					),
				shiny::fluidRow(
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("starsBox")),
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("forksBox")),
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("issuesBox"))
					),

				shiny::fluidRow(
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("importsBox")),
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("suggestsBox")),
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("prBox"))
					),
				shiny::fluidRow(
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("branchesBox")),
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("releasesBox")),
					shinycssloaders::withSpinner(shinydashboard::valueBoxOutput("versionBox"))
					)
				),
			shinydashboard::tabItem(tabName = "cran_check",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("CRAN Check Results"),
						shiny::br(),
						shiny::tableOutput("cran_check_results_table") %>%
						shinycssloaders::withSpinner()
						)
					)
				),
			shinydashboard::tabItem(tabName = "issues",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("Open Issues"),
						shiny::br(),
						shiny::tableOutput("gh_issues") %>%
						shinycssloaders::withSpinner()
						)
					)
				),
			shinydashboard::tabItem(tabName = "releases",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("Releases"),
						shiny::br(),
						shiny::tableOutput("gh_releases") %>%
						shinycssloaders::withSpinner()
						)
					)
				),
			shinydashboard::tabItem(tabName = "branches",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("GitHub Branches"),
						shiny::br(),
						shiny::tableOutput("gh_branches") %>%
						shinycssloaders::withSpinner()
						)
					)
				),
			shinydashboard::tabItem(tabName = "deps",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("Dependencies")
						)
					),
				shiny::fluidRow(
					shiny::column(6, align = 'center',
						shiny::tableOutput("cran_imports") %>%
						shinycssloaders::withSpinner()
						),
					shiny::column(6, align = 'center',
						shiny::tableOutput("cran_suggests") %>%
						shinycssloaders::withSpinner()
						)
					)
				),
			shinydashboard::tabItem(tabName = "pr",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("Open Pull Requests"),
						shiny::br(),
						shiny::tableOutput("gh_prs") %>%
						shinycssloaders::withSpinner()
						)
					)
				),
			shinydashboard::tabItem(tabName = "so",
				shiny::fluidRow(
					shiny::column(12, align = 'center',
						shiny::h2("Stack Overflow"),
						shiny::br(),
						shiny::tableOutput("gh_so") %>%
						shinycssloaders::withSpinner()
						)
					)
				),
			shinydashboard::tabItem(tabName = "exit",
				shiny::fluidRow(shiny::column(12, align = 'center',
				  shiny::h2("Thank you for using", shiny::strong("pkginfo"), "!"))),
				shiny::fluidRow(shiny::column(12, align = 'center',
				  shiny::actionButton("exit_button", "Exit App")))
				)
			)
)
)

server <- function(input, output, session) {

	valid_package <- shiny::eventReactive(input$check_package_name, {
		pkginfo:::check_package_name(input$repo_name)
	})

	output$package_check <- renderUI({
		if (valid_package()) {
			shiny::column(12, align = 'center',
						shiny::textInput("user_name", "GitHub Owner/Org", value = NULL),
						shiny::br(),
						shiny::actionButton(inputId = "check_repo_name", label = "Find User/Org"),
						shiny::actionButton("retrieve_info", "Retrieve Info"),
						shiny::br(),
						shiny::br(),
						shiny::column(3),
						shiny::column(6, align = 'center',
							shiny::h4("Click on the Find User/Org button if you do not know the GitHub username or
								organization name. The app will find it if the package has a GitHub repository.")
							),
						shiny::column(3)
						)
		} else {
			shiny::column(12, align = 'center',
				shiny::h5("You have entered an invalid package name.")
			)
		}
	})

  pkg_details <-
    shiny::eventReactive(input$retrieve_info, {
      pkginfo::get_pkg_details(input$repo_name)
    })

	bug_url <- shiny::eventReactive(input$retrieve_info, {

	  get_bugs_url <-
	    pkg_details() %>%
	    pkginfo::get_pkg_urls() %>%
	    dplyr::filter(website == "Bugs") %>%
	    nrow()

	  if (get_bugs_url != 1) {
	    "NA"
	  } else {
	    pkg_details() %>%
	      pkginfo::get_pkg_urls() %>%
	      dplyr::filter(website == "Bugs") %>%
	      dplyr::pull(urls)
	  }
	})

	bugs_url_link <- shiny::eventReactive(input$retrieve_info, {

		if (bug_url() != "NA") {
			"Link"
		} else {
			"NA"
		}

	})

	github_url <- shiny::eventReactive(input$retrieve_info, {

	  get_github_url <-
	    pkg_details() %>%
	    pkginfo::get_pkg_urls() %>%
	    dplyr::filter(website == "GitHub") %>%
	    nrow()

	  if (get_github_url != 1) {
	    git_url <- "NA"
	  } else {
	    git_url <-
	      pkg_details() %>%
	      pkginfo::get_pkg_urls() %>%
	      dplyr::filter(website == "GitHub") %>%
	      dplyr::pull(urls)
	  }

	})

	github_url_link <- shiny::eventReactive(input$retrieve_info, {

		if (github_url() != "NA") {
			"Link"
		} else {
			"NA"
		}

	})

	website_url <- shiny::eventReactive(input$retrieve_info, {
	  get_docs_url <-
	    pkg_details() %>%
	    pkginfo::get_pkg_urls() %>%
	    dplyr::filter(website == "Others") %>%
	    nrow()

	  if (get_docs_url != 1) {
	    "NA"
	  } else {
	    pkg_details() %>%
	      pkginfo::get_pkg_urls() %>%
	      dplyr::filter(website == "Others") %>%
	      dplyr::pull(urls)
	  }
	})

	website_url_link <- shiny::eventReactive(input$retrieve_info, {

		if (website_url() != "NA") {
			"Link"
		} else {
			"NA"
		}

	})

	basic_info_title <- eventReactive(input$repo_name, {
		shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Title      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(pkginfo::get_pkg_title(pkg_details()))
					),
				shiny::column(3)
		)
	})

	basic_info_desc <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Description      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(pkginfo::get_pkg_desc(pkg_details()))
					),
				shiny::column(3)
				)
	})

	basic_info_version <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Version      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(pkginfo::get_pkg_version(pkg_details()))
					),
				shiny::column(3)
				)
	})

	basic_info_pub <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Published      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(pkginfo::get_pkg_publish_date(pkg_details()))
					),
				shiny::column(3)
				)
	})

	basic_info_maintainter <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Maintainer      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(pkginfo::get_pkg_maintainer(pkg_details())[[1]])
					),
				shiny::column(3)
				)
	})

	basic_info_maintainter_email <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Email      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(pkginfo::get_pkg_maintainer(pkg_details())[[2]])
					),
				shiny::column(3)
				)
	})

	basic_info_cran <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('CRAN      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(shiny::tagList("", shiny::a("Link",
					  href=paste0("https://CRAN.R-project.org/package=", input$repo_name),
				target="_blank")))
					),
				shiny::column(3)
				)
	})

	basic_info_bug <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Bugs            ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(shiny::tagList("", shiny::a(bugs_url_link(), href=bug_url(),
					                                      target="_blank")))
					),
				shiny::column(3)
				)
	})

	basic_info_github <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('GitHub      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(shiny::tagList("", shiny::a(github_url_link(), href=github_url(),
					                                      target="_blank")))
					),
				shiny::column(3)
				)
	})

	basic_info_website <- eventReactive(input$repo_name, {
			shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Website      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(shiny::tagList("", shiny::a(website_url_link(), href=website_url(),
					                                      target="_blank")))
					),
				shiny::column(3)
			)
	})


	output$out_basic_title <- shiny::renderUI({
		basic_info_title()
	})

	output$out_basic_desc <- shiny::renderUI({
		basic_info_desc()
	})

	output$out_basic_version <- shiny::renderUI({
		basic_info_version()
	})

	output$out_basic_pub <- shiny::renderUI({
		basic_info_pub()
	})

	output$out_basic_maintainer <- shiny::renderUI({
		basic_info_maintainter()
	})

	output$out_basic_maintainer_email <- shiny::renderUI({
		basic_info_maintainter_email()
	})

	output$out_basic_cran <- shiny::renderUI({
		basic_info_cran()
	})

	output$out_basic_bugs <- shiny::renderUI({
		basic_info_bug()
	})

	output$out_basic_github <- shiny::renderUI({
		basic_info_github()
	})

	output$out_basic_website <- shiny::renderUI({
		basic_info_website()
	})

	update_repo <- shiny::eventReactive(input$check_repo_name, {
		repo_name <- pkginfo::get_gh_username(input$repo_name)
		if (is.null(repo_name)) {
			"NA"
		} else {
			repo_name
		}
	})

	shiny::observe({
		shiny::updateTextInput(
			session,
			inputId = "user_name",
			value = update_repo()
			)
		})

	shiny::observeEvent(input$retrieve_info, {
		shinydashboard::updateTabItems(session, "tabs", "basic_info")
		})

	shiny::observeEvent(input$retrieve_info, {
		shiny::updateDateInput(
			session,
			inputId = "start_date",
			value = lubridate::today() - 8
			)
		})

	shiny::observeEvent(input$retrieve_info, {
		shiny::updateDateInput(
			session,
			inputId = "end_date",
			value = lubridate::today() - 2
			)
		})

	compute_downloads <- reactive({

		cranlogs::cran_downloads(input$repo_name, from = input$start_date,
		                         to = input$end_date) %>%
		dplyr::select(date, count) %>%
		ggplot2::ggplot() +
		ggplot2::geom_line(ggplot2::aes(x = date, y = count), color = 'red') +
		ggplot2::xlab("Date") + ggplot2::ylab("Downloads") +
		ggplot2::ggtitle("CRAN Downloads")

		})

	output$downloads_plot <- shiny::renderPlot({
		compute_downloads()
		})

	output$cran_downloads <- shiny::renderTable({
		pkginfo::get_pkg_downloads(input$repo_name) %>%
		dplyr::rename(Latest = latest, `Last Week` = last_week,
		              `Last Month` = last_month, Total = total) 
		})

	# indicators: travis status
	travis_status <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <- pkginfo::get_status_travis(input$repo_name, input$user_name)
		}
		return(out)
	})

	output$travisBox <- shinydashboard::renderInfoBox({
		shinydashboard::infoBox(
			"Travis", travis_status(),
			icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: appveyor status
	appveyor_status <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <- pkginfo::get_status_appveyor(input$repo_name, input$user_name)
		}
		return(out)
	})

	output$appveyorBox <- shinydashboard::renderInfoBox({
		shinydashboard::infoBox(
			"Appveyor", appveyor_status(),
			icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: code coverage
	code_status <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <- pkginfo::get_code_coverage(input$repo_name, input$user_name)
		}
		return(out)
	})

	output$coverageBox <- shinydashboard::renderInfoBox({
		shinydashboard::infoBox(
			"Coverage", code_status(),
			icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: GitHub stars
	github_stars <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <- pkginfo::get_gh_stats(input$repo_name, input$user_name)$stars
		}
		return(out)
	})

	output$starsBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			github_stars(), "Stars", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: GitHub forks
	github_forks <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <- pkginfo::get_gh_stats(input$repo_name, input$user_name)$forks
		}
		return(out)
	})

	output$forksBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			github_forks(), "Forks", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: GitHub issues
	github_issues <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <- pkginfo::get_gh_stats(input$repo_name, input$user_name)$issues
		}
		return(out)
	})

	output$issuesBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			github_issues(), "Issues", icon = shiny::icon("list"),
			color = "purple"
			)
		})


	# indicators: GitHub releases
	github_releases <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <-
			  pkginfo::get_gh_releases(input$repo_name, input$user_name) %>%
			  nrow() %>%
			  as.character()
		}
		return(out)
	})

	output$releasesBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			github_releases(), "Releases", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: GitHub branches
	github_branches <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <-
			  pkginfo::get_gh_branches(input$repo_name, input$user_name) %>%
			  nrow() %>%
			  as.character()
		}
		return(out)
	})

	output$branchesBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			github_branches(), "Branches", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: GitHub pull requests
	github_prs <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			out <- NA
		} else {
			out <-
			  pkginfo::get_gh_pr(input$repo_name, input$user_name) %>%
			  nrow() %>%
			  as.character()
		}
		return(out)
	})

	output$prBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			github_prs(), "Pull Requests", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: imports
	imports <- shiny::reactive({
		pkg_details() %>%
		  pkginfo::get_pkg_imports()
	})

	output$importsBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			as.character(length(imports())), "Imports", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: suggests
	suggests <- shiny::reactive({
		pkg_details() %>%
		  pkginfo::get_pkg_suggests()
	})

	output$suggestsBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			as.character(length(suggests())), "Suggests", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# indicators: R version
	output$versionBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			pkginfo::get_pkg_r_dep(pkg_details()), "R Version",
			icon = shiny::icon("list"),
			color = "purple"
			)
		})

	# cran check results
	cr_check <- shiny::eventReactive(input$retrieve_info, {
		itable <-
				pkginfo::get_pkg_cran_check_results(input$repo_name) %>%
				dplyr::rename(OS = os, R = r, Status = status, URL = url)

			prep_url <- itable$URL

			itable %>%
			  dplyr::select(-URL) %>%
			  dplyr::mutate(Link = kableExtra::cell_spec("Link", "html", link = prep_url)) %>%
			  knitr::kable("html", escape = FALSE) %>%
  			kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = FALSE)
	})

	output$cran_check_results_table <- shiny::renderPrint({
		cr_check()
	})

	# issues
	github_issues_list <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			"There is no GitHub repository associated with this R package."
		} else {
			itable <-
				pkginfo::get_gh_issues(input$repo_name, input$user_name) %>%
				dplyr::rename(Date = date, Number = number, Author = author, Title = title)

			prep_url <-
			  paste0("https://github.com/", input$user_name, "/", input$repo_name,
			  	"/issues/", itable$Number)

			itable %>%
			  dplyr::mutate(Link = kableExtra::cell_spec("Link", "html", link = prep_url)) %>%
			  knitr::kable("html", escape = FALSE) %>%
  			kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = FALSE)
		}
	})

	output$gh_issues <- shiny::renderPrint({
		github_issues_list()
	})

	# releases
	github_releases_list <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			"There is no GitHub repository associated with this R package."
		} else {
			itable <-
				pkginfo::get_gh_releases(input$repo_name, input$user_name) %>%
				dplyr::rename(Tag = tag, Date = date, Title = title, Prerelease = prerelease)

			prep_url <-
			  paste0("https://github.com/", input$user_name, "/", input$repo_name,
			  	"/releases/tag/", itable$Tag)

			itable %>%
			  dplyr::mutate(Link = kableExtra::cell_spec("Link", "html", link = prep_url)) %>%
			  knitr::kable("html", escape = FALSE) %>%
  			kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = FALSE)
		}
	})

	output$gh_releases <- shiny::renderPrint({
		github_releases_list()
	})

	# branches
	github_branches_list <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			"There is no GitHub repository associated with this R package."
		} else {
			itable <-
				pkginfo::get_gh_branches(input$repo_name, input$user_name) %>%
				dplyr::rename(Branch = branches)

			prep_url <-
			  paste0("https://github.com/", input$user_name, "/", input$repo_name,
			  	"/tree/", itable$Branches)

			itable %>%
			  dplyr::mutate(Link = kableExtra::cell_spec("Link", "html", link = prep_url)) %>%
			  knitr::kable("html", escape = FALSE) %>%
  			kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = FALSE)
		}
	})

	output$gh_branches <- shiny::renderPrint({
		github_branches_list()
	})

	# pull requests
	github_prs_list <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			"There is no GitHub repository associated with this R package."
		} else {
			itable <-
				pkginfo::get_gh_pr(input$repo_name, input$user_name) %>%
				dplyr::rename(Number = number, Date = date, Title = title, Status = status)

			prep_url <-
			  paste0("https://github.com/", input$user_name, "/", input$repo_name,
			  	"/pull/", itable$Number)

			itable %>%
			  dplyr::mutate(Link = kableExtra::cell_spec("Link", "html", link = prep_url)) %>%
			  knitr::kable("html", escape = FALSE) %>%
  			kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = FALSE)
		}
	})

	output$gh_prs <- shiny::renderPrint({
		github_prs_list()
	})

	# stack overflow
	github_so_list <- shiny::eventReactive(input$retrieve_info, {
		if (input$user_name == "NA") {
			"There are no questions associated with this R package on Stack Overflow."
		} else {
			itable <-
				pkginfo::get_so_questions(input$repo_name) %>%
				dplyr::rename(Date = date, Title = title, Owner = owner,
				              Answered = answered, Views = views, qlink = link)

			itable %>%
			  dplyr::mutate(Link = kableExtra::cell_spec("Link", "html", link = itable$qlink)) %>%
			  dplyr::select(-qlink) %>%
			  knitr::kable("html", escape = FALSE) %>%
  			kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = FALSE)
		}
	})

	output$gh_so <- shiny::renderPrint({
		github_so_list()
	})

	# imports
	output$cran_imports <- shiny::renderPrint({
		imports() %>%
			tibble::tibble() %>%
		  magrittr::set_colnames("packages") %>%
		  dplyr::mutate(Imports = kableExtra::cell_spec(packages, "html",
		    link = paste0("https://CRAN.R-project.org/package=", packages))
		  ) %>%
		  dplyr::select(Imports) %>%
		  knitr::kable("html", escape = FALSE, target = "_blank") %>%
		  kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = FALSE)
		}
	)

	# suggests
	output$cran_suggests <- shiny::renderPrint({
		suggests() %>%
			tibble::tibble() %>%
		  magrittr::set_colnames("packages") %>%
		  dplyr::mutate(Suggests = kableExtra::cell_spec(packages, "html",
		    link = paste0("https://CRAN.R-project.org/package=", packages))
		  ) %>%
		  dplyr::select(Suggests) %>%
		  knitr::kable("html", escape = FALSE, target = "_blank") %>%
		  kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"), full_width = FALSE)
		}
	)

	shiny::observeEvent(input$exit_button, {
		shiny::stopApp()
		})
}

shiny::shinyApp(ui, server)
