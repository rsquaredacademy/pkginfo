library(magrittr)

ui <- shinydashboard::dashboardPage(
	shinydashboard::dashboardHeader(title = "pkginfo"),
	shinydashboard::dashboardSidebar(
		shinydashboard::sidebarMenu(
			id = "tabs",
			shinydashboard::menuItem("Welcome", tabName = "welcome", icon = shiny::icon("th")),
			shinydashboard::menuItem("Overview", tabName = "basic_info", icon = shiny::icon("th")),
			shinydashboard::menuItem("Downloads", tabName = "downloads", icon = shiny::icon("th")),
			shinydashboard::menuItem("Indicators", tabName = "build_status", icon = shiny::icon("th")),
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
						shiny::textInput("user_name", "GitHub Owner/Org", value = NULL)
						)
					),
				shiny::fluidRow(
					shiny::column(12, align = 'center',
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
						shiny::h2("Stack OVerflow"),
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

	bug_url <- shiny::eventReactive(input$retrieve_info, {
		pkginfo:::get_cran_table(input$repo_name) %>%
			dplyr::filter(X1 == 'BugReports:') %>%
			dplyr::select(X2)
	})

	github_url <- shiny::eventReactive(input$retrieve_info, {
		uname <- pkginfo:::get_gh_username(input$repo_name)
		paste0('https://github.com/', uname, "/", input$repo_name)
	})

	website_url <- shiny::eventReactive(input$retrieve_info, {
		pkginfo::get_pkg_urls(input$repo_name) %>%
			dplyr::filter(website != "Bugs") %>%
			dplyr::select(urls) %>%
			unlist() %>%
			stringr::str_extract(pattern = '^((?!github).)*$') %>%
			na.exclude() %>%
			magrittr::extract(1)
	})

	basic_info_title <- eventReactive(input$repo_name, {
		shiny::fluidRow(
				shiny::column(3),
				shiny::column(1, align = 'left',
					shiny::h5('Title      ')
					),
				shiny::column(5, align = 'left',
					shiny::h5(pkginfo::get_pkg_title(input$repo_name))
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
					shiny::h5(pkginfo::get_pkg_desc(input$repo_name))
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
					shiny::h5(pkginfo::get_pkg_version(input$repo_name))
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
					shiny::h5(pkginfo::get_pkg_publish_date(input$repo_name))
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
					shiny::h5(pkginfo::get_pkg_maintainer(input$repo_name))
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
					shiny::h5(shiny::tagList("", shiny::a("Link", href=bug_url(),
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
					shiny::h5(shiny::tagList("", shiny::a("Link", href=github_url(),
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
					shiny::h5(shiny::tagList("", shiny::a("Link", href=website_url(),
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
		pkginfo::get_gh_username(input$repo_name)
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

	output$cran_downloads <- shiny::renderPrint({
		pkginfo::get_pkg_downloads(input$repo_name) %>%
		dplyr::rename(Latest = latest, `Last Week` = last_week,
		              `Last Month` = last_month, Total = total) %>%
		knitr::kable(format = "html") %>%
		kableExtra::kable_styling(full_width = FALSE)
		})

	output$travisBox <- shinydashboard::renderInfoBox({
		shinydashboard::infoBox(
			"Travis", pkginfo::get_status_travis(input$repo_name, input$user_name),
			icon = shiny::icon("list"),
			color = "purple"
			)
		})

	output$appveyorBox <- shinydashboard::renderInfoBox({
		shinydashboard::infoBox(
			"Appveyor", pkginfo::get_status_appveyor(input$repo_name, input$user_name),
			icon = shiny::icon("list"),
			color = "purple"
			)
		})

	output$coverageBox <- shinydashboard::renderInfoBox({
		shinydashboard::infoBox(
			"Coverage", pkginfo::get_code_coverage(input$repo_name, input$user_name),
			icon = shiny::icon("list"),
			color = "purple"
			)
		})

	info <- shiny::reactive({
		pkginfo::get_gh_stats(input$repo_name, input$user_name)
		})

	output$starsBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			info()$stars, "Stars", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	output$forksBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			info()$forks, "Forks", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	output$issuesBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			info()$issues, "Issues", icon = shiny::icon("list"),
			color = "purple"
			)
		})


	issues <- shiny::reactive({
		pkginfo::get_gh_issues(input$repo_name, input$user_name)
		})

	releases <- shiny::reactive({
		pkginfo::get_gh_releases(input$repo_name, input$user_name)
		})

	output$releasesBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			as.character(nrow(releases())), "Releases", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	branches <- shiny::reactive({
		pkginfo::get_gh_branches(input$repo_name, input$user_name)
		})

	output$branchesBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			as.character(nrow(branches())), "Branches", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	prs <- shiny::reactive({
		pkginfo::get_gh_pr(input$repo_name, input$user_name)
		})

	output$prBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			as.character(nrow(prs())), "Pull Requests", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	imports <- shiny::reactive({
		pkginfo::get_cran_imports(input$repo_name)
		})

	output$importsBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			as.character(nrow(imports())), "Imports", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	suggests <- shiny::reactive({
		pkginfo::get_pkg_suggests(input$repo_name)
		})

	output$suggestsBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			as.character(nrow(suggests())), "Suggests", icon = shiny::icon("list"),
			color = "purple"
			)
		})

	output$versionBox <- shinydashboard::renderValueBox({
		shinydashboard::valueBox(
			pkginfo::get_pkg_r_dep(input$repo_name), "R Version",
			icon = shiny::icon("list"),
			color = "purple"
			)
		})

	output$gh_issues <- shiny::renderPrint({
		issues() %>%
		dplyr::rename(Date = date, Number = number, Author = author,
		              Title = title) %>%
		knitr::kable(format = "html") %>%
		kableExtra::kable_styling(full_width = FALSE)
		})

	output$gh_releases <- shiny::renderPrint({
		releases() %>%
		dplyr::rename(Tag = tag, Date = date, Title = title,
		              Prerelease = prerelease) %>%
		knitr::kable(format = "html") %>%
		kableExtra::kable_styling(full_width = FALSE)
		})

	output$gh_branches <- shiny::renderPrint({
		branches() %>%
		dplyr::rename(Branches = branches) %>%
		knitr::kable(format = "html") %>%
		kableExtra::kable_styling(full_width = FALSE)
		})

	output$gh_prs <- shiny::renderPrint({
		prs() %>%
		dplyr::rename(Number = number, Date = date, Title = title,
		              Status = status) %>%
		knitr::kable(format = "html") %>%
		kableExtra::kable_styling(full_width = FALSE)
		})

	output$gh_so <- shiny::renderPrint({
		pkginfo::get_so_questions(input$repo_name) %>%
		dplyr::rename(Date = date, Title = title, Owner = owner,
		              Answered = answered, Views = views) %>%
		knitr::kable(format = "html") %>%
		kableExtra::kable_styling(full_width = FALSE)
		})

	output$cran_imports <- shiny::renderPrint({
		imports() %>%
		dplyr::rename(Imports = imports) %>%
		knitr::kable(format = "html") %>%
		kableExtra::kable_styling(full_width = FALSE)
		})

	output$cran_suggests <- shiny::renderPrint({
		suggests() %>%
		dplyr::rename(Suggests = suggests) %>%
		knitr::kable(format = "html") %>%
		kableExtra::kable_styling(full_width = FALSE)
		})

	shiny::observeEvent(input$exit_button, {
		shiny::stopApp()
		})
}

shiny::shinyApp(ui, server)
