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
	    		),
	    		br(),
	    		shiny::column(12, align = 'center',
	    			shiny::p('Coming soon!')
	    		)
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
	    	shiny::fluidRow(shiny::column(12, align = 'center', shiny::h2("Thank you for using", shiny::strong("pkginfo"), "!"))),
        shiny::fluidRow(shiny::column(12, align = 'center', shiny::actionButton("exit_button", "Exit App")))
	    )
	  )
	)
)

server <- function(input, output, session) {

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
			value = lubridate::today() - 6
		)
	})

	compute_downloads <- reactive({
		
		cranlogs::cran_downloads("olsrr", from = input$start_date, to = input$end_date) %>%
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
		pkginfo::get_cran_downloads(input$repo_name) %>%
		  dplyr::rename(Latest = latest, `Last Week` = last_week, `Last Month` = last_month, Total = total) %>%
		  knitr::kable(format = "html") %>%
		  kableExtra::kable_styling(full_width = FALSE)
	})

	output$travisBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Travis", pkginfo::get_status_travis(input$repo_name, input$user_name), icon = shiny::icon("list"),
      color = "purple"
    )
  })

  output$appveyorBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Appveyor", pkginfo::get_status_appveyor(input$repo_name, input$user_name), icon = shiny::icon("list"),
      color = "purple"
    )
  })

  output$coverageBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Coverage", pkginfo::get_code_coverage(input$repo_name, input$user_name), icon = shiny::icon("list"),
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
  	pkginfo::get_cran_suggests(input$repo_name)
  })

  output$suggestsBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      as.character(nrow(suggests())), "Suggests", icon = shiny::icon("list"),
      color = "purple"
    )
  })

  output$versionBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      pkginfo::get_cran_r_dep(input$repo_name), "R Version", icon = shiny::icon("list"),
      color = "purple"
    )
  })

	output$gh_issues <- shiny::renderPrint({
		issues() %>%
			dplyr::rename(Date = date, Number = number, Author = author, Title = title) %>%
		  knitr::kable(format = "html") %>%
		  kableExtra::kable_styling(full_width = FALSE)
	})

	output$gh_releases <- shiny::renderPrint({
		releases() %>%
		  dplyr::rename(Tag = tag, Date = date, Title = title, Prerelease = prerelease) %>%
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
			dplyr::rename(Number = number, Date = date, Title = title, Status = status) %>%
		  knitr::kable(format = "html") %>%
		  kableExtra::kable_styling(full_width = FALSE)
	})

	output$gh_so <- shiny::renderPrint({
		pkginfo::get_so_questions(input$repo_name) %>%
			dplyr::rename(Date = date, Title = title, Owner = owner, Answered = answered, Views = views) %>%
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
