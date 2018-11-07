library(magrittr)

ui <- shinydashboard::dashboardPage(
	shinydashboard::dashboardHeader(title = "pkginfo"),
	shinydashboard::dashboardSidebar(
	  shinydashboard::sidebarMenu(
	    shinydashboard::menuItem("Basic Info", tabName = "basic_info", icon = shiny::icon("th")),
	    shinydashboard::menuItem("Downloads", tabName = "downloads", icon = shiny::icon("th")),
	    shinydashboard::menuItem("Build Status", tabName = "build_status", icon = shiny::icon("th")),
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
	    shinydashboard::tabItem(tabName = "basic_info",
	    	shiny::fluidRow(
	    	  shiny::column(12, align = 'center',
	    	  	shiny::h2("Basic Information"),
	    	  	shiny::br(),
  	    		shiny::textInput("user_name", "Repository Owner", value = NULL),
  	    		shiny::textInput("repo_name", "Repository Name", value = NULL)
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
	    	)
	    ),
	    shinydashboard::tabItem(tabName = "build_status",
	    	shiny::fluidRow(
	    		shiny::column(12, align = 'center',
	    			shiny::h2("Build Status & Code Coverage")
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

server <- function(input, output) {

	output$cran_downloads <- shiny::renderPrint({
		pkginfo::get_cran_downloads(input$repo_name) %>%
		  dplyr::rename(Latest = latest, `Last Week` = last_week, `Last Month` = last_month, Total = total) %>%
		  knitr::kable(format = "html") %>%
		  kableExtra::kable_styling(full_width = FALSE)
	})

	output$travisBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Travis", get_status_travis(input$user_name, input$repo_name), icon = shiny::icon("list"),
      color = "purple"
    )
  })

  output$appveyorBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Appveyor", get_status_appveyor(input$user_name, input$repo_name), icon = shiny::icon("list"),
      color = "purple"
    )
  })

  output$coverageBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Coverage", get_code_coverage(input$user_name, input$repo_name), icon = shiny::icon("list"),
      color = "purple"
    )
  })

  info <- shiny::reactive({
  	get_gh_stats(input$user_name, input$repo_name)
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
  	pkginfo::get_gh_issues(input$user_name, input$repo_name)
  })

  releases <- shiny::reactive({
  	pkginfo::get_gh_releases(input$user_name, input$repo_name)
  })

  output$releasesBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      as.character(nrow(releases())), "Releases", icon = shiny::icon("list"),
      color = "purple"
    )
  })

  branches <- shiny::reactive({
  	pkginfo::get_gh_branches(input$user_name, input$repo_name)
  })

  output$branchesBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      as.character(nrow(branches())), "Branches", icon = shiny::icon("list"),
      color = "purple"
    )
  })

  prs <- shiny::reactive({
  	pkginfo::get_gh_pr(input$user_name, input$repo_name)
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
