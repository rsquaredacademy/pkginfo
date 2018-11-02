library(shiny)
library(shinydashboard)
library(pkginfo)

ui <- dashboardPage(
	dashboardHeader(title = "pkginfo"),
	dashboardSidebar(
	  sidebarMenu(
	    menuItem("Basic Info", tabName = "basic_info", icon = icon("th")),
	    menuItem("Downloads", tabName = "downloads", icon = icon("th")),
	    menuItem("Build Status", tabName = "build_status", icon = icon("th")),
	    menuItem("Issues", tabName = "issues", icon = icon("th")),
	    menuItem("Releases", tabName = "releases", icon = icon("th")),
	    menuItem("Branches", tabName = "branches", icon = icon("th")),
	    menuItem("License", tabName = "license", icon = icon("th")),
	    menuItem("Dependencies", tabName = "deps", icon = icon("th")),
	    menuItem("Pull Requests", tabName = "pr", icon = icon("th"))
	  )
	),
	dashboardBody(
	  tabItems(
	    tabItem(tabName = "basic_info",
	    	h2("Basic Information"),
	    	fluidRow(
	    		textInput("user_name", "Repository Owner", value = NULL),
	    		textInput("repo_name", "Repository Name", value = NULL)
	    	)
	    ),
	    tabItem(tabName = "downloads",
	    	h2("CRAN Downloads"),
	    	fluidRow(
	    		verbatimTextOutput("cran_downloads")
	    	)
	    ),
	    tabItem(tabName = "build_status",
	    	h2("Build Status & Code Coverage"),
	    	fluidRow(
	    		infoBox("Travis", "Success", icon = icon("credit-card"), fill = TRUE),
	    		infoBox("Appveyor", "Success", icon = icon("credit-card"), fill = TRUE),
	    		infoBox("Codecov", "80%", icon = icon("list"), color = "purple", fill = TRUE)
	    	),
	    	fluidRow(
	    		valueBox(57, "Stars", icon = icon("credit-card")),
	    		valueBox(9, "Forks", icon = icon("credit-card")),
	    		valueBox(6, "Issues", icon = icon("credit-card"))
	    	),

	    	fluidRow(
	    		valueBox(57, "Imports", icon = icon("credit-card")),
	    		valueBox(9, "Suggests", icon = icon("credit-card")),
	    		valueBox(6, "Pull Requests", icon = icon("credit-card"))
	    	),
	    	fluidRow(
	    		valueBox(57, "Branches", icon = icon("credit-card")),
	    		valueBox(9, "Releases", icon = icon("credit-card")),
	    		valueBox(6, "R Version", icon = icon("credit-card"))
	    	)
	    ),
	    tabItem(tabName = "issues",
	    	h2("Open Issues"),
	    	fluidRow(
	    		verbatimTextOutput("gh_issues")
	    	)
	    ),
	    tabItem(tabName = "releases",
	    	h2("Releases"),
	    	fluidRow(
	    		verbatimTextOutput("gh_releases")
	    	)
	    ),
	    tabItem(tabName = "branches",
	    	h2("Branches"),
	    	fluidRow(
	    		verbatimTextOutput("gh_branches")
	    	)
	    ),
	    tabItem(tabName = "license",
	    	h2("License"),
	    	fluidRow(
	    		verbatimTextOutput("gh_license")
	    	)
	    ),
	    tabItem(tabName = "deps",
	    	h2("Dependencies"),
	    	fluidRow(
	    		h3("R Version"),
	    		verbatimTextOutput("cran_r_version")
	    	),
	    	fluidRow(
	    		h3("Imports"),
	    		verbatimTextOutput("cran_imports")
	    	),
	    	fluidRow(
	    		h3("Suggests"),
	    		verbatimTextOutput("cran_suggests")
	    	)
	    ),
	    tabItem(tabName = "pr",
	    	h2("Open Pull Requests"),
	    	fluidRow(
	    		verbatimTextOutput("gh_prs")
	    	)
	    )
	  )
	)
)

server <- function(input, output) {

	output$cran_downloads <- renderPrint({
		pkginfo::get_cran_downloads(input$repo_name)
	})

	output$gh_issues <- renderPrint({
		pkginfo::get_gh_issues(input$user_name, input$repo_name)
	})

	output$gh_releases <- renderPrint({
		pkginfo::get_gh_releases(input$user_name, input$repo_name)
	})

	output$gh_branches <- renderPrint({
		pkginfo::get_gh_branches(input$user_name, input$repo_name)
	})

	output$gh_prs <- renderPrint({
		pkginfo::get_gh_pr(input$user_name, input$repo_name)
	})

	output$gh_license <- renderPrint({
		pkginfo::get_gh_license(input$user_name, input$repo_name)
	})

	output$cran_r_version <- renderPrint({
		pkginfo::get_cran_r_dep(input$repo_name)
	})

	output$cran_imports <- renderPrint({
		pkginfo::get_cran_imports(input$repo_name)
	})

	output$cran_suggests <- renderPrint({
		pkginfo::get_cran_suggests(input$repo_name)
	})

}

shinyApp(ui, server)
