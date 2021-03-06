#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(
        title = "Lions 2020"
      ),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Offense",
            tabName = "offense",
            icon = shiny::icon("baseball-ball"),
            startExpanded = T,
            shinydashboard::menuSubItem(
              "Spider",
              tabName = "spider",
              icon = shiny::icon("spider")
            ),
            shinydashboard::menuSubItem(
              "Relative contributions",
              tabName = "pies",
              icon = shiny::icon("spider")
            )#,
            # shinydashboard::menuSubItem(
            #   "stats over time",
            #   tabName = "timeline",
            #   icon = shiny::icon("spider")
            # )
          )
        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "spider",
            mod_spider_ui("main")
          ),
          shinydashboard::tabItem(
            tabName = "pies",
            mod_pies_ui("main")
          ),
          shinydashboard::tabItem(
            tabName = "timeline",
            mod_timeline_ui("main")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'lions2020'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

