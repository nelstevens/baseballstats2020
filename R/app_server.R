#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # set environment
  #Sys.setenv(NLS_LANG = "GERMAN_GERMANY.UTF8")
  #Sys.setenv(LC_ALL = "de_CH.utf8")
  
  # List the first level callModules here
  shiny::callModule(
    mod_spider_server,
    "main"
  )
  
  shiny::callModule(
    mod_pies_server,
    "main"
  )
  
  # shiny::callModule(
  #   mod_timeline_server,
  #   "main"
  # )
}
