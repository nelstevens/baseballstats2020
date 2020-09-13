#' timeline UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::uiOutput(ns("tmoppUI"))
    ),
    plotly::plotlyOutput(ns("tplt"))
  )
}
    
#' timeline Server Function
#'
#' @noRd 
mod_timeline_server <- function(input, output, session){
  ns <- session$ns
  
  # definiere stat
  stat <- "BA"
 
  # make df
  df <- data.frame()
  fls <- list.files(app_sys("app/www/data"), pattern = "lions_off_g[0-9]")
  for (i in fls) {
    tmp <- read.csv(
      app_sys(sprintf("app/www/data/%s", i)),
      sep = ";",
      encoding = "UTF-8"
    )
    df <- rbind(df, tmp)
  }
  df <- df %>% 
    dplyr::filter(Teamname == "Z\u00fcrich Lions") %>% 
    #dplyr::mutate(GameDate = lubridate::dmy(.$GameDate)) %>% 
    dplyr::mutate_all(dplyr::funs(stringr::str_replace(., '^\\.', '0.'))) %>%
    dplyr::mutate_all(dplyr::funs(stringr::str_replace_all(., '-', '0'))) %>% 
    # convert all columns to appropriate data type
    dplyr::mutate_all(type.convert) %>% 
    dplyr::mutate(GameDate = lubridate::dmy(.$GameDate)) %>% 
    dplyr::group_by(GameDate, Player) %>% 
    dplyr::mutate(
      gmn = ifelse(
        dplyr::row_number() == 1,
        "11.00",
        "14.00"
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(time = GameDate + lubridate::hm(gmn)) %>% 
    dplyr::group_by(time, Opponent) %>% 
    dplyr::summarise_at(
      dplyr::vars(stat),
      .funs = mean
    ) %>% 
    dplyr::ungroup()
  
  # make picker input
  output$tmoppUI <- shiny::renderUI({
    shinyWidgets::pickerInput(
      session$ns("pin"),
      "Select player(s)",
      choices = as.character(unique(df$Opponent)),
      selected = NULL,
      multiple = F
    )
  })
  
  # subset df
  # df2 <- shiny::reactive({
  #   df %>% 
  #     dplyr::filter(Player == !!input$pin) %>% 
  #     dplyr::arrange(time)
  # })
  df2 <- shiny::reactive({
    df %>% 
      dplyr::filter(Opponent == !!input$pin) %>% 
    dplyr::arrange(time)
  })
  
  
  
  #mache plot
  plt <- shiny::reactive({
    df2() %>% 
      plotly::plot_ly() %>% 
      plotly::add_trace(
        type = "scatter",
        mode = "lines",
        x = ~time,
        y = df2()[[stat]]
      )
  })
  # rendere plot
  output$tplt <- plotly::renderPlotly({plt()})
}
    
## To be copied in the UI
# mod_timeline_ui("timeline_ui_1")
    
## To be copied in the server
# callModule(mod_timeline_server, "timeline_ui_1")
 
