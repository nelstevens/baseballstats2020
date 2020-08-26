#' pies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pies_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::selectInput(
        ns('Stat'),
        'Select statisitic(s)',
        choices = c(
          "PA",
          "AB",
          "R",
          "H",
          "RBI",
          "X2B",
          "X3B",
          "HR",
          "SB",
          "CS",
          "BB",
          "SO",
          "HBP",
          "LOB"
        ),
        multiple = FALSE)
    ),
    #shiny::uiOutput(ns('ui'))
    plotly::plotlyOutput(ns("pies"))
  )
}
    
#' pies Server Function
#'
#' @noRd 
mod_pies_server <- function(input, output, session){
  ns <- session$ns
  # stats of interes
  soi <- c(
    "PA",
    "AB",
    "R",
    "H",
    "RBI",
    "X2B",
    "X3B",
    "HR",
    "SB",
    "CS",
    "BB",
    "SO",
    "HBP",
    "LOB"
  )
  # get data
  df <- read.csv(
    app_sys("app/www/data/lions_off_tot.csv"),
    sep = ";",
    encoding = "UTF-8"
  ) %>% 
    dplyr::filter(Teamname == "Z\u00fcrich Lions") %>% 
    dplyr::select(Player, soi) %>% 
    dplyr::mutate_all(dplyr::funs(stringr::str_replace(., '^\\.', '0.'))) %>%
    # convert all columns to appropriate data type
    dplyr::mutate_all(type.convert) %>% 
    tidyr::gather('statistic', 'value', -Player) %>% 
    dplyr::mutate(statistic = forcats::fct_inorder(statistic))
  
  # subset df
  df2 <- shiny::reactive({
    df %>% 
      dplyr::filter(statistic %in% !!input$Stat)
  })
  
  pies <- shiny::reactive({
    df2() %>% 
      dplyr::group_by(statistic) %>% 
      dplyr::mutate(relvalue = round((value/sum(value))*100)) %>%
      dplyr::arrange(statistic, dplyr::desc(relvalue)) %>% 
      #group_by(statistic) %>% 
      dplyr::mutate(rank = dplyr::row_number()) %>% 
      dplyr::mutate(Player_1 = ifelse(rank %in% 1:5, as.character(Player), 'Rest')) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(statistic, Player_1) %>% 
      dplyr::mutate(newval = sum(relvalue)) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(statistic) %>% 
      dplyr::top_n(-6, rank) %>% 
      dplyr::do(plots = 
           plotly::plot_ly(.,
                   labels = ~Player_1,
                   values = ~newval,
                   type = 'pie',
                   textposition = 'outside',
                   textinfo = 'label+percent') %>% 
           plotly::layout(margin = list(t = 60),
                  showlegend = FALSE
           )
      )
    
    
  })
  output$pies <- plotly::renderPlotly({
    pies()$plots[[1]] %>% 
      plotly::layout(
        title = list(
          text = paste0(
            '<b>Relative contribution to total ',
            as.character(pies()$statistic[[1]]),
            '</b>'
          ),
          size = 20
        )
      )
  })
  # lvls <- shiny::reactive({length(input$Stat)})
  # shiny::observe({
  #   lapply(1:lvls(), function(i){
  #     output[[i]] <- plotly::renderPlotly({
  #       pies()$plots[[i]]} %>% 
  #         plotly::layout(
  #           title = list(
  #             text = paste0(
  #               '<b>Relative contribution to total ',
  #               as.character(pies()$statistic[[i]]),
  #               '</b>'
  #             ),
  #             size = 20
  #           )
  #         )
  #     )
  #   })
  # })
  # 
  # 
  # output$ui <- shiny::renderUI({
  #   shiny::req(input$Stat)
  #   lapply(1:length(input$Stat), function(i){
  #     plotly::plotlyOutput(session$ns(i))
  #   })
  # })
}
    
## To be copied in the UI
# mod_pies_ui("pies_ui_1")
    
## To be copied in the server
# callModule(mod_pies_server, "pies_ui_1")
 
