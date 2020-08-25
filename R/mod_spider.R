#' spider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spider_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::uiOutput(
        ns("sppckplayerUI")
      )
    ),
    plotly::plotlyOutput(ns("spplt"))
  )
}
    
#' spider Server Function
#'
#' @noRd 
mod_spider_server <- function(input, output, session){
  ns <- session$ns
  
  
  # define stats
  statsin <- c("H", "X2B", "X3B", "HR", "BB", "SO", "HBP")
 
  # get data
  df <- read.csv(
    app_sys("app/www/data/lions_off_tot.csv"),
    sep = "|"
  ) %>% 
    dplyr::filter(Teamname == "Zürich Lions") %>% 
    dplyr::mutate_at(
      dplyr::vars(statsin),
      .funs = function(x){x/.$PA}
    )
  
  # make input
  output$sppckplayerUI <- shiny::renderUI({
    shinyWidgets::pickerInput(
      session$ns("spdin"),
      "Select player(s)",
      choices = df$Player,
      selected = NULL,
      multiple = T
    )
  })
  # subset df
  df_sub <- shiny::reactive({
    shiny::req(input$spdin)
    df %>% 
      dplyr::filter(Player %in% !!input$spdin) %>% 
      dplyr::select(c(Player, statsin)) %>% 
      tidyr::pivot_longer(
        cols = c(-Player),
        names_to = "stat"
      )
  })

  # make plot
  plt <- shiny::reactive({
    #browser()
    plt <- plotly::plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = "lines"
    )
    for (i in unique(df_sub()$Player)) {
      len <- length(df_sub()$stat[df_sub()$Player == i])
      plt <- plt %>% 
        plotly::add_trace(
          r = c(df_sub()$value[df_sub()$Player == i], df_sub()$value[df_sub()$Player == i][1]),
          theta = c(df_sub()$stat[df_sub()$Player == i], df_sub()$stat[df_sub()$Player == i][1]),
          name = i
        )
    }
    plt <- plt %>% 
      plotly::layout(
        polar = list(
          radialaxis = list(
            visible = T#,
            #range = c(0,1)
          )
        )
      )
    return(plt)
  })
  
  # render plot
  output$spplt <- plotly::renderPlotly({
    plt()
  })
  
  
}
    
## To be copied in the UI
# mod_spider_ui("spider_ui_1")
    
## To be copied in the server
# callModule(mod_spider_server, "spider_ui_1")
 
