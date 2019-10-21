indicadores <- fluidPage(
  fluidRow(
    # column(4,
    #        wellPanel(style = "overflow-y:scroll; height:100vh",
    #                  DT::dataTableOutput("tabelaListaIndicadores"))
    #        ),
    # column(8,
           column(1, 
                  tags$style(".btn-custom {background-color: #310D87; color: #FFF;}"),
                  dropdownButton(wellPanel(style = "overflow-y:scroll; height:100vh",
                                           DT::dataTableOutput("tabelaListaIndicadores")),
                                 circle = TRUE, status = "custom", icon = icon("list"), width = "400px",tooltip = tooltipOptions(title = "Clique para ver os indicadores."))
                  ),
           column(11, 
                  uiOutput("painelCondicionalSelecione"),
                  uiOutput("conteudoIndicadoresUI"),
                  align = "center"
                  ),
           uiOutput("tabsIndicadorUI"),
           br(),
           uiOutput("boxMedidasResumo"),
           uiOutput("painelCondicionalCarregando"),
           uiOutput("visualizacaoIndicadorUI"),
           br(),
           uiOutput("indicadoresExtraUI")
           #)
    
    
  )
)