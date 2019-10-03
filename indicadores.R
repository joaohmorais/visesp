indicadores <- fluidPage({
  fluidRow(
    # column(4,
    #        wellPanel(style = "overflow-y:scroll; height:100vh",
    #                  DT::dataTableOutput("tabelaListaIndicadores"))
    #        ),
    # column(8,
           dropdownButton(wellPanel(style = "overflow-y:scroll; height:100vh",
                                                      DT::dataTableOutput("tabelaListaIndicadores")),
                          circle = TRUE, status = "primary", icon = icon("list"), width = "400px",
                          tooltip = tooltipOptions(title = "Clique para ver os indicadores.")),
           column(12, 
                  uiOutput("painelCondicionalSelecione"),
                  uiOutput("conteudoIndicadoresUI"),
                  align = "center"
                  ),
           uiOutput("boxMedidasResumo"),
           uiOutput("painelCondicionalCarregando"),
           uiOutput("tabsIndicadorUI"),
           uiOutput("visualizacaoIndicadorUI")
           #)
    
    
  )
})