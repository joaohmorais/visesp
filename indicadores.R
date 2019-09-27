indicadores <- fluidPage({
  fluidRow(
    column(4,
           wellPanel(style = "overflow-y:scroll; height:100vh",
                     DT::dataTableOutput("tabelaListaIndicadores"))
           ),
    column(8,
           column(12, 
                  uiOutput("conteudoIndicadoresUI"),
                  align = "center"
                  ),
           uiOutput("boxMedidasResumo"),
           uiOutput("painelCondicionalCarregando"),
           uiOutput("tabsIndicadorUI"),
           uiOutput("visualizacaoIndicadorUI")
           )
    
    
  )
})

# indicadores <- sidebarLayout(
#   sidebarPanel(
#     DT::dataTableOutput("tabelaListaIndicadores"),
#     style = "overflow-y:scroll; height:100vh"
#   ),
#   mainPanel()
# )