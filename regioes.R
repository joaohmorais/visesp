regioes <- fluidPage(
  fluidRow(
    column(12, radioGroupButtons("selecRegiao", "", choices = c("DRS", "Região de Saúde", "RRAS")), align="center"),
    column(4,
           wellPanel(style = "overflow-y:scroll; height:100vh",
                     DT::dataTableOutput("tabelaListaRegioes"))
    ),
    column(4,
           uiOutput("dropdownRegioesUI")
           
           
           
           ),
    column(4, 
           renderLeaflet("mapaIndicadorPorRegiao"))
  )
  
)