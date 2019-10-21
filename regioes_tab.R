regioes <- fluidPage(
  fluidRow(
    column(
      6,
      uiOutput("nomeRegioesTabUI"),
      wellPanel(
        strong("Região"),
        radioGroupButtons(
          "selecRegiaoKind",
          "",
          choices = c("DRS", "Região de Saúde", "RRAS")
        ),
        uiOutput("selecRegiaoUI")
        
      ),
        strong("Indicador"),
      uiOutput("selecIndsRegiaoUI")
    ),
    column(6,
           leafletOutput("leafletRegionSelect"))
  ),
  fluidRow(
    # tabsetPanel(
    #   id = "tabsRegiao",
    #   type = "tabs",
    #   tabPanel("Dashboard"),
    #   tabPanel("Mapa"),
    #   tabPanel("Gráficos")
    # ),
    uiOutput("dadosRegiaoUI"),
    uiOutput("tabsRegiaoUI"),
    align = "center"
  ),
  fluidRow(uiOutput("tituloRegioesUI"), align="center"),
  fluidRow(column(6, 
                  uiOutput("boxResumoRegionalUI")
                  )
           ,
           column(6,
                  uiOutput("leafletRegionalUI")
                  )
            )
)