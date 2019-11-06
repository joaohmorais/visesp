regioes <- fluidPage(
  fluidRow(uiOutput("nomeRegioesTabUI"),
           column(4, uiOutput("selecRegiaoUI"), align = "center", style = "padding:20px"),
           column(4, radioGroupButtons(
             "selecRegiaoKind",
             "",
             choices = c("DRS", "Região de Saúde", "RRAS")
           ), align = "center"),
           column(4,
                  uiOutput("selecIndRegiaoAutoUI"), align = "center", style="padding:20px;"
                  )
           , align = "center"),
  fluidRow(uiOutput("nomeIndRegUI")),
  fluidRow(
    uiOutput("leafletRegionalUI"),
    # column(6,
    #        helpText("Selecione uma região clicando no mapa abaixo."),
    #        leafletOutput("leafletRegionSelect"),
    #        align = "center"),
    column(6
           ,
           uiOutput("boxResumoRegionalUI")
           )
    # ,
    # column(
    #   6,
    #   wellPanel(
    #     style = paste0(
    #       "background-color:",
    #       "#E2F7FB",
    #       ";
    #              "
    #     )
    #     
    #   )
    # )
  ),
  fluidRow(
    # tabsetPanel(
    #   id = "tabsRegiao",
    #   type = "tabs",
    #   tabPanel("Dashboard"),
    #   tabPanel("Mapa"),
    #   tabPanel("Gráficos")
    # ),
    # uiOutput("dadosRegiaoUI"),
    # uiOutput("tabsRegiaoUI"),
    # align = "center"
  ),
  fluidRow(
    #uiOutput("leafletRegionalUI")
    )
  #fluidRow(uiOutput("tituloRegioesUI"), align="center")
  # fluidRow(column(6, 
  #                 uiOutput("boxResumoRegionalUI")
  #                 )
  #          ,
  #          column(6,
  #                 uiOutput("leafletRegionalUI")
  #                 )
  #           )
)