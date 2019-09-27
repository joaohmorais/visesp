library(shiny)
library(rtabnetsp)
library(shinyjs)
library(DT)
library(stringi)

matriz <- tabnet_index()
print(matriz)

indicador_ativo <- reactiveValues()
ind_data <- reactiveValues(mun=NULL)

function(input, output, session) {
  
  getNumIndicadores <- reactive({
    req(matriz$Nomes)
    return (length(matriz$Nomes))
  })
  
  output$homeUI <- renderUI({
    req(matriz$Nomes)
    numIndicadores <- getNumIndicadores()
    print(numIndicadores)
    print("ok")
    tagList(
      fluidRow(
        column(4,
               wellPanel(
                 id = "painelNumIndicadores",
                 tagList(
                   h1(numIndicadores),
                   h3("Indicadores")
                 ),
                 style = "
                 cursor: pointer;
                 background-color: #394a6d;
                 text-align: center;
                 color: #FFFFFF
                 "
               )
               ),
        column(4,
               wellPanel(
                 id = "painelNumMunicipios",
                 tagList(
                   h1("646"),
                   h3("Municípios")
                 ),
                 style = "
                 cursor: pointer;
                 background-color: #3c9d9b;
                 text-align: center;
                 color: #FFFFFF
                 "
             )
             ),
      column(4,
             wellPanel(
               id = "painelNumRegistros",
               tagList(
                 h1("44512682"),
                 h3("Registros")
               ),
               style = "
               cursor: pointer;
               background-color: #52de97;
               text-align: center;
               color: #FFFFFF
               "
             )
             )
    ),
    
    fluidRow(
      column(4,
             wellPanel(
               tagList(
                 img(src = "peru.png", height="50%", width="50%", align="center"),
                 h2("Regiões")
               ),
               style = "
                 cursor: pointer;
                 background-color: #52DE97;
                 text-align: center;
                 color: #FFFFFF;
                 "
             )
             ),
      column(4,
             wellPanel(
               tagList(
                 img(src = "line-chart.png", height="50%", width="50%", align="center"),
                 h2("Indicadores")
               ),
               style = "
                 cursor: pointer;
                 background-color: #424C55;
                 text-align: center;
                 color: #FFFFFF;
                 "
             )
      ),
      column(4,
             wellPanel(
               tagList(
                 img(src = "spreadsheet.png", height="50%", width="50%", align="center"),
                 h2("Tabelas")
               ),
               style = "
                 cursor: pointer;
                 background-color: #FFAF87;
                 text-align: center;
                 color: #FFFFFF;
                 "
             )
      )
    )
  )
})
  
  output$tabelaListaIndicadores <- DT::renderDataTable({
    req(matriz$Nomes)
    tab <- as.data.frame(matriz$Nomes)
    colnames(tab) <- c("Lista de Indicadores")
    tab
  },
  extensions = 'FixedHeader',
  selection = "single", options = list(
    language = list(url = "http://cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"),
    dom = c('ft'),
    pageLength = length(matriz$Nomes),
    fixedHeader = TRUE,
    cursor = "pointer"
  ),
  style = 'bootstrap'
  )
  
  observeEvent(input$tabelaListaIndicadores_rows_selected, 
               {
                 # req(matriz$Links)
                 # indicador_ativo <<- make_tabnet_obj(matriz$Links[input$tabelaListaIndicadores_rows_selected])
                 # print(indicador_ativo)
               })
  
  output$conteudoIndicadoresUI <- renderUI({
    req(matriz$Nomes, matriz$Links)
    req(input$tabelaListaIndicadores_rows_selected)
    indicador_ativo <<- make_tabnet_obj(matriz$Links[input$tabelaListaIndicadores_rows_selected])
    linhas <- setNames(c(1:length(indicador_ativo$NomesLinhas)), indicador_ativo$NomesLinhas)
    linhas <- linhas[indicador_ativo$NomesLinhas %in% c("Município", "DRS", "RRAS", "Região de Saúde", "Regiões Saúde")]
    inds <- setNames(c(1:length(indicador_ativo$NomesIndicadores)), indicador_ativo$NomesIndicadores)
    anos <- setNames(c(1:length(indicador_ativo$Anos)), indicador_ativo$Anos)
    print(linhas)
    print(inds)
    
    tagList(
      h2(strong((matriz$Nomes[input$tabelaListaIndicadores_rows_selected]))),
      #HTML(paste0("<p>", paste0(stri_remove_empty(indicador_ativo$Info), collapse = "<br/>"), "</p>"))
      column(6, 
             selectInput("selecInd", "Indicador", inds, selected = length(inds))),
      column(6, selectInput("selecAno", "Período", anos, multiple = TRUE, selected = 1))
    )
    
  })
  
  observeEvent({
    input$selecInd
    },{
      req(input$tabelaListaIndicadores_rows_selected)
      print(input$selecInd)
      print(input$selecAno)
      print(showConnections())
      ind_data$mun <<- tabnet_df_retrieval(indicador_ativo, ind_index = as.integer(input$selecInd), timeout = 4)
      ind_data$ind <<- input$tabelaListaIndicadores_rows_selected
      ind_data$subind <<- input$selecInd
      ind_data$ano <<- input$selecAno
      print(head(ind_data$mun))
    })
  
  output$boxMedidasResumo <- renderUI({
    # req(input$tabelaListaIndicadores_rows_selected)
    # req(input$selecInd)
    req(input$selecAno)
    req(ind_data$mun)
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(ind_data$subind == input$selecInd)
    print(paste0(ind_data$subind, " = ", input$selecInd))
    print("refresh")
    
    print(input$selecAno)
    print(indicador_ativo$Anos)
    print(indicador_ativo$Anos[as.integer(input$selecAno)])
    
    yearData <- ind_data$mun[ind_data$mun$Ano %in% indicador_ativo$Anos[as.integer(input$selecAno)],]
    print(head(yearData))
    media <- mean(yearData$Valor)
    min <- yearData[yearData$Valor == min(yearData$Valor),]
    qtd_min <- dim(min)[1]
    if (qtd_min > 1) {
      min$Município <- paste0("Em ", qtd_min, " municípios")
    }
    max <- yearData[yearData$Valor == max(yearData$Valor),]
    qtd_max <- dim(max)[1]
    if (qtd_max > 1) {
      max$Município <- paste0("Em ", qtd_max, " municípios")
    }
    
    tagList(
      column(4, 
             wellPanel(
               id = "painelMediaIndicador",
               tagList(
                 h1(round(media, 2)),
                 h4("Média do Estado")
               ),
               style = "
                 line-height: 200px;
                 background-color: #394a6d;
                 text-align: center;
                 border-radius: 5px;
                 border: 2px solid gray;
                 color: #FFFFFF;
                 height: 200px;
                 vertical-align: middle;
                 max-width: 320px
                 
                 "
             ),
             align = "center"),
      column(4, 
             wellPanel(
               id = "painelMenorValorIndicador",
               tagList(
                 h4("Menor ocorrência"),
                 h1(round(min$Valor, 2)),
                 h4(paste0(min$Município, ", ", min$Ano))
               ),
               style = "
                 background-color: #3c9d9b;
                 text-align: center;
                 border: 2px solid gray;
                 color: #FFFFFF;
                 height: 200px;
                 max-width: 320px
                 "
             ),
             align = "center"),
      column(4, 
             wellPanel(
               id = "painelMaiorValorIndicador",
               tagList(
                 h4("Maior ocorrência"),
                 h1(round(max$Valor, 2)),
                 h4(paste0(max$Município, ", ", max$Ano))
               ),
               style = "
                 background-color: #52de97;
                 text-align: center;
                 border: 2px solid gray;
                 color: #FFFFFF;
                 height: 200px;
                 max-width: 320px
                 "
             ), align = "center"),
      tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          "))
      
    )
  })
  
  output$painelCondicionalCarregando <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind)
    if (input$tabelaListaIndicadores_rows_selected != ind_data$ind) {
      tags <- tagList(helpText("Carregando...", align="center"))
    } else {
      tags <- tagList()
    }
    tags
  })
  
  output$tabsIndicadorUI <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind)
    if (input$tabelaListaIndicadores_rows_selected == ind_data$ind) {
      tags <- tagList(
        tabsetPanel(id = "tabsIndicador",
                    type = "tabs",
                    tabPanel("Plot"),
                    tabPanel("Summary"),
                    tabPanel("Table")

        ),
        h3("tabs")
      )
    } else {
      tags <- tagList()
    }
    tags
  })
  
  output$plot <- renderPlot({
    ggplot(data=iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot()
  })
  
  output$visualizacaoIndicadorUI <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind)
    plotOutput("plot")
    
  })
  
  output$grafIndicadorBarra <- renderPlot({
    req(input$selecLinha)
    req(input$selecAno)
    
    
  })
  
  
}

