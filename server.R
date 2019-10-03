source("globals.R")

library(shiny)
library(rtabnetsp)
library(shinyjs)
library(DT)
library(stringi)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(binr)

matriz <- tabnet_index()
print(matriz)

indicador_ativo <- reactiveValues()
ind_data <- reactiveValues(mun=NULL)
line_selected <- reactiveValues(id=2)

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
    response <- s_make_tabnet_obj(matriz$Links[input$tabelaListaIndicadores_rows_selected])
    if (is.null(response$error)) {
      indicador_ativo <<- make_tabnet_obj(matriz$Links[input$tabelaListaIndicadores_rows_selected])
      linhas <- setNames(c(1:length(indicador_ativo$NomesLinhas)), indicador_ativo$NomesLinhas)
      linhas <- linhas[indicador_ativo$NomesLinhas %in% c("Município", "DRS", "RRAS", "Região de Saúde", "Regiões Saúde")]
      linhas <- c(linhas[-1], linhas[1])
      inds <- setNames(c(1:length(indicador_ativo$NomesIndicadores)), indicador_ativo$NomesIndicadores)
      anos <- setNames(c(1:length(indicador_ativo$Anos)), indicador_ativo$Anos)
      print(linhas)
      print(inds)
      
      tagList(
        h2(strong((matriz$Nomes[input$tabelaListaIndicadores_rows_selected]))),
        #HTML(paste0("<p>", paste0(stri_remove_empty(indicador_ativo$Info), collapse = "<br/>"), "</p>"))
        #DT::dataTableOutput("selecLinhaDT"),
        radioGroupButtons("selecLinha", choices = linhas),
        column(6, 
               selectInput("selecInd", "Indicador", inds, selected = length(inds))),
        column(6, selectInput("selecAno", "Período", anos, multiple = TRUE, selected = 1))
      )
      
    } else {
      sendSweetAlert(session = session, title = "Erro na conexão", 
                     text = "Não foi possível conectar-se ao TABNET. Verifique sua conexão e tente novamente.",
                     type = "error")
    }
    
  })
  
  output$selecLinhaDT <- DT::renderDT({
    req(input$tabelaListaIndicadores_rows_selected)
    indicador_ativo$linhas
  },
  selection = list(target = 'column', mode = 'single')
  )
  
  observeEvent({
    input$selecInd
    input$selecLinha
    },{
      req(input$tabelaListaIndicadores_rows_selected)
      print(input$selecInd)
      print(input$selecAno)
      print(showConnections())
      response <- s_tabnet_df_retrieval(indicador_ativo, line_index = as.integer(input$selecLinha), ind_index = as.integer(input$selecInd))
      if (is.null(response$error)) {
        ind_data$mun <<- tabnet_df_retrieval(indicador_ativo, line_index = as.integer(input$selecLinha), ind_index = as.integer(input$selecInd), timeout = 1)
        ind_data$ind <<- input$tabelaListaIndicadores_rows_selected
        ind_data$subind <<- input$selecInd
        ind_data$ano <<- input$selecAno
        print(head(ind_data$mun))
      } else {
        sendSweetAlert(session = session, title = "Erro na conexão", 
                       text = "Não foi possível conectar-se ao TABNET. Verifique sua conexão e tente novamente.",
                       type = "error")
      }
      
    })
  
  output$boxMedidasResumo <- renderUI({
    req(input$selecAno)
    req(ind_data$mun)
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(ind_data$subind == input$selecInd)

    yearData <- ind_data$mun[ind_data$mun$Ano %in% indicador_ativo$Anos[as.integer(input$selecAno)],]
    if (input$tabelaListaIndicadores_rows_selected == 5) {
      yearData <- ind_data$mun
    }
    yearData$Ano <- as.integer(as.character(yearData$Ano))
    print("boxUI")
    
    regions <- switch(colnames(yearData)[2], 
                      "Município" = "municípios",
                      "DRS" = "DRS",
                      "RRAS" = "RRAS",
                      "Região.de.Saúde" = "regiões de saúde",
                      "Regiões.Saúde" = "regiões de saúde"
                      )
    print(yearData)
    media <- mean(yearData$Valor)
    min <- yearData[yearData$Valor == min(yearData$Valor),]
    colnames(min) <- c("id", "Região", "Ano", "Valor")
    qtd_min <- dim(min)[1]
    if (qtd_min > 1) {
      min_year <- names(table(min$Ano) == max(table(min$Ano)))[1]
      print(min_year)
      min <- min[min$Ano == min_year,]
      qtd_min <- nrow(min)
      min$Região[1] <- paste0("Em ", qtd_min, " ", regions)
    }
    max <- yearData[yearData$Valor == max(yearData$Valor),]
    qtd_max <- dim(max)[1]
    colnames(max) <- c("id", "Região", "Ano", "Valor")
    if (qtd_max > 1) {
      max_year <- names(table(max$Ano) == max(table(max$Ano)))[1]
      max <- max[max$Ano == max_year,]
      qtd_max <- nrow(max)
      max$Região[1] <- paste0("Em ", qtd_max, " ", regions)
    }
    
    print(min[1,])
    print(max[1,])
    
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
                 h1(round(min$Valor[1], 2)),
                 h4(paste0(min$Região[1], ", ", min$Ano[1]))
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
                 h1(round(max$Valor[1], 2)),
                 h4(paste0(max$Região[1], ", ", max$Ano[1]))
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
  
  output$painelCondicionalSelecione <- renderUI({
    if (!isTRUE(input$tabelaListaIndicadores_rows_selected > 0)){
      tags <- tagList(helpText("Selecione um indicador clicando no botão à esquerda!", align="center"))
    } else {
      tags <- tagList()
    }
    tags
  })
  
  output$painelCondicionalCarregando <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind)
    if (input$tabelaListaIndicadores_rows_selected != ind_data$ind){
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
                    tabPanel("Mapa"),
                    tabPanel("Gráficos"),
                    tabPanel("Tabela")

        )
      )
    } else {
      tags <- tagList()
    }
    tags
  })
  
  output$plot <- renderPlot({
    ggplot(data=iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot()
  })
  
  output$mapaIndicadores <- renderLeaflet({
    req(input$selecLinha)
    req(input$selecInd)
    req(ind_data$mun)
    region <- colnames(ind_data$mun)[2]
    print(region)
    geometry <- switch (region,
      "Município" = sp_cities,
      "DRS" = sp_drs,
      "RRAS" = sp_rras,
      "Região.de.Saúde" = sp_reg_saude,
      "Regiões.Saúde" = sp_reg_saude
    )
    colnames(geometry)[1] <- c("id")
    yearCut <- ind_data$mun[as.integer(ind_data$mun$Ano) == max(as.integer(ind_data$mun$Ano)),]
    colnames(yearCut)[2] <- c("Name")
    plotData <- merge(geometry, yearCut, by="id")
    
    print(head(plotData))
    
    bounds <- st_bbox(plotData)
    
    bins <- bins(plotData$Valor[!is.na(plotData$Valor)], 5, 5)
    bins <- bins.getvals(bins)
    bins <- as.numeric(bins)
    bins[1] <- min(plotData$Valor, na.rm = TRUE)
    bins[6] <- max(plotData$Valor, na.rm = TRUE)
    
    pal <- colorBin("GnBu", plotData$Valor, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Valor: %g",
      plotData$Name, plotData$Valor
    ) %>% lapply(htmltools::HTML)
    
    print(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)])
    
    leaflet() %>%
      addTiles(
        urlTemplate = positron_no_labels,
        attribution = 'Mapa por <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
      ) %>%
      setView(mean(bounds[c(1,3)]),
              mean(bounds[c(2,4)]), zoom = 6) %>%
      addLegend(pal = pal, values = plotData$Valor, opacity = 0.6, title = indicador_ativo$NomesIndicadores[as.integer(input$selecInd)],
                position = "bottomright") %>%
      addPolygons(data = plotData,
                  fillColor = pal(plotData$Valor),
                  fillOpacity = 0.6,
                  color = "black",
                  weight = 1,
                  group = "Polígonos",
                  highlightOptions = highlightOptions(
                    weight = 4,
                    color = "white",
                    opacity = 1,
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                  ),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    
    
  })
  
  output$visualizacaoIndicadorUI <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind)
    req(input$tabsIndicador)
    tags <- NULL
    
    if (input$tabelaListaIndicadores_rows_selected == ind_data$ind) {
      tags <- switch (input$tabsIndicador,
                      "Mapa" = leafletOutput("mapaIndicadores")
      )
    }
    
    
    tags
    
  })
  
  output$grafIndicadorBarra <- renderPlot({
    req(input$selecLinha)
    req(input$selecAno)
    
    
    
  })
  
  
}

