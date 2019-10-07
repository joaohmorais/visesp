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
                 style = paste0("
                 cursor: pointer;
                 background-color: ", scheme_1$sec_2[1], ";
                 text-align: center;
                 color: #FFFFFF
                 ")
               )
               ),
        column(4,
               wellPanel(
                 id = "painelNumMunicipios",
                 tagList(
                   h1("646"),
                   h3("Municípios")
                 ),
                 style = paste0("
                 cursor: pointer;
                 background-color: ", scheme_1$primary[1], ";
                 text-align: center;
                 color: #FFFFFF
                 ")
             )
             ),
      column(4,
             wellPanel(
               id = "painelNumRegistros",
               tagList(
                 h1("44512682"),
                 h3("Registros")
               ),
               style = paste0("
               cursor: pointer;
               background-color: ", scheme_1$sec_1[1], ";
               text-align: center;
               color: #FFFFFF
               "
             ))
             )
    ),
    
    fluidRow(
      column(4,
             wellPanel(
               tagList(
                 img(src = "peru.png", height="50%", width="50%", align="center"),
                 h2("Regiões")
               ),
               style = paste0("
                 cursor: pointer;
                 background-color: ", scheme_1$sec_1[1], ";
                 text-align: center;
                 color: #FFFFFF;
                 "
             ))
             ),
      column(4,
             wellPanel(
               tagList(
                 img(src = "line-chart.png", height="50%", width="50%", align="center"),
                 h2("Indicadores")
               ),
               style = paste0("
                 cursor: pointer;
                 background-color:", scheme_1$sec_2[3], ";
                 text-align: center;
                 color: #FFFFFF;
                 ")
             )
      ),
      column(4,
             wellPanel(
               tagList(
                 img(src = "spreadsheet.png", height="50%", width="50%", align="center"),
                 h2("Tabelas")
               ),
               style = paste0("
                 cursor: pointer;
                 background-color:", scheme_1$primary[1],";
                 text-align: center;
                 color: #FFFFFF;
                 ")
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
  
  # observeEvent(input$navbarTabs, {
  #   if (input$navbarTabs == "Indicadores") {
  #     toggleDropdownButton("dropButtonIndicadores")
  #   }
  # })
  
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
    req(input$tabsIndicador)
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(ind_data$subind == input$selecInd)
    
    if (input$tabsIndicador == "Dashboard") {
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
      yearData <- yearData[!is.na(yearData$Valor),]
      media <- mean(yearData$Valor, na.rm = TRUE)
      min <- yearData[yearData$Valor == min(yearData$Valor, na.rm = TRUE),]
      colnames(min) <- c("id", "Região", "Ano", "Valor")
      qtd_min <- dim(min)[1]
      if (qtd_min > 1) {
        min_year <- names(table(min$Ano) == max(table(min$Ano)))[1]
        print(min_year)
        min <- min[min$Ano == min_year,]
        qtd_min <- nrow(min)
        min$Região[1] <- paste0("Em ", qtd_min, " ", regions)
      }
      max <- yearData[yearData$Valor == max(yearData$Valor, na.rm = TRUE),]
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
                 style = paste0("
                                line-height: 200px;
                                background-color: ", scheme_2$sec_1[1], ";
                                text-align: center;
                                border-radius: 5px;
                                border: 2px solid gray;
                                color: #FFFFFF;
                                height: 200px;
                                vertical-align: middle;
                                max-width: 320px
                                
                                ")
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
                 style = paste0("
                                background-color: ", scheme_2$sec_1[3], ";
                                text-align: center;
                                border: 2px solid gray;
                                color: #FFFFFF;
                                height: 200px;
                                max-width: 320px
                                ")
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
                 style = paste0("
                                background-color:", scheme_2$sec_1[4], ";
                                text-align: center;
                                border: 2px solid gray;
                                color: #FFFFFF;
                                height: 200px;
                                max-width: 320px
                                ")
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
    }

    
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
                    tabPanel("Dashboard"),
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
  
  output$plotBarrasMaioresValores <- renderPlot({
    req(input$selecLinha)
    req(input$selecInd)
    req(input$selecAno)
    req(ind_data$mun)
    region <- colnames(ind_data$mun)[2]
    
    yearCut <- ind_data$mun[as.integer(as.character(ind_data$mun$Ano)) == max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])),]
    if (input$tabelaListaIndicadores_rows_selected == 5) {
      yearCut <- ind_data$mun
    }
    regions <- switch(colnames(yearCut)[2], 
                      "Município" = "Municípios",
                      "DRS" = "DRS's",
                      "RRAS" = "RRAS's",
                      "Região.de.Saúde" = "Regiões de saúde",
                      "Regiões.Saúde" = "Regiões de saúde"
    )
    colnames(yearCut)[2] <- c("Name")
    
    yearCut <- yearCut[order(-yearCut$Valor),][c(1:7),]
    yearCut$Name <- factor(yearCut$Name, levels = yearCut$Name[order(yearCut$Valor)])
    
    g <- ggplot(data=yearCut, aes(y=Valor, x=Name, fill=Name)) + 
      geom_bar(stat="identity") + coord_flip() + 
      geom_text(aes(label=Valor), position = position_dodge(width = 1), hjust=-0.3) +
      labs(y = indicador_ativo$NomesIndicadores[as.integer(input$selecInd)]) +
      ggtitle(label = paste0(regions, " com maiores valores de ", indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], " em ", max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])))) +
      theme(legend.position = "none", axis.title.y = element_blank(), 
            axis.text.y = element_text(size=12), 
            axis.title.x = element_blank(),
            axis.text.x = element_text(size=12),
            title = element_text(size=14),
            panel.background = element_rect(fill = "#FFFFFF"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) + 
      ylim(0, 1.2*max(yearCut$Valor)) +
      scale_fill_manual(values = gradient_primary)
    g
  })
  
  output$mapaIndicadores <- renderLeaflet({
    req(input$selecLinha)
    req(input$selecInd)
    req(input$selecAno)
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
    yearCut <- ind_data$mun[as.integer(as.character(ind_data$mun$Ano)) == max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])),]
    if (input$tabelaListaIndicadores_rows_selected == 5) {
      yearCut <- ind_data$mun
    }
    colnames(yearCut)[2] <- c("Name")
    plotData <- merge(geometry, yearCut, by="id")
    
    print(head(plotData))
    
    bounds <- st_bbox(plotData)
    
    bins <- bins(plotData$Valor[!is.na(plotData$Valor)], target.bins = 5, max.breaks = 5, exact.groups = TRUE)
    bins <- bins.getvals(bins)
    bins <- as.numeric(bins)
    bins[1] <- min(plotData$Valor, na.rm = TRUE)
    bins[6] <- max(plotData$Valor, na.rm = TRUE)
    
    pal <- colorBin(gradient_sec_2, plotData$Valor, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Valor em %s: %g",
      plotData$Name, plotData$Ano, plotData$Valor
    ) %>% lapply(htmltools::HTML)
    
    mapTitle <- tags$div(
      h6(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], " por ", region, " em ", max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])))
    )  
    
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
                    direction = "auto")) %>%
      addControl(mapTitle, position = "topright")
    
    
  })
  
  output$visualizacaoIndicadorUI <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind)
    req(input$tabsIndicador)
    tags <- NULL
    
    if (input$tabelaListaIndicadores_rows_selected == ind_data$ind) {
      tags <- switch (input$tabsIndicador,
                      "Dashboard" = tagList(
                        column(6,
                               plotOutput("plotBarrasMaioresValores")
                               ),
                        column(6, 
                               leafletOutput("mapaIndicadores")
                               )
                        
                      )
      )
    }
    
    
    tags
    
  })
  
  output$indicadoresExtraUI <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind)
    req(input$tabsIndicador)
    tags <- NULL
    
    if (input$tabelaListaIndicadores_rows_selected == ind_data$ind) {
      tags <- switch (input$tabsIndicador,
                      "Dashboard" = tagList(
                        br(),
                        br(),
                        column(6,
                               wellPanel(h4(strong(indicador_ativo$Nome)),
                                         p(indicador_ativo$Info)
                                         )
                        ),
                        column(6, 
                               downloadBttn("botaoDownloadMapaIndicadores", "Baixar Mapa"),
                               align="center"
                        )
                        
                      )
      )
    }
  })
  
  mapaIndicadorDownload <- reactive({
    req(input$selecLinha)
    req(input$selecInd)
    req(input$selecAno)
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
    yearCut <- ind_data$mun[as.integer(as.character(ind_data$mun$Ano)) == max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])),]
    if (input$tabelaListaIndicadores_rows_selected == 5) {
      yearCut <- ind_data$mun
    }
    colnames(yearCut)[2] <- c("Name")
    plotData <- merge(geometry, yearCut, by="id")
    palette = "GnBu"
    bounds <- st_bbox(plotData)

    plotData <- plotData %>% 
      mutate(valor_discreto = num_intervals(plotData$Valor, 6))
    
    
    g <- ggplot(data=plotData) + 
      geom_sf(aes(fill=valor_discreto, data_id = Name), lwd = 0.2) + 
      scale_fill_brewer(indicador_ativo$NomesIndicadores[indicator], palette = palette)
    
    g <- g +
      ggtitle(paste0(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], ", por ",
                                            region, ", em ", levels(plotData$Ano)
                     )) +
      theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5, size = 15), axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(),
            legend.position = "bottom", 
            legend.direction = "horizontal",
            legend.title = element_blank())
    g
  })
  
  output$botaoDownloadMapaIndicadores <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    content = function(file) {
      ggsave(file, mapaIndicadorDownload(), width = 16, height = 10.4)
    }
  )
  
  
}

