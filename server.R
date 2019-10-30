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
library(sf)
library(purrr)
library(plotly)

matriz <- tabnet_index()
print(matriz)

indicador_ativo <- reactiveValues()
indicador_reg_ativo <- reactiveValues()
ind_data <- reactiveValues(mun=NULL, reg=NULL, reg_cut=NULL)
line_selected <- reactiveValues(id=2)
cache <- reactiveValues(dash_year=NULL, linha_year = NULL, barra_year = NULL)
regions <- reactiveValues(muns_selected = NULL)
trigger <- reactiveValues(triggered = FALSE, region_id = NULL)

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
        img(
          src = 'visesp_2.png',
          align = "center",
          height = "200px"
        ),
        align = "center"
      ),
      fluidRow(
        column(
          4,
          wellPanel(
            id = "painelNumIndicadores",
            tagList(h1(numIndicadores),
                    h3("Indicadores")),
            style = paste0(
              "
              cursor: pointer;
              background-color: ",
              "#FFB819",
              ";
              text-align: center;
              color: #FFFFFF
              "
            )
            )
            ),
        column(
          4,
          wellPanel(
            id = "painelNumMunicipios",
            tagList(h1("646"),
                    h3("Municípios")),
            style = paste0(
              "
              cursor: pointer;
              background-color: ",
              "#FFB000",
              ";
              text-align: center;
              color: #FFFFFF
              "
            )
            )
            ),
        column(
          4,
          wellPanel(
            id = "painelNumRegioes",
            tagList(h1("97"),
                    h3("Regiões")),
            style = paste0(
              "
              cursor: pointer;
              background-color: ",
              "#FFB819",
              ";
              text-align: center;
              color: #FFFFFF
              "
            )
            )
            )
            ),
      
      fluidRow(
        column(4,
               wellPanel(
                 tagList(
                   img(
                     src = "sp_white.png",
                     height = "50%",
                     width = "50%",
                     align = "center"
                   ),
                   h2("Regiões")
                 ),
                 style = paste0(
                   "
                   cursor: pointer;
                   background-color: ",
                   "#33147F",
                   ";
                   text-align: center;
                   color: #FFFFFF;
                   "
                 ),
                 id = "painelRegioes"
                 )),
        column(
          4,
          wellPanel(
            tagList(
              img(
                src = "line-chart.png",
                height = "50%",
                width = "50%",
                align = "center"
              ),
              h2("Indicadores")
            ),
            style = paste0(
              "
              cursor: pointer;
              background-color:",
              "#3B12A1",
              ";
              text-align: center;
              color: #FFFFFF;
              "
            ),
            id = "painelIndicadores"
            )
          ),
        column(4,
               wellPanel(
                 id = "painelPlanilha",
                 tagList(
                   img(
                     src = "spreadsheet.png",
                     height = "50%",
                     width = "50%",
                     align = "center"
                   ),
                   h2("Tabelas")
                 ),
                 style = paste0(
                   "
                 cursor: pointer;
                 background-color:",
                   "#291067",
                   ";
                 text-align: center;
                 color: #FFFFFF;
                 "
                 )
               ))
          )
        )
  })
  
  #onClick functions
  
  shinyjs::onclick("painelIndicadores",
                   updateTabsetPanel(session, "navbarTabs", selected = "Indicadores")
                   )
  shinyjs::onclick("painelPlanilha",{
                   updateTabsetPanel(session, "navbarTabs", selected = "Indicadores")
                   updateTabsetPanel(session, "tabsIndicador", selected = "Tabela")
  }
  )
  shinyjs::onclick("painelRegioes",{
    updateTabsetPanel(session, "navbarTabs", selected = "Regiões")
  }
  )
  shinyjs::onclick("painelNumIndicadores",{
    updateTabsetPanel(session, "navbarTabs", selected = "Indicadores")
  }
  )
  shinyjs::onclick("painelNumMunicipios",{
    updateTabsetPanel(session, "navbarTabs", selected = "Indicadores")
  }
  )
  shinyjs::onclick("painelNumRegioes",{
    updateTabsetPanel(session, "navbarTabs", selected = "Regiões")
  }
  )
  
  
  
  output$tabelaListaIndicadores <- DT::renderDataTable({
    req(matriz$Nomes)
    tab <- as.data.frame(matriz$Nomes[-1])
    colnames(tab) <- c("Lista de Indicadores TABNET")
    tab
  },
  extensions = 'FixedHeader',
  selection = "single", options = list(
    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"),
    dom = c('ft'),
    pageLength = length(matriz$Nomes),
    fixedHeader = TRUE,
    cursor = "pointer"
  ),
  style = 'bootstrap'
  )
  
  output$conteudoIndicadoresUI <- renderUI({
    req(matriz$Nomes, matriz$Links)
    req(input$tabelaListaIndicadores_rows_selected)
    response <- s_make_tabnet_obj(matriz$Links[input$tabelaListaIndicadores_rows_selected + 1])
    if (is.null(response$error)) {
      indicador_ativo <<- response$result
      linhas <- setNames(c(1:length(indicador_ativo$NomesLinhas)), indicador_ativo$NomesLinhas)
      linhas <- linhas[indicador_ativo$NomesLinhas %in% c("Município", "DRS", "RRAS", "Região de Saúde", "Regiões Saúde")]
      linhas <- c(linhas[-1], linhas[1])
      inds <- setNames(c(1:length(indicador_ativo$NomesIndicadores)), indicador_ativo$NomesIndicadores)
      anos <- setNames(c(1:length(indicador_ativo$Anos)), indicador_ativo$Anos)
      print(linhas)
      print(inds)
      
      tagList(
        h2(strong((matriz$Nomes[input$tabelaListaIndicadores_rows_selected + 1]))),
        #HTML(paste0("<p>", paste0(stri_remove_empty(indicador_ativo$Info), collapse = "<br/>"), "</p>"))
        #DT::dataTableOutput("selecLinhaDT"),
        radioGroupButtons("selecLinha", choices = linhas),
        column(6, 
               selectInput("selecInd", "Indicador", inds, selected = length(inds))),
        column(6, 
               pickerInput(
                 inputId = "selecAno",
                 label = "Período", 
                 choices = anos,
                 options = list(
                   `actions-box` = TRUE,
                   `deselect-all-text` = "Limpar seleção",
                   `select-all-text` = "Selecionar tudo"), 
                 multiple = TRUE,
                 selected = 1
               ))
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
      response <- s_tabnet_df_retrieval(indicador_ativo, line_index = as.integer(input$selecLinha), ind_index = as.integer(input$selecInd), timeout = 2)
      if (is.null(response$error)) {
        ind_data$mun <<- response$result
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
    req(indicador_ativo$Anos)
    req(input$tabsIndicador)
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(ind_data$subind == input$selecInd)
    
    if (input$tabsIndicador == "Dashboard") {
      yearData <- ind_data$mun[ind_data$mun$Ano %in% indicador_ativo$Anos[as.integer(input$selecAno)],]
      if (input$tabelaListaIndicadores_rows_selected == 4) {
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
      tags <- tagList(helpText("Selecione algum indicador clicando no botão à esquerda!", align="center"))
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
  
  output$plotBarrasMaioresValores <- renderPlot({
    req(input$selecLinha)
    req(input$selecInd)
    req(input$selecAno)
    req(ind_data$mun)
    region <- colnames(ind_data$mun)[2]
    
    yearCut <- ind_data$mun[as.integer(as.character(ind_data$mun$Ano)) == max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])),]
    if (input$tabelaListaIndicadores_rows_selected == 4) {
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
    req(sp_cities_md)
    req(sp_rras)
    req(sp_reg_saude)
    req(sp_drs)
    region <- colnames(ind_data$mun)[2]
    print(region)
    geometry <- switch (region,
      "Município" = sp_cities_md,
      "DRS" = sp_drs,
      "RRAS" = sp_rras,
      "Região.de.Saúde" = sp_reg_saude,
      "Regiões.Saúde" = sp_reg_saude
    )
    colnames(geometry)[1] <- c("id")
    yearCut <- ind_data$mun[as.integer(as.character(ind_data$mun$Ano)) == max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])),]
    if (input$tabelaListaIndicadores_rows_selected == 4) {
      yearCut <- ind_data$mun
    }
    colnames(yearCut)[2] <- c("Name")
    print("plotData")
    print(head(yearCut))
    print("geometry")
    print(head(geometry))
    plotData <- merge(geometry, yearCut, by="id", all.x = TRUE)
    
    print(head(plotData))
    
    bounds <- st_bbox(plotData)
    
    bins <- bins(plotData$Valor[!is.na(plotData$Valor)], target.bins = 5, minpts = 1, exact.groups = TRUE)
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
                  layerId = ~plotData$id,
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
  
  output$mapaIndicadorPorRegiao <- renderLeaflet({
    req(ind_data$mun)
    req(sp_cities_md)
    req(sp_rras)
    req(sp_reg_saude)
    req(sp_drs)
    region <- colnames(ind_data$mun)[2]
    print(region)
    geometry <- switch (region,
                        "Município" = sp_cities_md,
                        "DRS" = sp_drs,
                        "RRAS" = sp_rras,
                        "Região.de.Saúde" = sp_reg_saude,
                        "Regiões.Saúde" = sp_reg_saude
    )
    colnames(geometry)[1] <- c("id")
    yearCut <- ind_data$mun[as.integer(as.character(ind_data$mun$Ano)) == max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])),]
    if (input$tabelaListaIndicadores_rows_selected == 4) {
      yearCut <- ind_data$mun
    }
    colnames(yearCut)[2] <- c("Name")
    print("plotData")
    print(head(yearCut))
    print("geometry")
    print(head(geometry))
    plotData <- merge(geometry, yearCut, by="id", all.x = TRUE)
    
    print(head(plotData))
    
    bounds <- st_bbox(plotData)
    
    bins <- bins(plotData$Valor[!is.na(plotData$Valor)], target.bins = 5, minpts = 1, exact.groups = TRUE)
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
  
  observeEvent(input$tabsIndicador,
               {
                 if (input$tabsIndicador == "Gráficos") {
                   cache$dash_year <<- input$selecAno
                   updateSelectInput(session, "selecAno", selected = c(1:length(indicador_ativo$Anos)))
                 } else if (input$tabsIndicador == "Dashboard") {
                   if (!is.null(cache$dash_year)) {
                     updateSelectInput(session, "selecAno", selected = cache$dash_year)
                   }
                 }
               }
  )
  
  observeEvent(input$tipoGrafico, 
               {
                 if (input$tipoGrafico == "Linhas") {
                   cache$barra_year <- input$selecAno
                   if (!is.null(cache$linha_year)) {
                     updateSelectInput(session, "selecAno", selected = cache$linha_year)
                   } 
                 } else {
                   cache$linha_year <- input$selecAno
                   if (!is.null(cache$barra_year)) {
                     updateSelectInput(session, "selecAno", selected = cache$barra_year)
                   } else {
                     print("entrou")
                     updateSelectInput(session, "selecAno", selected = c(1))
                   }
                 }
               }
               )
  getIndicadoresRegiaoPlotData <- reactive({
    req(ind_data$mun)
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(input$selecAno)
    req(input$selecLinha)
    req(input$selecRegiaoLinha)
    req(indicador_ativo$Anos)
    print(input$selecRegiaoLinha)
    plotData <- ind_data$mun[ind_data$mun$Ano %in% indicador_ativo$Anos[as.integer(input$selecAno)]
                             & ind_data$mun$id %in% input$selecRegiaoLinha
                             ,]
    plotData
  })
  
  graficoLinhasIndicadoresPlot <- reactive({
    g <- NULL
    plotData <- getIndicadoresRegiaoPlotData()
    if (dim(plotData)[1] > 0) {
      region <- colnames(plotData)[2]
      colnames(plotData)[2] <- c("Nome")
      
      print(head(plotData))
      plotData$Região <- paste0(plotData$Nome, "\nAno: ", plotData$Ano, "\nValor: ", plotData$Valor)
      plotData$Nome <- as.factor(plotData$Nome)
      g <- ggplot(data=plotData, aes(x=Ano, y=Valor, group=Nome, color = Nome,
                                     linetype = Nome,
                                     shape = Nome,
                                     tooltip_label = Região)) + 
        geom_line(size=1, show.legend = FALSE) + geom_point(size=1.2, show.legend = FALSE) + 
        labs(color = region, fill = region) + 
        ylab(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)]) +
        xlab("") +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
              axis.text.x = element_text(angle=90))
      
      
    }
    
    g
  })
  
  graficoBarrasIndicadoresPlot <- reactive({
    req(is.logical(input$graficoBarraLabels))
    req(is.logical(input$graficoBarraOrder))
    req(is.logical(input$graficoBarraEixo))
    g <- NULL
    plotData <- getIndicadoresRegiaoPlotData()
    if (dim(plotData)[1] > 0) {
      maxYear <- max(as.numeric(as.character(plotData$Ano))) #faz só do ano mais recente
      plotData <- plotData[as.numeric(as.character(plotData$Ano)) == maxYear,] #yearCut
      region <- colnames(plotData)[2]
      colnames(plotData)[2] <- c("Nome") #padronizar
      
      print(head(plotData))
      plotData$Região <- paste0(plotData$Nome, "\nAno: ", plotData$Ano, "\nValor: ", plotData$Valor)
      
      if (input$graficoBarraOrder) {
        plotData$Nome <- factor(plotData$Nome, levels = plotData$Nome[order(plotData$Valor)])
      }
      g <- ggplot(data=plotData, aes(x=as.factor(Nome), y=Valor, fill = Valor,
                                     tooltip_label = Região)) +
        geom_bar(stat="identity", width = 0.6, position = 'dodge', color = "white") + 
        ggtitle(paste0(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], ", por ", 
                       region, ", em ", maxYear)) + 
        ylab(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)]) +
        xlab("") +
        scale_fill_gradient(low = '#FFd370', high = '#FFB000') +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.title = element_blank(), legend.position = "none", 
              panel.background = element_rect(fill = '#FFF9EC')) 
      if (input$graficoBarraEixo) {
        g <- g + coord_flip()
      }
      
      if (length(unique(plotData$Nome)) > 6) {
        g <- g + theme(axis.text.x = element_text(angle=90))
      }
      
      if (input$graficoBarraLabels) {
        g <- g + geom_text(aes(label=Valor), size = 3.5, nudge_y = 0.5) +
          ylim(0, 1.2*max(plotData$Valor))
      }
    }
    
    g
  })
  
  output$graficoSelecaoIndicadores <- renderPlotly({
    gp <- NULL
    req(input$tipoGrafico)
    if (input$tipoGrafico == "Barras") {
      req(is.logical(input$graficoBarraEixo))
      g <- graficoBarrasIndicadoresPlot()
      if (!is.null(g)) {
        gp <- ggplotly(g, tooltip = "tooltip_label") %>% config(displayModeBar = FALSE)
        if (input$graficoBarraEixo) {
          gp <- gp %>% style(textposition = "right")
        }
      }
    } else {
      g <- graficoLinhasIndicadoresPlot()
      if (!is.null(g)) {
        gp <- ggplotly(g, tooltip = "tooltip_label") %>% config(displayModeBar = FALSE)
      }
    }
    
    
    gp
  })
  
  output$visualizacaoIndicadorUI <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(ind_data$mun)
    req(input$selecLinha)
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
                        
                      ),
                      "Gráficos" = tagList(
                        
                        column(3, 
                               uiOutput("selecaoRegiaoGraficoUI"),
                               uiOutput("boxResumoSelecaoUI")
                               ),
                        column(9, 
                               radioGroupButtons("tipoGrafico", "Gráfico", choices = c("Linhas", "Barras")),
                               uiOutput("graficoSelecaoIndicadoresUI"),
                               downloadBttn("botaoDownloadGraficoLinhas", "Baixar Gráfico", style = "minimal"),
                               align="center"
                               )
                      ),
                      "Tabela" = DT::dataTableOutput("tabelaIndicadoresDT")
      )
    }
    
    
    tags
    
  })
  
  output$graficoSelecaoIndicadoresUI <- renderUI({
    req(input$selecRegiaoLinha)
    req(input$tipoGrafico)
    tags <- NULL
    if (input$tipoGrafico == "Barras") {
      n <- length(input$selecRegiaoLinha)
      tags <- tagList(
        fluidRow(
        column(4, 
               awesomeCheckbox(
                 inputId = "graficoBarraEixo",
                 label = "Eixos invertidos", 
                 value = TRUE,
                 status = "info"
               )
               ),
        column(4, 
               awesomeCheckbox(
                 inputId = "graficoBarraLabels",
                 label = "Mostrar valores no gráfico", 
                 value = TRUE,
                 status = "info"
               )
               ),
        column(4, 
               awesomeCheckbox(
                 inputId = "graficoBarraOrder",
                 label = "Ordenar por valor", 
                 value = TRUE,
                 status = "info"
               )
               )
        ),
        plotlyOutput("graficoSelecaoIndicadores", height = 300 + 25*n),
        helpText("Para o gráfico de barras, apenas o ano mais recente entre os selecionados será considerado.")
      )
    } else {
      tags <- plotlyOutput("graficoSelecaoIndicadores", height = "100%")
    }
    tags
  })
  
  output$selecaoRegiaoGraficoUI <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(ind_data$mun)
    req(input$selecLinha)
    req(input$selecInd)
    regioes <- setNames(unique(ind_data$mun$id), unique((ind_data$mun)[,2]))
    regions <- switch(colnames(ind_data$mun)[2], 
                      "Município" = "Municípios",
                      "DRS" = "DRS's",
                      "RRAS" = "RRAS's",
                      "Região.de.Saúde" = "Regiões de saúde",
                      "Regiões.Saúde" = "Regiões de saúde"
    )
    tags <- wellPanel(
      h4(strong(paste0("Seleção de ", regions))),
      selectizeInput(
        inputId = "selecRegiaoLinha",
        label = "", 
        choices = regioes,
        multiple = TRUE,
        options = list(plugins= list('remove_button')),
        selected = (ind_data$mun$id)[c(1:3)]
      ),
      fluidRow(
        column(6, 
               actionBttn(
                 inputId = "selecRegiaoLinhaAll",
                 label = "Selecionar tudo",
                 style = "simple", 
                 color = "primary",
                 size = "sm"
               )
               )
      )
    )
    
    tags
    
  })
  
  observeEvent(input$selecRegiaoLinhaAll, {
    updateSelectizeInput(
      session,
      "selecRegiaoLinha",
      selected = (ind_data$mun$id)[c(1:length(ind_data$mun$id))]
    )
  })
  
  observeEvent(input$selecRegiaoLinhaAll, {
    updateSelectizeInput(
      session,
      "selecRegiaoLinha",
      selected = c()
    )
  })
  
  output$boxResumoSelecaoUI <- renderUI({
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(ind_data$mun)
    req(input$selecRegiaoLinha)
    req(indicador_ativo$Anos)
    regions <- switch(colnames(ind_data$mun)[2], 
                      "Município" = "Municípios",
                      "DRS" = "DRS's",
                      "RRAS" = "RRAS's",
                      "Região.de.Saúde" = "Regiões de saúde",
                      "Regiões.Saúde" = "Regiões de saúde"
    )
    
    cutData <- ind_data$mun[ind_data$mun$Ano %in% indicador_ativo$Anos[as.integer(input$selecAno)]
                            & ind_data$mun$id %in% input$selecRegiaoLinha
                            ,]
    tags <- NULL
    cutData <- cutData[!is.na(cutData$Valor),]
    if (dim(cutData)[1] > 0) {
      min <- cutData[cutData$Valor == min(cutData$Valor, na.rm = TRUE),]
      colnames(min) <- c("id", "Região", "Ano", "Valor")
      qtd_min <- dim(min)[1]
      if (qtd_min > 1 & length(unique(min$Região)) > 1) {
        min_year <- (names(table(min$Ano))[table(min$Ano) == max(table(min$Ano))])[1]
        print(min_year)
        min <- min[min$Ano == min_year,]
        qtd_min <- nrow(min)
        min$Região[1] <- paste0("Em ", qtd_min, " ", regions)
      }
      max <- cutData[cutData$Valor == max(cutData$Valor, na.rm = TRUE),]
      qtd_max <- dim(max)[1]
      colnames(max) <- c("id", "Região", "Ano", "Valor")
      if (qtd_max > 1 & length(unique(max$Região)) > 1) {
        max_year <- (names(table(max$Ano))[table(max$Ano) == max(table(max$Ano))])[1]
        max <- max[max$Ano == max_year,]
        qtd_max <- nrow(max)
        max$Região[1] <- paste0("em ", qtd_max, " ", regions)
      }
      
      tags <- tagList(
        wellPanel(
          h4("Valor máximo"),
          h2(round(max$Valor[1], 2)),
          h4(paste0(max$Região[1], ", ", max$Ano[1])),
          style = paste0("
                 background-color: ", "#2B0095", ";
                 text-align: center;
                 color: #FFFFFF
                 ")
        ),
        wellPanel(
          h4("Valor mínimo"),
          h2(round(min$Valor[1], 2)),
          h4(paste0(min$Região[1], ", ",  min$Ano[1])),
          style = paste0("
                 background-color: ", "#D99500", ";
                 text-align: center;
                 color: #FFFFFF
                 ")
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
                                         HTML(paste0("<p>", paste(indicador_ativo$Info, collapse = '<br/>'),"</p>")),
                                         HTML(paste0('<a href="', indicador_ativo$url, '">Ver no TABNET</a>'))
                                         #p(indicador_ativo$Info)
                                         )
                        ),
                        column(6, 
                               downloadBttn("botaoDownloadMapaIndicadores", "Baixar Mapa", style = "minimal"),
                               align="center"
                        )
                        
                      )
      )
    }
  })
  
  output$tabelaIndicadoresDT <- DT::renderDataTable({
    req(ind_data$mun)
    req(input$selecAno)
    req(input$selecLinha)
    df <- ind_data$mun[ind_data$mun$Ano %in% indicador_ativo$Anos[as.integer(input$selecAno)],]
    if (colnames(df)[2] == "Município") {
      df <- merge(df, regionalizacao, by.x="id", by.y="cod_mun", all.x=TRUE)
      df <- df[,c(1:2, 8, 10, 12, 3, 4)]
      colnames(df) <- c("Código", "Município", "DRS", "RRAS", "Região de Saúde", "Ano", "Valor")
    }
    df
  }, 
  class = 'cell-border stripe',
  filter = 'top',
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    pageLength = 25,
    language = list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"),
    buttons = list(list(
      extend = 'csv',
      title = paste0('tabela_', indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], "_por_", colnames(ind_data$mun)[2])
    ), list(
      extend = 'excel',
      title = paste0('tabela_', indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], "_por_", colnames(ind_data$mun)[2])
    ))
    )
  )
  
  mapaIndicadorDownload <- reactive({
    req(input$selecLinha)
    req(input$selecInd)
    req(input$selecAno)
    req(ind_data$mun)
    region <- colnames(ind_data$mun)[2]
    print(region)
    geometry <- switch (region,
                        "Município" = sp_cities_md,
                        "DRS" = sp_drs,
                        "RRAS" = sp_rras,
                        "Região.de.Saúde" = sp_reg_saude,
                        "Regiões.Saúde" = sp_reg_saude
    )
    colnames(geometry)[1] <- c("id")
    yearCut <- ind_data$mun[as.integer(as.character(ind_data$mun$Ano)) == max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])),]
    if (input$tabelaListaIndicadores_rows_selected == 4) {
      yearCut <- ind_data$mun
    }
    colnames(yearCut)[2] <- c("Name")
    plotData <- merge(geometry, yearCut, by="id")
    palette = "GnBu"
    bounds <- st_bbox(plotData)
    
    bins <- bins(plotData$Valor[!is.na(plotData$Valor)], target.bins = 5, minpts = 1, exact.groups = TRUE)
    bins <- bins.getvals(bins)
    bins <- as.numeric(bins)

    plotData <- plotData %>% 
      mutate(valor_discreto = num_intervals_breaks(plotData$Valor, bins))
    
    
    g <- ggplot(data=plotData) + 
      geom_sf(aes(fill=valor_discreto, data_id = Name), lwd = 0.2) + 
      scale_fill_brewer(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], palette = palette)
      #scale_fill_manual(gradient_sec_2)
    
    g <- g +
      ggtitle(paste0(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], ", por ",
                                            region, ", em ", max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)]))
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
      paste0(indicador_ativo$NomesIndicadores[as.integer(input$selecInd)], 
             "_",
             indicador_ativo$NomesLinhas[as.integer(input$selecLinha)], 
             "_",
             max(as.integer(indicador_ativo$Anos[as.integer(input$selecAno)])),
             ".png"
             )
    },
    content = function(file) {
      req(input$selecLinha)
      req(input$selecInd)
      req(input$selecAno)
      ggsave(file, mapaIndicadorDownload(), width = 16, height = 10.4)
    }
  )
  
  output$botaoDownloadGraficoLinhas <- downloadHandler(
    filename = function() {
      paste0(switch(input$tipoGrafico, "Linhas" = "linhas_", "Barras" = "barras_"),
             indicador_ativo$NomesIndicadores[as.integer(input$selecInd)],
             "_",
             indicador_ativo$NomesLinhas[as.integer(input$selecLinha)],
             "_",
             ".png"
      )
    },
    content = function(file) {
      req(input$tipoGrafico)
      g <- switch (input$tipoGrafico,
                   "Linhas" = graficoLinhasIndicadoresPlot(),
                   "Barras" = graficoBarrasIndicadoresPlot()
      )
      ggsave(file, g)
    }
  )
  
  observeEvent(input$selecIndReg, {
    print(input$selecIndReg)
    response <- s_make_tabnet_obj(matriz$Links[as.integer(input$selecIndReg)])
    if (is.null(response$error)) {
      indicador_reg_ativo <<- response$result
    }
    print(response$error)
  }
  )
  
  
  
  output$selectSubIndRegUI <- renderUI({
    req(input$selecIndRegLive)
    req(indicador_reg_ativo$Info)
    subinds <- indicador_reg_ativo$NomesIndicadores
    print(subinds)
    pickerInput("selectSubIndReg", 
                label = "Sub-Indicador", 
                choices = setNames(1:length(subinds), subinds), 
                selected = length(subinds)
                )
  })
  
  output$dropdownRegioesUI <- renderUI({
    tagList(
    tags$style(".btn-custom {background-color: #310D87; color: #FFF;}"),
    column(1, 
           dropdown(
             # wellPanel(style = "overflow-y:scroll; height:300px",
             # h5("Seleção de Indicadores"),
             # column(9,
             #                  DT::dataTableOutput("tabelaListaIndicadoresReg")),
             # column(3,
             #        uiOutput("selectSubIndReg")
             #        )
             # ),
             column(6, 
                    pickerInput(
                      inputId = "selecIndRegLive",
                      label = "Indicador", 
                      choices = setNames(2:length(matriz$Nomes), matriz$Nomes[-1]),
                      options = list(
                        `live-search` = TRUE)
                    )
             ),
             column(6, 
                    uiOutput("selectSubIndRegUI")),
             circle = TRUE, status = "custom", icon = icon("list"), width = "600px",tooltip = tooltipOptions(title = "Clique para ver os indicadores."))
    ),
    column(11, 
           uiOutput("tituloIndicadorReg"), align="center"
    )
    )
  })
  
  # output$selecIndRegioesUI <- renderUI({
  #   selectInput("selectIndRegioes", "Indicador", choices = setNames(c(2:length(matriz$Nomes)), matriz$Nomes[-1]))
  # })
  
  output$nomeRegioesTabUI <- renderUI({
    req(input$selecRegiaoKind)
    h3(strong(paste0("Visualização por ", input$selecRegiaoKind)))
  })
  
  output$tituloIndicadorReg <- renderUI({
    input$selectSubIndReg
    req(input$tabelaListaRegioes_rows_selected)
    tags <- NULL
    if (is.null(input$selectSubIndReg)) {
      tags <- helpText("Selecione um indicador clicando no botão.")
    } else {
      req(input$selectSubIndReg)
      list <- switch(input$selecRegiao,
                     "DRS" = drs_list,
                     "Região de Saúde" = reg_saude_list,
                     "RRAS" = rras_list)
      tags <- h4(paste0(indicador_reg_ativo$NomesIndicadores[as.integer(input$selectSubIndReg)], 
             " em ", list[input$tabelaListaRegioes_rows_selected,2]))
      
    }
    tags
  })
  
  output$selecRegiaoUI <- renderUI({
    req(input$selecRegiaoKind)
    regionNames <- switch (input$selecRegiaoKind,
      "DRS" = setNames(drs_list[,1], drs_list[,2]),
      "Região de Saúde" = setNames(reg_saude_list[,1], reg_saude_list[,2]),
      "RRAS" = setNames(rras_list[,1], rras_list[,2])
    )
    
    pickerInput(
      inputId = "selecRegiao",
      label = " ", 
      choices = regionNames,
      options = list(
        `live-search` = TRUE)
    )
  })
  
  observeEvent(input$selecRegiao, 
               print(input$selecRegiao)
               )
  
  output$leafletRegionSelect <- renderLeaflet({
    req(input$selecRegiaoKind)
    req(input$selecRegiao)
    geometry <- switch (input$selecRegiaoKind,
                        "DRS" = sp_drs,
                        "RRAS" = sp_rras,
                        "Região de Saúde" = sp_reg_saude
    )
    region_data <- switch (input$selecRegiaoKind,
                           "DRS" = drs_list,
                           "Região de Saúde" = reg_saude_list,
                           "RRAS" = rras_list)
    colnames(geometry)[1] <- c("cod")
    colnames(region_data) <- c("cod", "name")
    
    plotData <- merge(geometry, region_data, by="cod", all.x = TRUE)
    plotData$selected <- FALSE
    plotData$selected[plotData$cod == input$selecRegiao] <- TRUE
    
    bounds <- st_bbox(plotData)
    
    pal <- colorFactor(gradient_sec_2, plotData$selected)
    
    labels <- sprintf(
      "<strong>%s</strong>",
      plotData$name
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      addTiles(
        urlTemplate = positron_no_labels,
        attribution = 'Mapa por <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
      ) %>%
      setView(mean(bounds[c(1,3)]),
              mean(bounds[c(2,4)]), zoom = 6) %>%
      addPolygons(data = plotData,
                  fillColor = pal(plotData$selected),
                  fillOpacity = 0.6,
                  color = "black",
                  layerId = ~plotData$cod,
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
  
  
  observeEvent(input$mapaIndicadores_shape_click, {
    event <- input$mapaIndicadores_shape_click$id
    print( event )
    if (colnames(ind_data$mun)[2] != "Município") {
      region <- switch (colnames(ind_data$mun)[2],
                        "DRS" = "DRS",
                        "Região.de.Saúde" = "Região de Saúde",
                        "RRAS" = "RRAS",
                        "Regiões.Saúde" = "Região de Saúde"
      )
      updateNavbarPage(session, "navbarTabs", selected = "Regiões")
      updateRadioGroupButtons(session, "selecRegiaoKind", selected = region)
      trigger$region_id <<- event
      trigger$triggered <<- TRUE
      updatePickerInput(session, "selecRegiao", selected = event)
      # updatePickerInput(session, "selecIndReg", input$tabelaListaIndicadores_rows_selected + 1)
      # indicador_reg_ativo <<- indicador_ativo
      # updateSelectInput(session, "selecSubIndReg", selected = input$selecInd)
    }
  })
  
  output$selecIndsRegiaoUI <- renderUI({
    tagList(
      column(6, uiOutput("selecIndRegiaoUI")),
      column(6, uiOutput("selecSubIndRegUI"))
      
    )
  })
  
  observe({
    event <- input$leafletRegionSelect_shape_click$id
    print( event )
    updateSelectInput(session, "selecRegiao", selected = event)
  })
  
  output$selecIndsRegiaoUI <- renderUI({
    tagList(
      column(6, uiOutput("selecIndRegiaoUI")),
      column(6, uiOutput("selecSubIndRegUI"))
      
    )
  })
  
  output$selecIndRegiaoUI <- renderUI({
    req(matriz$Nomes)
    pickerInput(
      inputId = "selecIndReg",
      choices = setNames(2:length(matriz$Nomes), matriz$Nomes[-1]),
      options = list(`live-search` = TRUE)
    )

  })
  
  output$selecSubIndRegUI <- renderUI({
    req(input$selecIndReg)
    req(indicador_reg_ativo$Info)
    subinds <- indicador_reg_ativo$NomesIndicadores
    pickerInput(
      "selecSubIndReg",
      choices = setNames(1:length(subinds), subinds),
      selected = length(subinds)
    )
  })

  observeEvent(input$selecRegiaoKind, {
    if (trigger$triggered) {
      updatePickerInput(session, "selecRegiao", selected = trigger$region_id)
      trigger$triggered <<- FALSE
    }
  })
  
  observeEvent(input$selecRegiao, {
    req(input$selecRegiaoKind)
    req(input$selecRegiao)
    regionCut <- regionalizacao[,c(2, switch (input$selecRegiaoKind,
                                              "DRS" = 4,
                                              "Região de Saúde" = 8,
                                              "RRAS" = 6
    ))]
    regions$muns_selected <<- regionCut[regionCut[,2] == input$selecRegiao, 1]
    print(regions$muns_selected)
    
    req(ind_data$reg)
    ind_data$reg_cut <<- ind_data$reg[ind_data$reg$id %in% regions$muns_selected,]
    print(head(ind_data$reg_cut))
  })
  
  observeEvent({
    input$selecIndReg
    input$selecSubIndReg
  }, {
    
    print("entrou")
    
    line_index <- which(indicador_reg_ativo$NomesLinhas == "Município")
    
    if (length(line_index) > 0) {
      response <- s_tabnet_df_retrieval(indicador_reg_ativo, line_index = line_index, ind_index = as.integer(input$selecSubIndReg), timeout = 2)
      if (is.null(response$error)) {
        print("funfou")
        ind_data$reg <<- response$result
        ind_data$reg_cut <<- ind_data$reg[ind_data$reg$id %in% regions$muns_selected,]
        print(head(ind_data$reg_cut))
      } else {
        sendSweetAlert(session = session, title = "Erro na conexão", 
                       text = "Não foi possível conectar-se ao TABNET. Verifique sua conexão e tente novamente.",
                       type = "error")
      }
    } else {
      sendSweetAlert(session = session, title = "Erro com o indicador selecionado.", 
                     text = "O indicador selecionado não possui visualização a nível de município.",
                     type = "error")
      print("O indicador selecionado não possui visualização a nível de município.")
    }
  })
  
  # observeEvent(input$leafletRegionSelect_shape_click, {
  #   event <- input$map_shape_click
  #   print(event)
  # })
  
  output$tituloRegioesUI <- renderUI({
    req(input$selecRegiaoKind)
    req(input$selecRegiao)
    req(input$selecSubIndReg)
    req(indicador_reg_ativo$NomesIndicadores)
    req(regions$muns_selected)
    region_data <- switch (input$selecRegiaoKind,
                           "DRS" = drs_list,
                           "Região de Saúde" = reg_saude_list,
                           "RRAS" = rras_list)
    colnames(region_data) <- c("cod", "nome")
    region_name <- region_data[region_data$cod == input$selecRegiao, 2]
    numero <- length(regions$muns_selected)
    title <- paste0(indicador_reg_ativo$NomesIndicadores[as.integer(input$selecSubIndReg)], 
                    " em ", 
                    region_name
                    )
    # anos <- setNames(c(1:length(indicador_reg_ativo$Anos)), indicador_reg_ativo$Anos)
    anos <- indicador_reg_ativo$Anos
    tagList(h3(strong(title)),
            h4(paste0(numero, " municípios")), 
            pickerInput(
              inputId = "selecAnoReg",
              label = "Ano", 
              choices = anos
            ))
  })
  
  output$boxResumoRegionalUI <- renderUI({
    req(input$selecRegiao)
    req(input$selecSubIndReg)
    req(input$selecAnoReg)
    req(ind_data$reg_cut)
    
    #print(head(ind_data$reg$Ano))
    # print((input$selecAnoReg))
    
    
    #print(head(ind_data$reg_cut))
    cutData <- ind_data$reg_cut[ind_data$reg_cut$Ano == input$selecAnoReg,]
    cutDataSP <- ind_data$reg[ind_data$reg$Ano == input$selecAnoReg,]
    
    media <- mean(cutData$Valor, na.rm=TRUE)
    mediaSP <- mean(cutDataSP$Valor, na.rm=TRUE)

    maior <- cutData[cutData$Valor == max(cutData$Valor, na.rm=TRUE),]
    menor <- cutData[cutData$Valor == min(cutData$Valor, na.rm=TRUE),]
    
    maiorValor <- maior$Valor[1]
    menorValor <- menor$Valor[1]
    maiorNome <- maior$Município[1]
    menorNome <- menor$Município[1]
    
    if (dim(maior)[1] > 1) {
      maiorNome <- paste0("Em ", dim(maior)[1], " municípios")
    }
    
    if (dim(menor)[1] > 1) {
      menorNome <- paste0("Em ", dim(menor)[1], " municípios")
    }
    
    
    
  
    
    tagList(
      column(6, 
             wellPanel(
               tagList(
                 h1(round(media, 2)),
                 h4("Média")
               ),
               style = paste0("
                              background-color: ", "#230963", ";
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
      column(6, 
             wellPanel(
               tagList(
                 h1(round(mediaSP, 2)),
                 h4("Média do Estado")
               ),
               style = paste0("
                              background-color: ", "#311378", ";
                              text-align: center;
                              border: 2px solid gray;
                              color: #FFFFFF;
                              height: 200px;
                              max-width: 320px
                              ")
               ),
             align = "center"),
      column(6, 
             wellPanel(
               tagList(
                 h4("Maior ocorrência"),
                 h1(round(maiorValor, 2)),
                 h4(paste0(maiorNome))
               ),
               style = paste0("
                              background-color:", "#FFB000", ";
                              text-align: center;
                              border: 2px solid gray;
                              color: #FFFFFF;
                              height: 200px;
                              max-width: 320px
                              ")
             ), align = "center"), 
      column(6, 
             wellPanel(
               tagList(
                 h4("Menor ocorrência"),
                 h1(round(menorValor, 2)),
                 h4(paste0(menorNome))
               ),
               style = paste0("
                              background-color:", "#FFB819", ";
                              text-align: center;
                              border: 2px solid gray;
                              color: #FFFFFF;
                              height: 200px;
                              max-width: 320px
                              ")
             ), align = "center")
      )
    
  })
  
  output$leafletRegionalUI <- renderUI({
    tagList(
      leafletOutput("leafletRegional"),
      column(6, downloadBttn("botaoDownloadMapaRegiao", "Baixar Mapa", style = "minimal")),
      column(6, 
             br(),
             materialSwitch(
        inputId = "mapaRegLabels",
        label = "Nome dos municípios no mapa",
        value = FALSE, 
        status = "info"
      ))
      
    )
  })
  
  output$leafletRegional <- renderLeaflet({
    req(input$selecRegiao)
    req(input$selecSubIndReg)
    req(input$selecAnoReg)
    req(ind_data$reg_cut)
    
    geometry <- sp_cities_md
    cutData <- ind_data$reg_cut[ind_data$reg_cut$Ano == input$selecAnoReg,]
    
    plotData <- merge(geometry, cutData, by.x="cod_mun", by.y="id", all.x=FALSE)
    
    print(head(plotData))
    
    bounds <- st_bbox(plotData)
    
    bins <- bins(plotData$Valor[!is.na(plotData$Valor)], target.bins = 4, minpts = 1, exact.groups = TRUE)
    bins <- bins.getvals(bins)
    bins <- as.numeric(bins)
    bins[1] <- min(plotData$Valor, na.rm = TRUE)
    bins[5] <- max(plotData$Valor, na.rm = TRUE)
    
    pal <- colorBin(gradient_primary_2, plotData$Valor, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Valor em %s: %g",
      plotData$Município, plotData$Ano, plotData$Valor
    ) %>% lapply(htmltools::HTML)  
    
    leaflet() %>%
      addTiles(
        urlTemplate = positron_no_labels,
        attribution = 'Mapa por <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
      ) %>%
      setView(mean(bounds[c(1,3)]),
              mean(bounds[c(2,4)]), zoom = 7.6) %>%
      addLegend(pal = pal, values = plotData$Valor, opacity = 0.6, title = indicador_reg_ativo$NomesIndicadores[as.integer(input$selecSubIndReg)],
                position = "bottomright") %>%
      addPolygons(data = plotData,
                  fillColor = pal(plotData$Valor),
                  fillOpacity = 0.8,
                  smoothFactor = 0,
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
  
  mapaRegiaoDownload <- reactive({
    req(input$selecRegiao)
    req(input$selecSubIndReg)
    req(input$selecAnoReg)
    req(ind_data$reg_cut)
    geometry <- sp_cities_md
    colnames(geometry)[1] <- c("id")
    yearCut <- ind_data$reg_cut[ind_data$reg_cut$Ano == input$selecAnoReg,]
    plotData <- merge(geometry, yearCut, by="id", all.x=FALSE)
    palette = "GnBu"
    bounds <- st_bbox(plotData)
    
    region_data <- switch (input$selecRegiaoKind,
                           "DRS" = drs_list,
                           "Região de Saúde" = reg_saude_list,
                           "RRAS" = rras_list)
    colnames(region_data) <- c("cod", "nome")
    region_name <- region_data[region_data$cod == input$selecRegiao, 2]
    
    bins <- bins(plotData$Valor[!is.na(plotData$Valor)], target.bins = 4, minpts = 1, exact.groups = TRUE)
    bins <- bins.getvals(bins)
    bins <- as.numeric(bins)
    
    plotData <- plotData %>% 
      mutate(valor_discreto = num_intervals_breaks(plotData$Valor, bins))
    
    plotData$Município <- as.character(plotData$Município)
    print(plotData)
    dataPoints <- sf::st_point_on_surface(plotData)
    dataCoords <- as.data.frame(sf::st_coordinates(dataPoints))
    dataCoords$Name <- plotData$Município
    print(head(dataCoords))
    g <- ggplot() + 
      geom_sf(data=plotData, aes(fill=valor_discreto, data_id = Município), lwd = 0.2) + 
      scale_fill_brewer(indicador_reg_ativo$NomesIndicadores[as.integer(input$selecSubIndReg)], palette = palette)
    
    if (input$mapaRegLabels) {
      g <- g +
        geom_text(data=dataCoords, aes(X, Y, label = Name), colour = "black")
    }
      
      
    #scale_fill_manual(gradient_sec_2)
    
    g <- g +
      ggtitle(paste0(indicador_reg_ativo$NomesIndicadores[as.integer(input$selecSubIndReg)], " em ",
                     region_name, ", em ", input$selecAnoReg
      )) +
      theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5, size = 15), axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(),
            legend.position = "bottom", 
            legend.direction = "horizontal",
            legend.title = element_blank())
    g
  })
  
  output$botaoDownloadMapaRegiao <- downloadHandler(
    filename = function() {
      paste0(indicador_reg_ativo$NomesIndicadores[as.integer(input$selecSubIndReg)], 
             "_",
             input$selecAnoReg,
             ".png"
      )
    },
    content = function(file) {
      req(input$selecRegiao)
      req(input$selecSubIndReg)
      req(input$selecAnoReg)
      ggsave(file, mapaRegiaoDownload(), width = 16, height = 10.4)
    }
  )
}

