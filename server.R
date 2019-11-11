source("globals.R")

library(shiny)
library(rtabnetsp)
library(shinyjs)
library(ggplot2)
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
ind_data <- reactiveValues(mun=NULL, reg=NULL, reg_cut=NULL, reg_id = "none")
line_selected <- reactiveValues(id=2)
cache <- reactiveValues(dash_year=NULL, linha_year = NULL, barra_year = NULL)


#regions
indicador_reg_ativo <- reactiveValues(id = NULL)
regions <- reactiveValues(muns = NULL, region_id = NULL, regionNames = NULL, currentRegion = -1)
trigger <- reactiveValues(triggered = FALSE, regionId = NULL)
ind_reg_index <- reactiveValues(ind = NULL, sub_ind = NULL, year=NULL)

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
          ),
      fluidRow(
        column(8,
               wellPanel(
                 h4(strong("Sobre")),
                 style = paste0(
                   "background-color:",
                   "#7AC8D7",
                   ";
                 "
                 )
               )
               ),
        column(4, 
        wellPanel(
          id = "painelComoUsar",
          tagList(
            img(
              src = "information.png",
              height = "50%",
              width = "50%",
              align = "center"
            ),
            h2("Como usar")
          ),
          style = paste0(
            "
                 cursor: pointer;
                 background-color:",
            "#328EA0",
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
  
  shinyjs::onclick("painelComoUsar",{
    updateTabsetPanel(session, "navbarTabs", selected = "Como usar")
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
              panel.grid.major.y = element_line(color = "gray"),
              panel.background = element_rect(fill = "#FFFFFF"),
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
        gp <- ggplotly(g, tooltip = "tooltip_label") %>% plotly::config(displayModeBar = FALSE)
        if (input$graficoBarraEixo) {
          gp <- gp %>% style(textposition = "right")
        }
      }
    } else {
      g <- graficoLinhasIndicadoresPlot()
      if (!is.null(g)) {
        gp <- ggplotly(g, tooltip = "tooltip_label") %>% plotly::config(displayModeBar = FALSE)
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
  
  observeEvent(input$refreshSelecRegiaoLinha, {
    req(input$selPerRegKind)
    req(input$selectPerReg)
    req(input$selectPerRegMuns)
    print(input$selectPerRegMuns)
    
    updateSelectizeInput(session, "selecRegiaoLinha", selected = input$selectPerRegMuns)
    toggleDropdownButton("dropdownRegMunSelec")
  })
  
  output$selectizePerRegUI <- renderUI({
    req(input$selPerRegKind)
    req(input$selectPerReg)
    cut <- switch(input$selPerRegKind, 
                  "DRS" = regionalizacao[,c(2, 3, 4)],
                  "Região de Saúde" = regionalizacao[,c(2, 3, 8)],
                  "RRAS" = regionalizacao[,c(2, 3, 6)]
                  )
    colnames(cut)[3] <- c("cod_reg")
    cut <- cut[cut$cod_reg == input$selectPerReg,]
    names <- setNames(cut$cod_mun, cut$mun)
    selectizeInput("selectPerRegMuns", "Municípios", choices = names, multiple = TRUE, selected = names)
    
  })
  
  output$selPerRegMunsUI <- renderUI({
    req(input$selPerRegKind)
    choices <- switch(input$selPerRegKind, 
                      "DRS" = regionalizacao[,c(4, 5)],
                      "Região de Saúde" = regionalizacao[,c(8, 9)],
                      "RRAS" = regionalizacao[,c(6, 7)]
                      )
    colnames(choices) <- c("cod", "name")
    choices <- choices[order(choices$cod),]
    names <- setNames(choices$cod, choices$name)
    selectInput("selectPerReg", label = "", choices = names)
  })
  
  output$selecaoRegiaoGraficoUI <- renderUI({
    req(input$tabelaListaIndicadores_rows_selected)
    req(ind_data$ind == input$tabelaListaIndicadores_rows_selected)
    req(ind_data$mun)
    req(input$selecLinha)
    req(input$selecInd)
    regioes <-
      setNames(unique(ind_data$mun$id), unique((ind_data$mun)[, 2]))
    regions <- switch(
      colnames(ind_data$mun)[2],
      "Município" = "Municípios",
      "DRS" = "DRS's",
      "RRAS" = "RRAS's",
      "Região.de.Saúde" = "Regiões de saúde",
      "Regiões.Saúde" = "Regiões de saúde"
    )
    selecRegiaoLinhaUI <- NULL
    if (regions == "Municípios") {
      selecRegiaoLinhaUI <- tagList(
        h4(strong(paste0(
          "Seleção de ", regions
        ))),
        selectizeInput(
          inputId = "selecRegiaoLinha",
          label = "",
          choices = regioes,
          multiple = TRUE,
          options = list(plugins = list('remove_button')),
          selected = (ind_data$mun$id)[c(1:3)]
        ),
        fluidRow(br(),
                 column(
                   6,
                   dropdown(
                     fluidRow(strong("Selecionar municípios por Região")),
                     fluidRow(column(
                       6,
                       selectInput(
                         "selPerRegKind",
                         "",
                         choices = c("DRS", "Região de Saúde", "RRAS"),
                         selected = "DRS"
                       )
                     ),
                     column(6,
                            uiOutput(
                              "selPerRegMunsUI"
                            ))),
                     fluidRow(
                       uiOutput("selectizePerRegUI")
                     ),
                     actionBttn(
                       "refreshSelecRegiaoLinha",
                       label = "Selecionar",
                       style = "simple",
                       color = "success",
                       size = "sm"
                     ),
                     label = "Selecionar por Região",
                     width = "600px",
                     id = "dropdownRegMunSelec"
                   )
                 ))
        
      )
    } else {
      selecRegiaoLinhaUI <- tagList(
        h4(strong(paste0(
          "Seleção de ", regions
        ))),
        selectizeInput(
          inputId = "selecRegiaoLinha",
          label = "",
          choices = regioes,
          multiple = TRUE,
          options = list(plugins = list('remove_button')),
          selected = (ind_data$mun$id)[c(1:3)]
        ),
        br(),
        fluidRow(column(
          6,
          actionBttn(
            inputId = "selecRegiaoLinhaAll",
            label = "Selecionar tudo",
            style = "simple",
            color = "primary",
            size = "sm"
          )
        ))
      )
    }
    
    
    
    if (regions == "Municípios") {
      
    }
    
    tags <- wellPanel(
      selecRegiaoLinhaUI
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
  
  ###################  VISUALIZAÇÃO POR REGIÕES  ##########################################
  
  output$nomeRegioesTabUI <- renderUI({
    input$selecRegiaoKind
    title <- ifelse(is.null(input$selecRegiaoKind),
                    "Visualização por Região",
                    paste0("Visualização por ", input$selecRegiaoKind)
                    )
    h3(strong(title))
  })
  
  observeEvent(input$selecRegiaoKind, {
    print("selecRegiaoKind")
    regions$regionNames <<- switch (input$selecRegiaoKind,
                            "DRS" = setNames(drs_list[,1], drs_list[,2]),
                            "Região de Saúde" = setNames(reg_saude_list[,1], reg_saude_list[,2]),
                            "RRAS" = setNames(rras_list[,1] - 3500, rras_list[,2])
    )
  })

  observeEvent(regions$regionNames, {
    regions$regionId <<- regions$regionNames[[1]]
    print(regions$regionId)
  })
  
  #selecRegiao
  
  output$selecRegiaoUI <- renderUI({
    name <- names(regions$regionNames)[regions$regionNames==regions$regionId]
    dropdown(
      fluidRow(
        h4(strong(paste0("Seleção de ", input$selecRegiaoKind))),
        pickerInput(
          inputId = "selecRegiao",
          choices = regions$regionNames,
          selected = regions$regionId,
          options = list(`live-search` = TRUE)
        ),
        helpText("Selecione uma região clicando no mapa abaixo."),
        leafletOutput("leafletRegionSelect")
      ),
      tooltip = "Clique para trocar a região",
      label = name,
      align = "center",
      id = "dropdownSelectReg",
      width = "800px"
    )
  })
  
  output$leafletRegionSelect <- renderLeaflet({
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
    if (input$selecRegiaoKind == "RRAS") {
      plotData$cod <- plotData$cod - 3500
    }
    plotData$selected[plotData$cod == regions$regionId] <- TRUE
    
    
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
  
  observe({
    event <- input$leafletRegionSelect_shape_click$id
    print(event)
    #updateSelectInput(session, "selecRegiao", selected = event)
    regions$regionId <<- event
  })
  
  observeEvent(input$selecRegiao, {
    regions$regionId <<- input$selecRegiao
  })
  
  observeEvent({
    regions$regionId
  },
  {
    print(paste0("munsByReg - ", regions$regionId, " / ", regions$currentRegion))
    if (regions$currentRegion != regions$regionId | length(regions$muns) == 0) {
      munsByReg <- regionalizacao[, c(2, switch (
        input$selecRegiaoKind,
        "DRS" = 4,
        "Região de Saúde" = 8,
        "RRAS" = 6
      ))]
      if (input$selecRegiaoKind == "RRAS") {
        munsByReg[, 2] <- munsByReg[, 2] - 3500
      }
      muns_selected <- munsByReg[munsByReg[, 2] == regions$regionId, 1]
      
      
      regions$muns <<- muns_selected
      regions$currentRegion <<- regions$regionId
      print(paste0(regions$muns))
    }
  })
  
  #selecIndicador
  
  output$selecIndRegiaoAutoUI <- renderUI({
    title <- "Selecione um indicador"
    if (!is.null(indicador_reg_ativo)) {
      if (!is.null(ind_reg_index$sub_ind)) {
        title <- indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)]
      }
    }
    
    dropdown(
      fluidRow(
        h4(strong("Seleção de Indicador")),
        pickerInput(
          inputId = "selecIndRegAuto",
          label = "Indicador",
          choices = setNames(2:length(matriz$Nomes), matriz$Nomes[-1])[-4],
          selected = ind_reg_index$ind,
          options = list(`live-search` = TRUE)
        )
      ,
        uiOutput("selecSubIndRegAutoUI")
      ),
      tooltip = "Clique para selecionar o indicador",
      label = title,
      icon = icon("list"),
      align = "center",
      id = "dropdownSelectRegInd",
      width = "320px"
    )
  })
  
  output$selecSubIndRegAutoUI <- renderUI({
    req(!is.null(ind_reg_index$sub_ind))
    req(!is.null(ind_reg_index$year))
    subinds <- indicador_reg_ativo$NomesIndicadores
    anos <- indicador_reg_ativo$Anos
    tagList(
      pickerInput(
        "selecSubIndRegAuto",
        label = "Conteúdo",
        choices = setNames(1:length(subinds), subinds),
        selected = as.integer(ind_reg_index$sub_ind)
      ),
      pickerInput(
        inputId = "selecAnoReg",
        label = "Ano",
        choices = anos,
        selected = as.integer(ind_reg_index$year)
      )
    )
  })
  
  observeEvent(input$selecIndRegAuto, {
    print("selecIndRegAuto")
    ind_reg_index$ind <<- input$selecIndRegAuto
  })
  
  observeEvent(input$selecSubIndRegAuto, {
    print("selecSubIndRegAuto")
    ind_reg_index$sub_ind <<- input$selecSubIndRegAuto
  })
  
  observeEvent(input$selecAnoReg, {
    print("selecAnoReg")
    ind_reg_index$year <<- input$selecAnoReg
  })
  
  observeEvent({
    ind_reg_index$ind
    ind_reg_index$sub_ind
  }, {
    print("ind_reg_index$ind
    ind_reg_index$sub_ind")
    if (!is.null(ind_reg_index$sub_ind)) {
      key <- paste0(ind_reg_index$ind, ind_reg_index$sub_ind)
      if (key != ind_data$reg_id) {
        line_index <- which(indicador_reg_ativo$NomesLinhas == "Município")
        if (length(line_index) > 0) {
          response <-
            s_tabnet_df_retrieval(indicador_reg_ativo,
                                  line_index,
                                  as.integer(ind_reg_index$sub_ind),
                                  timeout = 4)
          if (is.null(response$error)) {
            ind_data$reg <<- response$result
            ind_data$reg_id <<- key
            print(head(ind_data$reg))
          } else {
            print(response$error)
          }
        } else {
          print("linha 2")
        }
      }
    }
  })
  
  observeEvent(ind_data$reg, {
    print("data load")
  })
  
  observeEvent(ind_reg_index$ind, {
    print("ind_reg_index$ind")
    if (!is.null(ind_reg_index$ind)) {
      response <- s_make_tabnet_obj(matriz$Links[as.integer(ind_reg_index$ind)])
      if (is.null(response$error)) {
        indicador_reg_ativo <<- response$result
        ind_reg_index$sub_ind <<- length(indicador_reg_ativo$NomesIndicadores)
        ind_reg_index$year <<- indicador_reg_ativo$Anos[1]
      }
    }
  })
  
  
  #painel de nome
  output$nomeIndRegUI <- renderUI({
    req(length(regions$muns) > 0)
    region_name <-
      names(regions$regionNames)[regions$regionNames == regions$regionId]
    num_muns <- length(regions$muns)
    tags <- NULL
    if (!is.null(ind_data$reg)) {
      tags <- tagList(wellPanel(
        h4(strong(
          paste0(indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)],
                 " em ", region_name)
        )),
        h5(ind_reg_index$year),
        h5(paste0(
          num_muns, ifelse(num_muns > 1, " municípios", " município")
        )),
        style = paste0("background-color:",
                       "#FFDC8F",
                       ";
                       "),
        align = "center"
        ))
    } else {
      tags <- tagList(
        wellPanel(h4(strong(region_name)),
                  h5(paste0(
                    num_muns, ifelse(num_muns > 1, " municípios", " município")
                  )),
        style = paste0("background-color:",
                       "#FFDC8F",
                       ";
                       "),
        align = "center"
        ))
    }
    tags
  })
  
  #mapa
  output$leafletRegionalUI <- renderUI({
    tagList(
      leafletOutput("leafletRegional"),
      column(6, downloadBttn("botaoDownloadMapaRegiao", "Baixar Mapa", style = "minimal"), align = "right",
             style="padding:20px;"),
      column(6, 
             br(),
             materialSwitch(
               inputId = "mapaRegLabels",
               label = "Nome dos municípios no mapa",
               value = FALSE, 
               status = "info"
             ),
             align = "left",
             style="padding:10px;")
      
    )
  })
  
  observeEvent(
    {ind_data$reg
      regions$muns
      }, 
    {
      ind_data$reg_cut <<- ind_data$reg[ind_data$reg$id %in% regions$muns,]
      #print(head(ind_data$reg_cut))
    }
  )
  
  output$leafletRegional <- renderLeaflet({
    ind_data$reg_cut
    req(!is.null(ind_reg_index$year))
    req(is.data.frame(ind_data$reg_cut))
    req(dim(ind_data$reg_cut)[1] > 0)
    
    #print("leafletRegional")
    
    plotData <- ind_data$reg_cut[ind_data$reg_cut$Ano == ind_reg_index$year,]
    print(plotData)
    leaflet()
    
    geometry <- sp_cities_md
    plotData <- merge(geometry, plotData, by.x="cod_mun", by.y="id", all.x=FALSE)
    bounds <- st_bbox(plotData)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Valor em %s: %g",
      plotData$Município, plotData$Ano, plotData$Valor
    ) %>% lapply(htmltools::HTML)  
    
    
    #Base Map
    l <- leaflet() %>%
      addTiles(
        urlTemplate = positron_no_labels,
        attribution = 'Mapa por <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
      ) %>%
      setView(mean(bounds[c(1,3)]),
              mean(bounds[c(2,4)]), zoom = 7.6)
    
    
    #Divide numeric variable into bins
    numBins <- 4
    repeat{
      bins_result <- s_bins(plotData$Valor[!is.na(plotData$Valor)], target.bins = numBins, minpts = 1, exact.groups = TRUE)
      if(is.null(bins_result$error) | numBins <= 1){
        break
      }
      numBins <- numBins - 1
      print(paste0("trying with: ", numBins, " bins"))
    }
    print(bins_result$error)
    
    #Case 1: Possible to use bins
    if (is.null(bins_result$error)) {
      bins <- bins_result$result
      bins <- bins.getvals(bins)
      bins <- as.numeric(bins)
      bins[1] <- min(plotData$Valor, na.rm = TRUE)
      bins[length(bins)] <- max(plotData$Valor, na.rm = TRUE)
      
      pal <- colorBin(gradient_primary_2, plotData$Valor, bins = bins)
      
      l <- l %>%
        addLegend(pal = pal, values = plotData$Valor, opacity = 0.6, title = indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)],
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
    } else { #case 2: no bins can be applied
      l <- l %>%
        addPolygons(data = plotData,
                    fillColor = gradient_primary_2[3],
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
    }
    
    l
  })
  
  output$boxResumoRegionalUI <- renderUI({
    ind_data$reg_cut
    req(!is.null(ind_reg_index$year))
    req(is.data.frame(ind_data$reg_cut))
    req(dim(ind_data$reg_cut)[1] > 0)
    
    cutData <- ind_data$reg_cut[ind_data$reg_cut$Ano == ind_reg_index$year,]
    cutDataSP <- ind_data$reg[ind_data$reg$Ano == ind_reg_index$year,]
    
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
                 h4("Menor ocorrência"),
                 h1(round(menorValor, 2)),
                 h4(paste0(menorNome))
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
               h4("Maior ocorrência"),
               h1(round(maiorValor, 2)),
               h4(paste0(maiorNome))
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
  
  output$graficoBarrasReg <- renderPlotly({
    ind_data$reg_cut
    req(!is.null(ind_reg_index$year))
    req(is.data.frame(ind_data$reg_cut))
    req(dim(ind_data$reg_cut)[1] > 0)
    g <- graficoBarrasRegPlot()
    ggplotly(g,  tooltip = "tooltip_label") %>% plotly::config(displayModeBar = FALSE)
  })
  
  graficoBarrasRegPlot <- reactive({
    g <- NULL
    plotData <- ind_data$reg_cut[ind_data$reg_cut$Ano == ind_reg_index$year,]
    if (dim(plotData)[1] > 0) {
      plotData$Label <- paste0(plotData$Nome, "\nAno: ", plotData$Ano, "\nValor: ", plotData$Valor)
      
      if (input$graficoBarraIndRegOrder) {
        plotData$Município <- factor(plotData$Município, levels = plotData$Município[order(plotData$Valor)])
      }
      
      
      if (dim(plotData)[1] > 30) {
          plotData <- plotData[c(1:30),]
          title <- paste0(indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)], " em ", 
                          names(regions$regionNames)[regions$regionNames == regions$regionId], " em ", ind_reg_index$year, " (maiores valores)")
      } else {
        title <- paste0(indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)], " em ", 
                        names(regions$regionNames)[regions$regionNames == regions$regionId], " em ", ind_reg_index$year)
        }
      g <- ggplot(data=plotData, aes(x=Município, y=Valor, fill = Valor,
                                     tooltip_label = Label)) +
        geom_bar(stat="identity", width = 0.6, position = 'dodge', color = "white") + 
        ggtitle(title) + 
        ylab(indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)]) +
        xlab("") +
        scale_fill_gradient(low = '#583d9b', high = '#311378') +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.title = element_blank(), legend.position = "none", 
              panel.background = element_rect(fill = '#FFFFFF')) 
      
      
      if (length(unique(plotData$Município)) > 6) {
        g <- g + theme(axis.text.x = element_text(angle=90))
      }
      
    }
    g
  })
  
  mapaRegiaoDownload <- reactive({
    geometry <- sp_cities_md
    plotData <- ind_data$reg_cut[ind_data$reg_cut$Ano == ind_reg_index$year,]
    plotData <- merge(geometry, plotData, by.x="cod_mun", by.y="id", all.x=FALSE)
    palette = "GnBu"
    bounds <- st_bbox(plotData)
    
    name <- names(regions$regionNames)[regions$regionNames==regions$regionId]
    
    #Divide numeric variable into bins
    numBins <- 4
    repeat{
      bins_result <- s_bins(plotData$Valor[!is.na(plotData$Valor)], target.bins = numBins, minpts = 1, exact.groups = TRUE)
      if(is.null(bins_result$error) | numBins <= 1){
        break
      }
      numBins <- numBins - 1
      print(paste0("trying with: ", numBins, " bins"))
    }
    
    print("download data:")
    print(head(plotData))
    
    if (is.null(bins_result$error)) {
      bins <- bins_result$result
      bins <- bins.getvals(bins)
      bins <- as.numeric(bins)
      plotData <- plotData %>% 
        mutate(valor_discreto = num_intervals_breaks(plotData$Valor, bins))
    } else {
      plotData$valor_discreto <- as.factor(plotData$Valor)
    }
    plotData$Município <- as.character(plotData$Município)
    dataPoints <- sf::st_point_on_surface(plotData)
    dataCoords <- as.data.frame(sf::st_coordinates(dataPoints))
    dataCoords$Name <- plotData$Município
    g <- ggplot() + 
      geom_sf(data=plotData, aes(fill=valor_discreto, data_id = Município), lwd = 0.2) + 
      scale_fill_brewer(indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)], palette = palette)
    
    if (input$mapaRegLabels) {
      g <- g +
        geom_text(data=dataCoords, aes(X, Y, label = Name), colour = "black")
    }
    
    g <- g +
      ggtitle(paste0(indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)], " em ",
                     name, ", em ", ind_reg_index$year
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
      paste0(indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)], 
             "_",
             ind_reg_index$year,
             ".png"
      )
    },
    content = function(file) {
      ggsave(file, mapaRegiaoDownload(), width = 16, height = 10.4)
    }
  )
  
  output$botaoDownloadGraficoLinhasReg <- downloadHandler(
    filename = function() {
      paste0(indicador_reg_ativo$NomesIndicadores[as.integer(ind_reg_index$sub_ind)], 
             "_",
             ind_reg_index$year,
             ".png"
      )
    },
    content = function(file) {
      ggsave(file, graficoBarrasRegPlot())
    }
  )
}

