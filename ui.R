library(shiny)
library(markdown)
library(shinyjs)
library(DT)
library(leaflet)
library(shinyWidgets)
library(plotly)

source("home.R")
source("indicadores.R")
source("regioes_tab.R")
source("como_usar.R")


navbarPage(
  id = "navbarTabs",
  "VISESP",
  tabPanel(
    "Home",
    home, 
    icon = icon("home")
  ),
  tabPanel(
    "Indicadores",
    indicadores,
    icon = icon("list")
  ),
  tabPanel(
    "Regiões",
    regioes,
    icon = icon("map")
  ),
  tabPanel(
    "Como usar",
    como_usar,
    icon = icon("question-circle-o")
  )
)