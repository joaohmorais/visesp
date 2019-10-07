library(shiny)
library(markdown)
library(shinyjs)
library(DT)
library(shinyWidgets)

source("home.R")
source("indicadores.R")

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
  )
)