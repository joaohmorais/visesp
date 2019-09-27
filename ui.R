library(shiny)
library(markdown)
library(shinyjs)
library(DT)

source("home.R")
source("indicadores.R")

navbarPage(
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