library(shiny)
library(shinythemes)
library(DT)
library(shinydashboardPlus)
library(shinyWidgets)
library(plotly)
library(openxlsx)
library(vtable)
library(ggridges)
library(beeswarm)
library(carData)
library(tidyverse)
library(datarium)
library(rstatix)
library(broom)
library(ggpubr)

source('RFunctions/table.R')
source('RFunctions/PlotFunct.R')
source('RFunctions/PlotFunct3D.R')
source('Interface/HomePage.R')
source('Interface/About.R')
source('Interface/ImportUnidimensional.R')
source('Interface/StatisticsUnidimensional.R')
source('Interface/Histograma2D.R')
source('Interface/BoxPlot.R')
source('Interface/Violin.R')
source('Interface/DotPlot2D.R')
source('Interface/Density2D.R')
source('Interface/ErrorPlot.R')
source('Interface/AvaliandoNorm.R')
source('Interface/Histogram3D.R')
source('Interface/Density3D.R')
source('Interface/DotPlot3D.R')
source('Interface/BarPlot3D.R')
source('Interface/ImportBidimensional.R')
source('Interface/ANCOVA.R')
source('Interface/ImportTridimensional.R')
source('Interface/Mesh.R')
source('Interface/Contact.R')

ui <- (fluidPage(
  title = 'Visualização de Dados do Inmetro',
  theme = shinytheme('flatly'),
  # h3('A'),
  # br(),
  # br(),
  HTML('<center><img src="Logo_exemplo_inmetro.png" width=1100 height=119></center>'),
  column(2),
  column(10,h3(strong('Visualização de dados do Inmetro'))),
    br(),
  br(),
  br(),
tags$style(HTML("
        @media (min-width: 1200px) {
            body > div .container-fluid {
                width: 1170px;
            }
            body > div > .container-fluid:nth-of-type(1) {
                 margin: 0 auto;
                 padding-top: 55px;
            }
        }")),
  navbarPage(
    selected = 'Home',
    # header = includeHTML('header.html'),
    footer = includeHTML("footer.html"),
    id = 'tabs',
    # position = "fixed-top",
    '',
    home_page(),
    about_page(),
    navbarMenu('Dados unidimensionais',
               tabPanel('Carregue seus dados',
                        tabsetPanel(
                            import_unidimensional_page(),
                            tabPanel('Digite seus dados na planilha')
                        )
               ),
               statistics_unidimensional_page(),
               tabPanel('Gráficos 2D',
                        tabsetPanel(
                          histogram2d_page(),
                          box_plot_page(),
                          violin_page(),
                          dotplot2d_page(),
                          density2D_page(),
                          error_plot_page()
                        )
               ),
               tabPanel('Gráficos 3D',
                        tabsetPanel(
                          histogram3d_page(),
                          density3D_page(),
                          dotplot3d_page(),
                          bar3D_page()
                        )
               ),
               tabPanel('Checando os dados',
                        tabsetPanel(
                          check_norm_page(),
                          tabPanel('Homogenidade das variâncias'),
                          tabPanel('Avaliando a esferecidade'),
                          tabPanel('Transformando os dados para normalidade')
                        )
               ),
    ),
    navbarMenu('Dados bidimensionais',
               import_bidimensional_page(),
               tabPanel('Comparando duas médias'),
               tabPanel('Comparando multiplas médias',
                        tabsetPanel(
                          ancova_page(),
                          tabPanel('ANOVA')
                        )
               ),
    ),
    navbarMenu('Dados tridimensionais',
               import_tridimensional_page(),
               mesh_page()
    ),
    tabPanel('Configurações'),
    contact_page(),
    # navbarMenu('Configurações'),
    # includeScript("multiple_navbar.js")
  ),
  column(2)
))