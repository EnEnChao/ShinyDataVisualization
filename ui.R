if (!(require(shiny))){install.packages("shiny"); require(shiny, quietly=TRUE)}
if (!(require(shinythemes))){install.packages("shinythemes"); require(shinythemes, quietly=TRUE)}
if (!(require(DT))){install.packages("DT"); require(DT, quietly=TRUE)}
if (!(require(shinydashboardPlus))){install.packages("shinydashboardPlus"); require(shinydashboardPlus, quietly=TRUE)}
if (!(require(shinydashboard))){install.packages("shinydashboard"); require(shinydashboard, quietly=TRUE)}
if (!(require(shinyWidgets))){install.packages("shinyWidgets"); require(shinyWidgets, quietly=TRUE)}
if (!(require(plotly))){install.packages("plotly"); require(plotly, quietly=TRUE)}
if (!(require(openxlsx))){install.packages("openxlsx"); require(openxlsx, quietly=TRUE)}
if (!(require(vtable))){install.packages("vtable"); require(vtable, quietly=TRUE)}
if (!(require(ggridges))){install.packages("ggridges"); require(ggridges, quietly=TRUE)}
if (!(require(beeswarm))){install.packages("beeswarm"); require(beeswarm, quietly=TRUE)}
if (!(require(car))){install.packages("car"); require(car, quietly=TRUE)}
if (!(require(carData))){install.packages("carData"); require(carData, quietly=TRUE)}
if (!(require(tidyverse))){install.packages("tidyverse"); require(tidyverse, quietly=TRUE)}
if (!(require(datarium))){install.packages("datarium"); require(datarium, quietly=TRUE)}
if (!(require(rstatix))){install.packages("rstatix"); require(rstatix, quietly=TRUE)}
if (!(require(broom))){install.packages("broom"); require(broom, quietly=TRUE)}
if (!(require(ggpubr))){install.packages("ggpubr"); require(ggpubr, quietly=TRUE)}
if (!(require(rhandsontable))){install.packages("rhandsontable"); require(rhandsontable, quietly=TRUE)}
if (!(require(multcomp))){install.packages("multcomp"); require(multcomp, quietly=TRUE)}
if (!(require(emmeans))){install.packages("emmeans"); require(emmeans, quietly=TRUE)}
if (!(require(readxl))){install.packages("readxl"); require(readxl, quietly=TRUE)}
if (!(require(slickR))){install.packages("slickR"); require(slickR, quietly=TRUE)}

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
source('Interface/Config.R')
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
                            insert_unidimensional()
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
                          tabPanel('Homogeneidade  das variâncias'),
                          tabPanel('Avaliando a esfericidade'),
                          tabPanel('Transformando os dados para normalidade')
                        )
               ),
    ),
    navbarMenu('Dados bidimensionais',
               tabPanel('Carregue seus dados',
                        tabsetPanel(
                          import_bidimensional_page(),
                          insert_bidimensional()
                        )
               ),
               transform_bidimensional(),
               tabPanel('Comparando duas médias',
                        tabsetPanel(
                          tabPanel('Teste - T'),
                          tabPanel('Teste Wilcoxon'),
                          tabPanel('Teste do sinal (Sign test)')
                        )
               ),
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
    config_page(),
    contact_page(),
    # navbarMenu('Configurações'),
  ),
  column(2)
))