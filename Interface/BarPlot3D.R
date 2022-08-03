bar3D_page <- function (){
  tabPanel(
    'Gráfico em barras',
    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           column(12,
                  accordion(
                    id = 'accordion_bar_plot_3d',
                   accordionItem(
                     title = "Algoritmo para o cálculo dos intervalos",
                     status = accordionStatus,
                     collapsed = TRUE,
                     selectInput(
                       inputId = 'algorithm_bar3d',
                       label = 'Escolha o algoritmo para o cálculo dos intervalos:',
                       choices = c(
                         'Sturges' = 'Sturges',
                         'Scott' = 'Scott',
                         'Freedman-Diaconis' = 'FD'
                       )
                     )
                   ),
                   accordionItem(
                     title = "Configurações das barras",
                     status = accordionStatus,
                     collapsed = TRUE,
                     numericInput(
                         label = 'Espaçamento entre as barras:',
                         inputId = 'spacing_bar_bar3d',
                         min = 0, value = 0.2, step = 0.1, max = NA
                     ),
                     sliderInput(
                         label = 'Opacidade das barras:',
                         inputId = 'opacity_bar_bar3d',
                         min = 0, value = 0.8, step = 0.01, max = 1
                     )
                   )
                 )
          )
    ),
    column(9,
           fluidPage(fluidRow(column(
             div(h2("Gráfico de Barras 3d",
                    style="text-align:center; font-size:50px;")),
                         tabPanel(title = 'Gráfico',
                                  shinycssloaders::withSpinner(
                                    plotlyOutput('plotly_bar3d'),
                                    type = spinnerType,
                                    color = spinnerColor,
                                    size = spinnerSize
                                  )
                         ),
        width = 9
      )))
    ),
    column(12,hr())
  )
}