histogram3d_page <- function (){
  tabPanel(
    'Histograma 3D',
    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           column(12,
                    accordion(
                      id = 'accordion_histogram3d',
                      accordionItem(
                        title = "Configurações do gráfico",
                        status = accordionStatus,
                        collapsed = TRUE,
                        numericInput(
                          inputId = 'bins_histogram3d',
                          label = 'Escolha de intervalo: ',
                          value = 1, min = 0, max = NA, step = 1
                        )
                      ),
                      accordionItem(
                        title = "Configurações das linhas",
                        status = accordionStatus,
                        collapsed = TRUE,
                        sliderInput(
                          inputId = 'width_line_histogram3d',
                          label = 'Comprimento das linhas: ',
                          value = 5, min = 0, max = 30, step = 1
                        ),
                        sliderInput(
                          inputId = 'opacity_histogram3d',
                          label = 'Determine a opacidade das linhas: ',
                          value = 1, min = 0, max = 1, step = 0.05
                        )
                      ),
                      accordionItem(
                        title = "Configurações dos pontos",
                        status = accordionStatus,
                        collapsed = TRUE,
                        materialSwitch(
                          label = 'Mostrar pontos:',
                          inputId = "markers_histogram3d",
                          status = switchStatus,
                          value = FALSE
                        ),
                        conditionalPanel(condition = 'input.markers_histogram3d',
                                         sliderInput(
                                           inputId = 'size_markers_histogram3d',
                                           label = 'Tamanho dos pontos: ',
                                           value = 4, min = 0, max = 30, step = 1
                                         )
                                         # ,selectInput(
                                         #   inputId = 'shape_markers_histogram3d',
                                         #   label = 'Escolha o formato: ',
                                         #   choices = c(
                                         #     'Circulo' = 'circle',
                                         #     'Circulo aberto' = 'circle-open',
                                         #     'Cruz' = 'cross',
                                         #     'Diamante' = 'diamond',
                                         #     'Diamante aberto' = 'diamond-open'
                                         #     # ,'Formas diversas' = 'shapes'
                                         #   )
                                         # )
                        )
                      )
                    )

             )
    ),
    column(9,
           fluidPage(fluidRow(column(
             h2("Histograma 3D",
               style="text-align:center; font-size:50px;"),
              tabPanel(title = 'Gráfico',
                       shinycssloaders::withSpinner(
                         plotlyOutput('plotly_histogram3d'),
                         type = spinnerType,
                         color = spinnerColor,
                         size = spinnerSize
                       )
        ),
        width = 9
      )))
    ),
    column(12, hr())
  )
}