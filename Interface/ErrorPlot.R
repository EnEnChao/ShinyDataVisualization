error_plot_page <- function (){
  tabPanel(
    'Gráfico de Erro',

    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           column(12,
                      accordion(
                        id = 'accordion_error_bar',
                        accordionItem(
                          title = "Configurações do Gráfico de Erro",
                          status = accordionStatus,
                          collapsed = TRUE,
                          h4(strong('Linha de erro:')),
                          materialSwitch(
                            inputId = "error_line",
                            status = switchStatus,
                            value = TRUE
                          ),
                          conditionalPanel(condition = 'input.error_line',
                                           sliderInput('opacity_error_line', 'Escolha a opacidade das linhas de erro: ',
                                                       min = 0, max = 1, value = 1, step = 0.05
                                           ),
                                           h4(strong('Escolha o algoritmo para o cálculo do erro')),
                                           selectInput('error_algorithm','',
                                                       choices = c(
                                                         'Desvio padrão' = 'sd',
                                                         'Erro padrão' = 'se',
                                                         'Intervalo de confiança' = 'ci'
                                                       )
                                           ),
                                           conditionalPanel(condition = 'input.error_algorithm == "ci"',
                                                            sliderInput('slider_ci', 'Escolha o valor de (1 - alpha): ',
                                                                        min = 0.1, max = 0.99, value = 0.95, step = 0.01)
                                           )
                          ),
                        ),
                        accordionItem(
                          title = "Configurações das barras",
                          status = accordionStatus,
                          collapsed = TRUE,
                          materialSwitch(
                            inputId = "bar_error_bar",
                            label = 'Adicionar barra: ',
                            status = switchStatus,
                            value = FALSE
                          ),
                          conditionalPanel(condition = 'input.bar_error_bar',
                                           sliderInput('opacity_error_bar', 'Escolha a opacidade das barras: ',
                                                       min = 0, max = 1, value = 1, step = 0.05
                                           )
                          )
                        ),
                        accordionItem(
                          title = "Configurações dos pontos",
                          status = accordionStatus,
                          collapsed = TRUE,
                          selectInput('markers_shape_error_bar', 'Escolha o formato: ',
                                      choices = c(
                                        'Circulos' = 'circle',
                                        'Quadrados' = 'square',
                                        'Triângulos' = 'triangle-up',
                                        'Formas diversas' = 'shapes'
                                      )
                          ),
                          materialSwitch(
                              label = 'Adicionar linha nos pontos:',
                              inputId = "line_markers_error_bar",
                              status = switchStatus,
                              value = FALSE
                            ),
                          sliderInput('size_markers_error_bar', 'Escolha o tamanho da fonte: ',
                                      min = 1, max = 35, value = 12, step = 1
                          ),
                          sliderInput('opacity_markers_error_bar', 'Determine a opacidade dos pontos: ',
                                      min = 0, max = 1, value = 1, step = 0.05
                          )
                        )
                      )
               )
    ),
    column(9,
fluidPage(fluidRow(column(
        h2("Gráfico de Erro",
               style="text-align:center; font-size:50px;"),
        tabsetPanel(type = 'tabs',
                    tabPanel(title = 'Gráfico',
                             shinycssloaders::withSpinner(
                               plotlyOutput('plotly_error_bar'),
                               type = spinnerType,
                               color = spinnerColor,
                               size = spinnerSize
                             ))
        ),
        width = 9
      )))
    ),
    column(12, hr())
  )
}