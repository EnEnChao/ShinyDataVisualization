violin_page <- function (){
  tabPanel(
    'Violino',
    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           column(12,
                  accordion(
                    id = "accordion_violin_plots",
                    accordionItem(
                      title = "Algorítmo para o cálculo dos quartis:",
                      status = accordionStatus,
                      collapsed = TRUE,
                      radioGroupButtons('violin_algorithm','',
                                        choices = c(
                                          'Linear' = 'linear',
                                          'Inclusivo' = 'inclusive',
                                          'Exclusivo' = 'exclusive'
                                        ),
                                        selected = 'linear',
                                        status = radioGroupStatus,
                                        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                      ),
                      materialSwitch(
                        label = 'Média visivel:',
                        inputId = "meanline_violin",
                        status = switchStatus,
                        value = TRUE
                      ),
                      materialSwitch(
                        label = 'Largura de Banda padrão (silverman\'s rule of thumb): ',
                        inputId = "bandwidth_violin",
                        status = switchStatus,
                        value = TRUE
                      ),
                      conditionalPanel(condition = '!input.bandwidth_violin',
                                       sliderInput(
                                         label = 'Escolha a largura de Banda:',
                                         inputId = "bandwidth_violin_size",
                                         min = 1, max = 50, value = 7, step = 1
                                       )
                      )
                    ),
                    accordionItem(
                      title = "Configurações dos pontos",
                      status = accordionStatus,
                      collapsed = TRUE,
                      materialSwitch(
                        label = 'Adicionar pontos:',
                        inputId = "point_violin",
                        status = switchStatus,
                        value = FALSE
                      ),
                      conditionalPanel(condition = 'input.point_violin',
                                       sliderInput(
                                         label = 'Distância entre os pontos:',
                                         inputId = "jitter_violin",
                                         min = 0, max = 1, value = 0.7, step = 0.01
                                       ),
                                       sliderInput(
                                         label = 'Posição dos pontos:',
                                         inputId = "jitter_pointpos",
                                         min = -2, max = 2, value = 0, step = 0.01
                                       ),
                                       sliderInput(
                                         label = 'Tamanho:',
                                         inputId = "points_size_violin",
                                         min = 1, max = 20, value = 6, step = 1
                                       ),
                                       sliderInput(
                                         label = 'Largura da borda dos pontos:',
                                         inputId = "points_width_violin",
                                         min = 0, max = 10, value = 0, step = 0.25
                                       ),
                                       sliderInput(
                                         label = 'Opacidade:',
                                         inputId = "points_opacity_violin",
                                         min = 0, max = 1, value = 1, step = 0.01
                                       ),
                                       radioGroupButtons(
                                         inputId = "dot_violin_shape",
                                         label = "Escolha o formado dos pontos: ",
                                         choices = c(
                                           'Circulos' = 'circle',
                                           'X' = 'x',
                                           'Linhas' = 'line-ns'
                                         ),
                                         selected = 'circle',
                                         status = radioGroupStatus,
                                         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                       )
                      )
                    )
                  )
           )
    ),
    column(9,
           fluidPage(fluidRow(column(
             h2("Gráfico de Violino", style="text-align:center; font-size:50px;"),
             tabPanel(title = 'Gráfico',
                      shinycssloaders::withSpinner(
                        plotlyOutput('plotly_violin'),
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