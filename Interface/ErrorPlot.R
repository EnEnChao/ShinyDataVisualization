error_plot_page <- function (){
  tabPanel(
    'Gráfico de Erro',

    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
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
                                selectInput('error_algorithm', '',
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
           ) }
           ),
           column(12,
                  h3(strong("Controle do layout:"), align = 'center'),
           {
                         column(12,
                         accordion(
                           id = 'accordion_error_bar_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_error_bar",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_error_bar',
                                              pickerInput(
                                                inputId = "bgcolor_error_bar",
                                                label = "Escolha a cor de fundo: ",
                                                choices = c(
                                                  "Branco" = 'white',
                                                  "Preto" = 'black',
                                                  "Cinza" = 'grey',
                                                  'Cinza claro' = 'lightgrey',
                                                  'Personalizada' = 'personal'
                                                ),
                                                selected = 'white'
                                              ),
                                              conditionalPanel(condition = 'input.bgcolor_error_bar == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_error_bar",
                                                                                         label = "Choose colour",
                                                                                         value = "red",
                                                                                         showColour = 'background'
                                                               )
                                              )
                             )
                           )},
                           {
                           accordionItem(
                             title = 'Configurações da legenda',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Mostrar legenda:',
                               inputId = "legend_error_bar",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_error_bar',
                                              textInput(
                                                inputId = 'title_legend_error_bar',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_error_bar",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_error_bar",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_error_bar",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_error_bar",
                                                label = 'Orientação da legenda:',
                                                choices = c(
                                                  'Vertical' = 'v',
                                                  'Horizontal' = 'h'
                                                ),
                                                selected = 'v'
                                              )
                             )
                           )},
                           #Nomes dos eixos
                           accordionItem(
                             title = 'Eixos',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Nome dos eixos padrões:',
                               inputId = "axis_error_bar",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_error_bar',
                                              textInput(
                                                inputId = 'axis_x_error_bar',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_error_bar',
                                                label = 'Digite o nome do eixo Y:',
                                                value = 'Frequência'
                                              )
                             )
                           )
                         )
                         )}
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