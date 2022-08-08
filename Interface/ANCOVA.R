ancova_page <- function (){
  tabPanel(
    'ANCOVA',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
             id = 'accordion_ancova',
             accordionItem(
               title = 'Variáveis',
               status = accordionStatus,
               collapsed = FALSE,
               uiOutput('ancova_variables')
             ),
             accordionItem(
               title = 'Configurações do gráfico',
               status = accordionStatus,
               collapsed = TRUE,
               numericInput(
                 inputId = 'ancova_ci',
                 label = 'Intervalo de confiança',
                 min = 0, max = 1, value = 0.05
               ),
               selectInput(
                 inputId = 'ancova_sumsq',
                 label = 'Algoritmo para soma dos quadrados',
                 choices = c(2, 3),
                 selected = 2
               )
             ),
             accordionItem(
               title = "Configurações das linhas",
               status = accordionStatus,
               collapsed = TRUE,
               numericInput(
                 'ancova_line_width',
                 'Largura das linhas',
                 min = 0, value = 3
               )
             ),
             accordionItem(
               title = "Configurações dos pontos",
               status = accordionStatus,
               collapsed = TRUE,
               sliderInput(
                 label = 'Determine a opacidade:',
                 inputId = "ancova_marker_opacity",
                 min = 0.01, max = 1, value = 0.8, step = 0.01
               ),
               numericInput(
                 label = 'Determine o tamanho:',
                 inputId = "ancova_marker_size",
                 min = 0, value = 4
               )
             )
           ) }
           ),
           column(12,
             h3(strong("Controle do layout:"), align = 'center'),
           {
                         column(12,
                         accordion(
                           id = 'accordion_ancova_plot_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_ancova_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_ancova_plot',
                                              pickerInput(
                                                inputId = "bgcolor_ancova_plot",
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
                                              conditionalPanel(condition = 'input.bgcolor_ancova_plot == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_ancova_plot",
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
                               inputId = "legend_ancova_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_ancova_plot',
                                              textInput(
                                                inputId = 'title_legend_ancova_plot',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_ancova_plot",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_ancova_plot",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_ancova_plot",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_ancova_plot",
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
                               inputId = "axis_ancova_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_ancova_plot',
                                              textInput(
                                                inputId = 'axis_x_ancova_plot',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_ancova_plot',
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
             h3("ANCOVA - Análise de Covariância", style="text-align:center; font-size:50px;"),
             tabPanel(title = 'Gráfico',
                      shinycssloaders::withSpinner(
                        uiOutput('plotly_ancova'),
                        type = spinnerType,
                        color = spinnerColor,
                        size = spinnerSize
                      ),
                      uiOutput('ancova_statistics')
        ),
        width = 9
      )))
    ),
    column(12, hr())
  )
}