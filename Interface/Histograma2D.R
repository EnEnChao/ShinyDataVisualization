histogram2d_page <- function (){
  tabPanel(
    'Histograma',
    column(3,
           column(12,
                  h3(strong("Controle de opções:"), align = 'center'),
                  conditionalPanel(condition = 'input.histogram_tabs == "linear_histogram"',{
                         column(12,
                                accordion(
                                  id = 'accordion_linear_histogram',
                                  accordionItem(
                                    title = 'Configurações do histograma',
                                    status = accordionStatus,
                                    collapsed = TRUE,
                                    materialSwitch(
                                      label = 'Histograma empilhado:',
                                      inputId = "switch_stack_histogram",
                                      status = switchStatus,
                                      value = FALSE
                                    ),
                                    materialSwitch(
                                      label = 'Espaçamento:',
                                      inputId = "bargap_histogram",
                                      status = switchStatus,
                                      value = FALSE
                                    ),
                                    conditionalPanel(condition = 'input.bargap_histogram',
                                                     sliderInput(
                                                      label = 'Escolha o tamanho do espaçamento:',
                                                      inputId = "bargap_histogram_level",
                                                      min = 0, max = 2, value = 0.5, step = 0.001
                                                    )
                                    ),
                                    sliderInput(
                                      label = 'Determine a opacidade:',
                                      inputId = "opacity_histogram",
                                      min = 0.01, max = 1, value = 0.8, step = 0.01
                                    ),
                                    materialSwitch(
                                      label = 'Intervalo padrão do histograma: ',
                                      inputId = "bins_histogram",
                                      status = switchStatus,
                                      value = TRUE
                                    ),
                                    conditionalPanel(condition = '!input.bins_histogram',
                                                     sliderInput(
                                                       label = 'Quantidade de bandas:',
                                                       inputId = "bandwidth_histogram",
                                                       min = 1, max = 100, value = 13, step = 1
                                                     )
                                    )
                                  ),
                                  accordionItem(
                                    title = 'Configurações gráfico de densidade:',
                                    status = accordionStatus,
                                    collapsed = TRUE,
                                    materialSwitch(
                                      label = 'Mostrar gráfico de densidade: ',
                                      inputId = "show_density_histogram",
                                      status = switchStatus,
                                      value = FALSE
                                    ),
                                    conditionalPanel(condition = 'input.show_density_histogram',
                                                     sliderInput(
                                                       label = 'Opacidade da linha do histograma:',
                                                       inputId = "density_histogram_line_opacity",
                                                       min = 0, max = 1, value = 1, step = 0.01
                                                     ),
                                                     numericInput(
                                                       label = 'Escala do gráfico:',
                                                       inputId = "density_histogram_scale",
                                                       min = 0, value = 1
                                                     ),
                                                     materialSwitch(
                                                       label = 'Preencher área: ',
                                                       inputId = "density_histogram_area",
                                                       status = switchStatus,
                                                       value = TRUE
                                                     ),
                                                     conditionalPanel(condition = 'input.density_histogram_area',
                                                                      sliderInput(
                                                                       label = 'Opacidade da area do histograma:',
                                                                       inputId = "density_histogram_area_opacity",
                                                                       min = 0, max = 1, value = 0.3, step = 0.01
                                                                     )
                                                     )

                                    )
                                  )
                                )
                         )
                       }),
                  conditionalPanel(condition = 'input.histogram_tabs == "ridges_histogram"', {
                         column(12,
                         { accordion(
                           id = 'accordion_ridges_histogram',
                           accordionItem(
                             title = 'Configurações do histograma',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Mostrar quartis:',
                               inputId = "show_quartis_ridges_histogram",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.show_quartis_ridges_histogram',
                                              numericInput(
                                                inputId = 'n_quartis_ridges_histogram',
                                                label = 'Número de quartis',
                                                min = 0, value = 4, max = NA, step = 1
                                              )
                             ),
                             sliderInput(
                               label = 'Largura de banda:',
                               inputId = "bandwidth_ridges_histogram",
                               min = 1, max = 30, value = 13, step = 1
                             ),
                             sliderInput(
                               label = 'Altura:',
                               inputId = 'scale_ridges_histogram',
                               min = 0.1, max = 4, value = 0.8, step = 0.1
                             )
                             #  ,sliderInput(
                             #   label = 'Determine a opacidade:',
                             #   inputId = "opacity_ridges_histogram",
                             #   min = 0.1, max = 1, value = 0.9, step = 0.1
                             # )
                           ),
                           accordionItem(
                             title = 'Configurações dos pontos',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Mostrar pontos:',
                               inputId = "points_ridges_histogram",
                               status = switchStatus,
                               value = FALSE
                             ),
                             conditionalPanel(condition = 'input.points_ridges_histogram',
                                              sliderInput(
                                                label = 'Tamanho:',
                                                inputId = 'points_size_ridges_histogram',
                                                min = 1, max = 20, value = 3, step = 1
                                              ),
                                              sliderInput(
                                                label = 'Opacidade:',
                                                inputId = 'points_opacity_ridges_histogram',
                                                min = 0, max = 1, value = 1, step = 0.01
                                              ),
                                              selectInput(
                                                label = 'Formato:',
                                                inputId = 'points_shape_ridges_histogram',
                                                choices = c(
                                                  'Ponto' = 'o',
                                                  'Linha' = '|',
                                                  'X' = 'x'
                                                ),
                                                selected = '|'
                                              ),
                                              selectInput(
                                                label = 'Posição:',
                                                inputId = 'points_position_ridges_histogram',
                                                choices = c(
                                                  'Identidade' = 'identity',
                                                  'Dentro do gráfico' = 'points_sina',
                                                  'Espalhados' = 'points_jitter',
                                                  'Abaixo do gráfico' = 'raincloud'
                                                )
                                              )

                             )
                           ),
                           accordionItem(
                             title = 'Cor do Histograma',
                             status = accordionStatus,
                             collapsed = TRUE,
                             radioGroupButtons(
                               inputId = "ridges_color",
                               label = "Diferente cores para o histograma: ",
                               choices = c(
                                 'A' = 'viridis',
                                 'B' = 'magma',
                                 'C' = 'inferno',
                                 'D' = 'plasma',
                                 'E' = 'cividis'
                               ),
                               selected = 'viridis',
                               status = switchStatus,
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                             ),
                             materialSwitch(
                               label = 'Estilo de cor reverso:',
                               inputId = "reverse_ridges_histogram",
                               status = switchStatus,
                               value = FALSE
                             )
                           )
                         ) }
                         ) }),
           ),
           column(12,
                  h3(strong("Controle do layout:"), align = 'center'),
                  conditionalPanel(condition = 'input.histogram_tabs == "linear_histogram"',{
                         column(12,
                         accordion(
                           id = 'accordion_linear_histogram_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_linear_histogram",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_linear_histogram',
                                              pickerInput(
                                                inputId = "bgcolor_linear_histogram",
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
                                              conditionalPanel(condition = 'input.bgcolor_linear_histogram == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_linear_histogram",
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
                               inputId = "legend_linear_histogram",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_linear_histogram',
                                              textInput(
                                                inputId = 'title_legend_linear_histogram',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_linear_histogram",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_linear_histogram",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_linear_histogram",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_linear_histogram",
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
                               inputId = "axis_linear_histogram",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_linear_histogram',
                                              textInput(
                                                inputId = 'axis_x_linear_histogram',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_linear_histogram',
                                                label = 'Digite o nome do eixo Y:',
                                                value = 'Frequência'
                                              )
                             )
                           )
                         )
                         )}),
                  conditionalPanel(condition = 'input.histogram_tabs == "ridges_histogram"',{
                         column(12,
                         accordion(
                           id = 'accordion_ridges_histogram_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_ridges_histogram",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_ridges_histogram',
                                              pickerInput(
                                                inputId = "bgcolor_ridges_histogram",
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
                                              conditionalPanel(condition = 'input.bgcolor_ridges_histogram == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_ridges_histogram",
                                                                                         label = "Choose colour",
                                                                                         value = "red",
                                                                                         showColour = 'background'
                                                               )
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
                               inputId = "axis_ridges_histogram",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_ridges_histogram',
                                              textInput(
                                                inputId = 'axis_x_ridges_histogram',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_ridges_histogram',
                                                label = 'Digite o nome do eixo Y:',
                                                value = 'Frequência'
                                              )
                             )
                           )
                         )
                         )})
           )
    ),
    column(9,
      fluidPage(fluidRow(column(
        h2("Histograma",
               style="text-align:center; font-size:50px;"),
        tabsetPanel(id = 'histogram_tabs', type = 'tabs',
                    tabPanel(id = 'linear_histogram', value = 'linear_histogram',title = 'Gráfico',
                             shinycssloaders::withSpinner(
                               plotlyOutput('plotly_linear_histogram'),
                               type = spinnerType,
                               color = spinnerColor,
                               size = spinnerSize
                             )),
                    tabPanel(id = 'ridges_histogram',value = 'ridges_histogram', title = 'Gráfico - Ridges',
                             shinycssloaders::withSpinner(
                               plotOutput('plot_histogram'),
                               type = spinnerType,
                               color = spinnerColor,
                               size = spinnerSize
                             ),
                      wellPanel(h4(strong('Baixe o gráfico em png: ( esta ação pode demorar um pouco )')),
                      downloadButton('download_histogram', 'Baixe :'))
                    )
        ),
        width = 9
      )))
    ),
    column(12,hr())
  )
}