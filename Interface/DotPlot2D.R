dotplot2d_page <- function (){
  tabPanel(
    'Gráfico de Pontos',
    column(3,
           column(12,
              h3(strong("Controle de opções:"), align = 'center'),
             #Ao clicar no painel Gráfico de pontos simples
           { conditionalPanel(condition = 'input.dot_plot_tabs == "simple_dot_plot"',
                         column(12,
                                accordion(
                                  id = 'accordion_simple_dot_plot',
                                  accordionItem(
                                    title = "Configurações dos pontos",
                                    status = accordionStatus,
                                    collapsed = TRUE,
                                    selectInput('shape_markers_dot_plot', 'Escolha o formato: ',
                                                choices = c(
                                                  'Circulos' = 'circle',
                                                  'Quadrados' = 'square',
                                                  'Triângulos' = 'triangle-up',
                                                  'Formas diversas' = 'shapes'
                                                )
                                    ),
                                    materialSwitch(
                                        label = 'Adicionar linha nos pontos:',
                                        inputId = "line_markers_dot_plot",
                                        status = switchStatus,
                                        value = FALSE
                                      ),
                                    sliderInput('size_markers_dot_plot', 'Escolha o tamanho da fonte: ',
                                                min = 1, max = 35, value = 12, step = 1
                                    ),
                                    sliderInput('opacity_markers_dot_plot', 'Determine a opacidade dos pontos: ',
                                                min = 0, max = 1, value = 1, step = 0.05
                                    ),
                                    numericInput(
                                      inputId = 'bins_dot_plot',
                                      label = 'Escolha o intervalo entre os valores: ',
                                      value = 1, min = 0, max = NA, step = 1
                                    )
                                ),
                                  accordionItem(
                                    title = "Configurações das linhas",
                                    status = accordionStatus,
                                    collapsed = TRUE,
                                    materialSwitch(
                                      label = 'Adicionar Linhas:',
                                      inputId = "line_dot_plot",
                                      status = switchStatus,
                                      value = FALSE
                                    ),
                                    conditionalPanel(condition = 'input.line_dot_plot == true',
                                                     sliderInput(inputId = 'opacity_line_dot_plot',
                                                                 label = 'Determine a opacidade das linhas: ',
                                                                 min = 0, max = 1, value = 1, step = 0.05
                                                     )
                                    )
                                  ),
                                  accordionItem(
                                    title = "Configurações das elipses",
                                    status = accordionStatus,
                                    collapsed = TRUE,
                                    #Para adicionar uma elipse
                                    materialSwitch(
                                      label = 'Adicionar ellipse:',
                                      inputId = "ellipse_dot_plot",
                                      status = switchStatus,
                                      value = FALSE
                                    ),
                                    conditionalPanel(condition = 'input.ellipse_dot_plot',
                                                     #Para adicionar a área da elipse
                                                     selectInput(
                                                       inputId = 'ellipse_line_format',
                                                       label = 'Escolha o formato da linha da elipse:',
                                                       choices = c(
                                                         'Linha' = 'solid',
                                                         'Tracejada' = 'dash',
                                                         'Tracejada longa' = 'longdash',
                                                         'Pontilhada' = 'dot',
                                                         'Pontilhada e tracejada' = 'dashdot',
                                                         'Pontilhada e tracejada longa' = 'longdashdot'
                                                       )
                                                     ),

                                                     materialSwitch(
                                                       label = 'Adicionar area da ellipse:',
                                                       inputId = "ellipse_area_dot_plot",
                                                       status = switchStatus,
                                                       value = TRUE
                                                     ),
                                                     #Opacidade da área da elipse
                                                     conditionalPanel(condition = 'input.ellipse_area_dot_plot',
                                                                      sliderInput(inputId = 'area_opacity_ellipse',
                                                                                  label = 'Determine a opacidade da area da ellipse: ',
                                                                                  min = 0, max = 1, value = 0.3, step = 0.05
                                                                      )
                                                     ),
                                                     #Intervalo de confiança da elipse
                                                     sliderInput(inputId = 'ci_ellipse',
                                                                 label = 'Determine o intervalo de confiança da ellipse: ',
                                                                 min = 0.01, max = 0.99, value = 0.95, step = 0.01
                                                     )

                                    )
                                  )
                                )
                         )
      )},
             #Ao clicar no painel Gráfico de pontos bee swarm
           {conditionalPanel(condition = 'input.dot_plot_tabs == "beeswarm_dot_plot"',
                        column(12,
                               accordion(
                                 id = 'accordion_beeswarm',
                                 accordionItem(
                                   title = "Configurações do gráfico",
                                    status = accordionStatus,
                                    collapsed = TRUE,
                                   numericInput(
                                     label = 'Tamanho do gráfico',
                                     inputId = 'size_beeswarm',
                                     min = 0, value = 1, step = 0.1, max = NA
                                   ),
                                   numericInput(
                                     label = 'Espaço entre os pontos',
                                     inputId = 'spacing_markers_beeswarm',
                                     min = 0, value = 1, step = 0.1, max = NA
                                   ),
                                   materialSwitch(
                                     label = 'Gráfico de lado:',
                                     inputId = "side_beeswarm",
                                     status = switchStatus,
                                     value = FALSE
                                   ),
                                   materialSwitch(
                                     label = 'Subgráficos:',
                                     inputId = "subplots_beeswarm",
                                     status = switchStatus,
                                     value = FALSE
                                   ),
                                   conditionalPanel(condition = '!input.subplots_beeswarm',
                                                    numericInput(
                                                      inputId = 'width_beeswarm',
                                                      label = 'Espaçamento entre os gráficos',
                                                      min = 0, value = 0.4, step = 0.1
                                                    ),
                                   ),
                                   selectInput(
                                     inputId = 'beeswarm_method',
                                     label = 'Escolha o método: ',
                                     choices = c(
                                       'Swarm' = 'swarm',
                                       'Swarm compacto' = 'compactswarm',
                                       'Centro' = 'center',
                                       'Hexagonal' = 'hex',
                                       'Quadrado' = 'square'
                                     ),
                                     selected = 'swarm'
                                   ),
                                   conditionalPanel(condition = 'input.beeswarm_method == "swarm"',
                                                    selectInput(
                                                      inputId = 'priority_beeswarm',
                                                      label = 'Escolha a prioridade: ',
                                                      choices = c(
                                                        'Crescente' = 'ascending',
                                                        'Decrescente' = 'descending',
                                                        'Aleatoria' = 'random',
                                                        'Densidade' = 'density',
                                                        'Nenhum' = 'none'
                                                      ),
                                                      selected = 'ascending'
                                                    ),
                                   )
                                 ),
                                 accordionItem(
                                    title = "Configurações dos pontos",
                                    status = accordionStatus,
                                    collapsed = TRUE,
                                    selectInput('shape_markers_beeswarm', 'Escolha o formato: ',
                                                choices = c(
                                                  'Circulos' = 'circle',
                                                  'Quadrados' = 'square',
                                                  'Triângulos' = 'triangle-up',
                                                  'Formas diversas' = 'shapes'
                                                )
                                    ),
                                    materialSwitch(
                                        label = 'Adicionar linha nos pontos:',
                                        inputId = "line_markers_beeswarm",
                                        status = switchStatus,
                                        value = FALSE
                                      ),
                                    numericInput('size_markers_beeswarm', 'Escolha o tamanho da fonte: ',
                                                min = 1, value = 8, step = 1
                                    ),
                                    sliderInput('opacity_markers_beeswarm', 'Determine a opacidade dos pontos: ',
                                                min = 0, max = 1, value = 1, step = 0.05
                                    )
                                )
                               )
                        )
      )}
           ),
           column(12,
             h3(strong("Controle do layout:"), align = 'center'),
                  conditionalPanel(condition = 'input.dot_plot_tabs == "simple_dot_plot"',{
                         column(12,
                         accordion(
                           id = 'accordion_simple_dot_plot_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_simple_dot_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_simple_dot_plot',
                                              pickerInput(
                                                inputId = "bgcolor_simple_dot_plot",
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
                                              conditionalPanel(condition = 'input.bgcolor_simple_dot_plot == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_simple_dot_plot",
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
                               inputId = "legend_simple_dot_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_simple_dot_plot',
                                              textInput(
                                                inputId = 'title_legend_simple_dot_plot',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_simple_dot_plot",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_simple_dot_plot",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_simple_dot_plot",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_simple_dot_plot",
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
                               inputId = "axis_simple_dot_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_simple_dot_plot',
                                              textInput(
                                                inputId = 'axis_x_simple_dot_plot',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_simple_dot_plot',
                                                label = 'Digite o nome do eixo Y:',
                                                value = 'Frequência'
                                              )
                             )
                           )
                         )
                         )}),
                  conditionalPanel(condition = 'input.dot_plot_tabs == "beeswarm_dot_plot"',{
                         column(12,
                         accordion(
                           id = 'accordion_beeswarm_dot_plot_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_beeswarm_dot_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_beeswarm_dot_plot',
                                              pickerInput(
                                                inputId = "bgcolor_beeswarm_dot_plot",
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
                                              conditionalPanel(condition = 'input.bgcolor_beeswarm_dot_plot == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_beeswarm_dot_plot",
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
                               inputId = "legend_beeswarm_dot_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_beeswarm_dot_plot',
                                              textInput(
                                                inputId = 'title_legend_beeswarm_dot_plot',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_beeswarm_dot_plot",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_beeswarm_dot_plot",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_beeswarm_dot_plot",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_beeswarm_dot_plot",
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
                               inputId = "axis_beeswarm_dot_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_beeswarm_dot_plot',
                                              textInput(
                                                inputId = 'axis_x_beeswarm_dot_plot',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_beeswarm_dot_plot',
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
        div(h2("Gráfico de pontos",
               style="text-align:center; font-size:50px;")),
        tabsetPanel(type = 'tabs', id = 'dot_plot_tabs',
                    tabPanel(id = 'simple_dot_plot', value = 'simple_dot_plot',title = 'Gráfico de pontos simples',
                             shinycssloaders::withSpinner(
                               plotlyOutput('plotly_dot_plot'),
                               type = spinnerType,
                               color = spinnerColor,
                               size = spinnerSize
                             )),
                    tabPanel(id = 'beeswarm_dot_plot', value = 'beeswarm_dot_plot',title = 'Gráfico bee swarm',
                             shinycssloaders::withSpinner(
                               plotlyOutput('plotly_beeswarm_dot_plot'),
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