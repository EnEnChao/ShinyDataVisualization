dotplot3d_page <- function (){
  tabPanel(
    'Gráfico de pontos',
    column(3,
          column(12,
            h3(strong("Controle de opções:"), align = 'center'),
          { conditionalPanel(condition = 'input.dot_plot3d_tabs == "simple_dot_plot3d"',
                 column(12,
                        accordion(
                          id = 'accordion_dot_plot_3d',
                          accordionItem(
                            title = "Configurações dos pontos",
                            status = accordionStatus,
                            collapsed = TRUE,
                            numericInput(
                              inputId = 'bins_scatter3d',
                              label = 'Escolha de intervalo: ',
                              value = 1, min = 0, max = NA, step = 1
                            ),
                            materialSwitch(
                              label = 'Adicionar linha nos pontos:',
                              inputId = "line_markers_scatter3d",
                              status = switchStatus,
                              value = FALSE
                            ),
                            sliderInput(
                              inputId = 'size_markers_scatter3d',
                              label = 'Escolha o tamanho da fonte: ',
                              value = 8, min = 0, max = 20, step = 1
                            ),
                            sliderInput(
                              inputId = 'opacity_markers_scatter3d',
                              label = 'Determine a opacidade dos pontos: ',
                              value = 1, min = 0, max = 1, step = 0.05
                            ),
                            selectInput(
                              inputId = 'shape_markers_scatter3d',
                              label = 'Escolha o formato: ',
                              choices = c(
                                'Circulo' = 'circle',
                                'Circulo aberto' = 'circle-open',
                                'Cruz' = 'cross',
                                'Diamante' = 'diamond',
                                'Diamante aberto' = 'diamond-open'
                                # ,'Formas diversas' = 'shapes'
                              )
                            )
                          ),
                          accordionItem(
                            title = "Configurações das elipses",
                            status = accordionStatus,
                            collapsed = TRUE,
                            materialSwitch(
                              label = 'Adicionar ellipse:',
                              inputId = "ellipse_scatter3d",
                              status = switchStatus,
                              value = FALSE
                            ),
                            conditionalPanel(condition = 'input.ellipse_scatter3d',
                                             sliderInput(inputId = 'ci_ellipse3d',
                                                         label = 'Determine o intervalo de confiança da elipse: ',
                                                         min = 0.01, max = 0.99, value = 0.95, step = 0.01
                                             ),
                                             #Para adicionar a área da elipse
                                             materialSwitch(
                                               label = 'Adicionar area da ellipse:',
                                               inputId = "area_ellipse3d",
                                               status = switchStatus,
                                               value = TRUE
                                             ),
                                             conditionalPanel(condition = 'input.area_ellipse3d',
                                                              sliderInput(inputId = 'opacity_area_ellipse3d',
                                                                          label = 'Determine a opacidade da area da ellipse: ',
                                                                          min = 0, max = 1, value = 0.3, step = 0.05
                                                              )
                                             ),
                                             materialSwitch(
                                               label = 'Adicionar contorno da ellipse:',
                                               inputId = "line_ellipse3d",
                                               status = switchStatus,
                                               value = TRUE
                                             ),
                                             conditionalPanel(condition = 'input.line_ellipse3d',
                                                              sliderInput(inputId = 'opacity_line_ellipse3d',
                                                                          label = 'Determine a opacidade do contorno da ellipse: ',
                                                                          min = 0, max = 1, value = 1, step = 0.05
                                                              ),
                                                              sliderInput(inputId = 'width_line_ellipse3d',
                                                                          label = 'Determine a largura do contorno da ellipse: ',
                                                                          min = 1, max = 20, value = 2, step = 0.25
                                                              ),
                                                              selectInput(
                                                                inputId = 'ellipse3d_line_format',
                                                                label = 'Escolha o formato da linha da elipse:',
                                                                choices = c(
                                                                  'Linha' = 'solid',
                                                                  'Tracejada' = 'dash',
                                                                  'Tracejada longa' = 'longdash',
                                                                  'Pontilhada' = 'dot',
                                                                  'Pontilhada e tracejada' = 'dashdot',
                                                                  'Pontilhada e tracejada longa' = 'longdashdot'
                                                                )
                                                              )

                                             )
                            )
                          )
                        )
                 )
) },
            #Ao clicar no painel Gráfico de pontos 3D bee swarm
          {conditionalPanel(condition = 'input.dot_plot3d_tabs == "beeswarm_dot_plot3d"',
                  column(12,
                         accordion(
                           id = 'accordion_beeswarm3d',
                           accordionItem(
                             title = "Configurações do gráfico",
                              status = accordionStatus,
                              collapsed = TRUE,
                             numericInput(
                               label = 'Espaço entre os pontos',
                               inputId = 'spacing_markers_beeswarm3d',
                               min = 0, value = 1, step = 0.1, max = NA
                             ),
                             materialSwitch(
                               label = 'Gráfico de lado:',
                               inputId = "side_beeswarm3d",
                               status = switchStatus,
                               value = FALSE
                             ),
                             selectInput(
                               inputId = 'beeswarm3d_method',
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
                             conditionalPanel(condition = 'input.beeswarm3d_method == "swarm"',
                                              selectInput(
                                                inputId = 'priority_beeswarm3d',
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
                              selectInput('shape_markers_beeswarm3d', 'Escolha o formato: ',
                                          choices = c(
                                            'Circulos' = 'circle',
                                            'Quadrados' = 'square',
                                            'Triângulos' = 'triangle-up',
                                            'Formas diversas' = 'shapes'
                                          )
                              ),
                              materialSwitch(
                                  label = 'Adicionar linha nos pontos:',
                                  inputId = "line_markers_beeswarm3d",
                                  status = switchStatus,
                                  value = FALSE
                                ),
                              numericInput('size_markers_beeswarm3d', 'Escolha o tamanho da fonte: ',
                                          min = 1, value = 8, step = 1
                              ),
                              sliderInput('opacity_markers_beeswarm3d', 'Determine a opacidade dos pontos: ',
                                          min = 0, max = 1, value = 1, step = 0.05
                              )
                          )
                         )
                  )
)}
          ),
           column(12,
                  h3(strong("Controle do layout:"), align = 'center'),
                  {
                         column(12,
                         accordion(
                           id = 'accordion_dot_plot_3d_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_dot_plot_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_dot_plot_3d',
                                              pickerInput(
                                                inputId = "bgcolor_dot_plot_3d",
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
                                              conditionalPanel(condition = 'input.bgcolor_dot_plot_3d == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_dot_plot_3d",
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
                               inputId = "legend_dot_plot_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_dot_plot_3d',
                                              textInput(
                                                inputId = 'title_legend_dot_plot_3d',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_dot_plot_3d",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_dot_plot_3d",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_dot_plot_3d",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_dot_plot_3d",
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
                               inputId = "axis_dot_plot_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_dot_plot_3d',
                                              textInput(
                                                inputId = 'axis_x_dot_plot_3d',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_dot_plot_3d',
                                                label = 'Digite o nome do eixo Y:',
                                                value = 'Frequência'
                                              ),
                                              textInput(
                                                inputId = 'axis_z_dot_plot_3d',
                                                label = 'Digite o nome do eixo Z:',
                                                value = 'Classificação'
                                              )
                             )
                           )
                         )
                         )}
           )
    ),
    column(9,
           fluidPage(fluidRow(column(
        h2("Gráfico de pontos 3d",
               style="text-align:center; font-size:50px;"),
        tabsetPanel(type = 'tabs', id = 'dot_plot3d_tabs',
                    tabPanel(id = 'simple_dot_plot3d', value = 'simple_dot_plot3d', title = 'Gráfico de pontos simples',
                             shinycssloaders::withSpinner(
                               plotlyOutput('plotly_scatter3d'),
                               type = spinnerType,
                               color = spinnerColor,
                               size = spinnerSize
                              )),
                    tabPanel(id = 'beeswarm_dot_plot3d', value = 'beeswarm_dot_plot3d',title = 'Gráfico bee swarm',
                             shinycssloaders::withSpinner(
                               plotlyOutput('plotly_beeswarm3d'),
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