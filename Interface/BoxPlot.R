box_plot_page <- function (){
  tabPanel('Box Plot',
           column(3,
                  column(12,
                    h3(strong("Controle de opções:"), align = 'center'),
                  { accordion(
               id = "accordion_box_plots",
               accordionItem(
                 title = "Algorítmo para o cálculo dos quartis:",
                 status = accordionStatus,
                 collapsed = TRUE,
                 radioGroupButtons('box_algorithm', '',
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
                   inputId = "meanline_box",
                   status = switchStatus,
                   value = TRUE
                 )
               ),
               accordionItem(
                 title = "Configurações dos pontos",
                 status = accordionStatus,
                 collapsed = TRUE,
                 materialSwitch(
                   label = 'Adicionar pontos:',
                   inputId = "point_box",
                   status = switchStatus,
                   value = FALSE
                 ),
                 conditionalPanel(condition = 'input.point_box',
                                  sliderInput(
                                    label = 'Distância entre os pontos:',
                                    inputId = "jitter_box",
                                    min = 0, max = 1, value = 0.7, step = 0.01
                                  ),
                                  sliderInput(
                                    label = 'Posição dos pontos:',
                                    inputId = "jitter_pointpos",
                                    min = -2, max = 2, value = 0, step = 0.01
                                  ),
                                  sliderInput(
                                    label = 'Tamanho:',
                                    inputId = "points_size_box",
                                    min = 1, max = 20, value = 6, step = 1
                                  ),
                                  sliderInput(
                                    label = 'Largura da borda dos pontos:',
                                    inputId = "points_width_box",
                                    min = 0, max = 10, value = 0, step = 0.25
                                  ),
                                  sliderInput(
                                    label = 'Opacidade:',
                                    inputId = "points_opacity_box",
                                    min = 0, max = 1, value = 1, step = 0.01
                                  ),
                                  radioGroupButtons(
                                    inputId = "dot_box_shape",
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
             ) }
                  ),
                  column(12,
                    h3(strong("Controle do layout:"), align = 'center'),
                  {
                         column(12,
                         accordion(
                           id = 'accordion_box_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_box",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_box',
                                              pickerInput(
                                                inputId = "bgcolor_box",
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
                                              conditionalPanel(condition = 'input.bgcolor_box == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_box",
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
                               inputId = "legend_box",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_box',
                                              textInput(
                                                inputId = 'title_legend_box',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_box",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_box",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_box",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_box",
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
                               inputId = "axis_box",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_box',
                                              textInput(
                                                inputId = 'axis_x_box',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_box',
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
           fluidPage(fluidRow(column(9,
            div(h3("Box Plot",
                   style="text-align:center; font-size:50px;")),
                        tabPanel(title = 'Gráfico',
                                 shinycssloaders::withSpinner(
                                   plotlyOutput('plotly_box_plot'),
                                   type = spinnerType,
                                   color = spinnerColor,
                                   size = spinnerSize
                                 )))
           ))),
           column(12, hr())
  )
}