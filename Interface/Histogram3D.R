histogram3d_page <- function (){
  tabPanel(
    'Histograma 3D',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
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
           ) }
           ),
           column(12,
             h3(strong("Controle do layout:"), align = 'center'),
                  {
                         column(12,
                         accordion(
                           id = 'accordion_histogram_3d_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_histogram_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_histogram_3d',
                                              pickerInput(
                                                inputId = "bgcolor_histogram_3d",
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
                                              conditionalPanel(condition = 'input.bgcolor_histogram_3d == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_histogram_3d",
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
                               inputId = "legend_histogram_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_histogram_3d',
                                              textInput(
                                                inputId = 'title_legend_histogram_3d',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_histogram_3d",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_histogram_3d",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_histogram_3d",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_histogram_3d",
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
                               inputId = "axis_histogram_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_histogram_3d',
                                              textInput(
                                                inputId = 'axis_x_histogram_3d',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_histogram_3d',
                                                label = 'Digite o nome do eixo Y:',
                                                value = 'Frequência'
                                              ),
                                              textInput(
                                                inputId = 'axis_z_histogram_3d',
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