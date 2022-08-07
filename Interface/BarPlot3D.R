bar3D_page <- function (){
  tabPanel(
    'Gráfico em barras',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
             id = 'accordion_bar_plot_3d',
             accordionItem(
               title = "Algoritmo para o cálculo dos intervalos",
               status = accordionStatus,
               collapsed = TRUE,
               selectInput(
                 inputId = 'algorithm_bar3d',
                 label = 'Escolha o algoritmo para o cálculo dos intervalos:',
                 choices = c(
                   'Sturges' = 'Sturges',
                   'Scott' = 'Scott',
                   'Freedman-Diaconis' = 'FD'
                 )
               )
             ),
             accordionItem(
               title = "Configurações das barras",
               status = accordionStatus,
               collapsed = TRUE,
               numericInput(
                 label = 'Espaçamento entre as barras:',
                 inputId = 'spacing_bar_bar3d',
                 min = 0, value = 0.2, step = 0.1, max = NA
               ),
               sliderInput(
                 label = 'Opacidade das barras:',
                 inputId = 'opacity_bar_bar3d',
                 min = 0, value = 0.8, step = 0.01, max = 1
               )
             )
           ) }
          ),
           column(12,
             h3(strong("Controle do layout:"), align = 'center'),
                  {
                         column(12,
                         accordion(
                           id = 'accordion_bar_plot_3d_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_bar_plot_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_bar_plot_3d',
                                              pickerInput(
                                                inputId = "bgcolor_bar_plot_3d",
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
                                              conditionalPanel(condition = 'input.bgcolor_bar_plot_3d == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_bar_plot_3d",
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
                               inputId = "legend_bar_plot_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_bar_plot_3d',
                                              textInput(
                                                inputId = 'title_legend_bar_plot_3d',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_bar_plot_3d",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_bar_plot_3d",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_bar_plot_3d",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_bar_plot_3d",
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
                               inputId = "axis_bar_plot_3d",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_bar_plot_3d',
                                              textInput(
                                                inputId = 'axis_x_bar_plot_3d',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_bar_plot_3d',
                                                label = 'Digite o nome do eixo Y:',
                                                value = 'Frequência'
                                              ),
                                              textInput(
                                                inputId = 'axis_z_bar_plot_3d',
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
             div(h2("Gráfico de Barras 3d",
                    style="text-align:center; font-size:50px;")),
                         tabPanel(title = 'Gráfico',
                                  shinycssloaders::withSpinner(
                                    plotlyOutput('plotly_bar3d'),
                                    type = spinnerType,
                                    color = spinnerColor,
                                    size = spinnerSize
                                  )
                         ),
        width = 9
      )))
    ),
    column(12,hr())
  )
}