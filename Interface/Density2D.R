density2D_page <- function (){
  tabPanel(
    'Gráfico de Densidade',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
             id = 'accordion_density_plot',
             accordionItem(
               title = "Configurações do gráfico de densidade",
               status = accordionStatus,
               collapsed = TRUE,
               materialSwitch(
                 label = 'Mostrar Área:',
                 inputId = "area_density_plot",
                 status = switchStatus,
                 value = TRUE
               ),
               materialSwitch(
                 label = 'Mostrar linhas:',
                 inputId = "line_density_plot",
                 status = switchStatus,
                 value = TRUE
               )
             ),
             accordionItem(
               title = "Estimativa de densidade kernel",
               status = accordionStatus,
               collapsed = TRUE,
               selectInput(
                 inputId = 'algorithm_density_plot',
                 label = 'Escolha a estimativa de densidade kernel: ',
                 choices = c(
                   'Gaussiana' = 'gaussian',
                   'Retangular' = 'rectangular',
                   'Triangular' = 'triangular',
                   'Epanechnikov' = 'epanechnikov',
                   'Biweight' = 'biweight',
                   'Cosseno' = 'cosine',
                   'Optcosseno' = 'optcosine'
                 ),
                 selected = 'gaussian'
               )
             )
           ) }
         ),
           column(12,
                  h3(strong("Controle do layout:"), align = 'center'),
           {
                         column(12,
                         accordion(
                           id = 'accordion_density_plot_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_density_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_density_plot',
                                              pickerInput(
                                                inputId = "bgcolor_density_plot",
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
                                              conditionalPanel(condition = 'input.bgcolor_density_plot == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_density_plot",
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
                               inputId = "legend_density_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_density_plot',
                                              textInput(
                                                inputId = 'title_legend_density_plot',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_density_plot",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_density_plot",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_density_plot",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_density_plot",
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
                               inputId = "axis_density_plot",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_density_plot',
                                              textInput(
                                                inputId = 'axis_x_density_plot',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_density_plot',
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
        h2("Gráfico de Densidade",
               style="text-align:center; font-size:50px;"),
        tabsetPanel(type = 'tabs',
                    tabPanel(title = 'Gráfico',
                             shinycssloaders::withSpinner(
                               plotlyOutput('plotly_density_plot'),
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