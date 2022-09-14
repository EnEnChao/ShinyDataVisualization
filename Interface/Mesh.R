mesh_page <- function (){
  tabPanel(
    'Gráfico em Mesh',
    column(3,
           column(12,
                  h3(strong("Controle de opções:"), align = 'center'),
                  h4('Escolha os gráficos a serem mostrados: ', align = 'center'),
                  uiOutput('checkbox_mesh_ui')
           ),
           column(12,
             # h3(strong("Controle do layout:"), align = 'center'),
             # h5('Em desenvolvimento.', align = 'center'),
                  # {
                  #        column(12,
                  #        accordion(
                  #          id = 'accordion_mesh_3d_layout',
                  #          #Cores de fundo
                  #          {
                  #          accordionItem(
                  #            title = 'Cores de fundo',
                  #            status = accordionStatus,
                  #            collapsed = TRUE,
                  #            materialSwitch(
                  #              label = 'Cores de fundo padrões:',
                  #              inputId = "colors_mesh_3d",
                  #              status = switchStatus,
                  #              value = TRUE
                  #            ),
                  #            conditionalPanel(condition = '!input.colors_mesh_3d',
                  #                             pickerInput(
                  #                               inputId = "bgcolor_mesh_3d",
                  #                               label = "Escolha a cor de fundo: ",
                  #                               choices = c(
                  #                                 "Branco" = 'white',
                  #                                 "Preto" = 'black',
                  #                                 "Cinza" = 'grey',
                  #                                 'Cinza claro' = 'lightgrey',
                  #                                 'Personalizada' = 'personal'
                  #                               ),
                  #                               selected = 'white'
                  #                             ),
                  #                             conditionalPanel(condition = 'input.bgcolor_mesh_3d == "personal" ',
                  #                                              colourpicker::colourInput(inputId = "personal_bgcolor_mesh_3d",
                  #                                                                        label = "Choose colour",
                  #                                                                        value = "red",
                  #                                                                        showColour = 'background'
                  #                                              )
                  #                             )
                  #            )
                  #          )},
                  #          {
                  #          accordionItem(
                  #            title = 'Configurações da legenda',
                  #            status = accordionStatus,
                  #            collapsed = TRUE,
                  #            materialSwitch(
                  #              label = 'Mostrar legenda:',
                  #              inputId = "legend_mesh_3d",
                  #              status = switchStatus,
                  #              value = TRUE
                  #            ),
                  #            conditionalPanel(condition = 'input.legend_mesh_3d',
                  #                             textInput(
                  #                               inputId = 'title_legend_mesh_3d',
                  #                               label = 'Digite o título da legenda',
                  #                               value = ''
                  #                             ),
                  #                             materialSwitch(
                  #                              label = 'Borda na legenda: ',
                  #                              inputId = "border_legend_mesh_3d",
                  #                              status = switchStatus,
                  #                              value = FALSE
                  #                             ),
                  #                             materialSwitch(
                  #                              label = 'Título da legenda em negrito: ',
                  #                              inputId = "bold_title_legend_mesh_3d",
                  #                              status = switchStatus,
                  #                              value = FALSE
                  #                             ),
                  #                             radioGroupButtons(
                  #                               inputId = "item_size_legend_mesh_3d",
                  #                               label = 'Tamanho do item na legenda:',
                  #                               choices = c(
                  #                                 'Constante' = 'constant',
                  #                                 'Variavel' = 'trace'
                  #                               )
                  #                             ),
                  #                             radioGroupButtons(
                  #                               inputId = "orientation_legend_mesh_3d",
                  #                               label = 'Orientação da legenda:',
                  #                               choices = c(
                  #                                 'Vertical' = 'v',
                  #                                 'Horizontal' = 'h'
                  #                               ),
                  #                               selected = 'v'
                  #                             )
                  #            )
                  #          )},
                  #          #Nomes dos eixos
                  #          accordionItem(
                  #            title = 'Eixos',
                  #            status = accordionStatus,
                  #            collapsed = TRUE,
                  #            materialSwitch(
                  #              label = 'Nome dos eixos padrões:',
                  #              inputId = "axis_mesh_3d",
                  #              status = switchStatus,
                  #              value = TRUE
                  #            ),
                  #            conditionalPanel(condition = '!input.axis_mesh_3d',
                  #                             textInput(
                  #                               inputId = 'axis_x_mesh_3d',
                  #                               label = 'Digite o nome do eixo X:',
                  #                               value = 'Dados'
                  #                             ),
                  #                             textInput(
                  #                               inputId = 'axis_y_mesh_3d',
                  #                               label = 'Digite o nome do eixo Y:',
                  #                               value = 'Frequência'
                  #                             ),
                  #                             textInput(
                  #                               inputId = 'axis_z_mesh_3d',
                  #                               label = 'Digite o nome do eixo Z:',
                  #                               value = 'Classificação'
                  #                             )
                  #            )
                  #          )
                  #        )
                  #        )}
           )
    ),
    column(9,
           fluidPage(fluidRow(column(
             h2("Gráfico em Mesh 3D", style="text-align:center; font-size:50px;"),
                         tabPanel(title = 'Gráfico',
                                  shinycssloaders::withSpinner(
                                    plotlyOutput('plotly_mesh3d'),
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