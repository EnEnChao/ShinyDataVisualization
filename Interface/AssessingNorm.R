check_norm_page <- function (){
  tabPanel(
    'Distribuição de dados Normais',
    column(12,
           tabsetPanel(id = 'check_norm_tabs', type = 'tabs',
                       tabPanel(id = 'check_norm_d', value = 'check_norm_d',title = 'Gráfico de densidade',
                                column(3,
                                  #Layout do Gráfico de densidade
                                { accordion(
                                         id = 'accordion_linear_check_norm_d_layout',
                                         #Cores de fundo
                                       {
                                      accordionItem(
                                        title = 'Cores de fundo',
                                        status = accordionStatus,
                                        collapsed = TRUE,
                                        materialSwitch(
                                          label = 'Cores de fundo padrões:',
                                          inputId = "colors_check_norm_d",
                                          status = switchStatus,
                                          value = TRUE
                                        ),
                                        conditionalPanel(condition = '!input.colors_check_norm_d',
                                                         pickerInput(
                                                           inputId = "bgcolor_check_norm_d",
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
                                                         conditionalPanel(condition = 'input.bgcolor_check_norm_d == "personal" ',
                                                                          colourpicker::colourInput(inputId = "personal_bgcolor_check_norm_d",
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
                                          inputId = "legend_check_norm_d",
                                          status = switchStatus,
                                          value = TRUE
                                        ),
                                        conditionalPanel(condition = 'input.legend_check_norm_d',
                                                         textInput(
                                                           inputId = 'title_legend_check_norm_d',
                                                           label = 'Digite o título da legenda',
                                                           value = ''
                                                         ),
                                                         materialSwitch(
                                                           label = 'Borda na legenda: ',
                                                           inputId = "border_legend_check_norm_d",
                                                           status = switchStatus,
                                                           value = FALSE
                                                         ),
                                                         materialSwitch(
                                                           label = 'Título da legenda em negrito: ',
                                                           inputId = "bold_title_legend_check_norm_d",
                                                           status = switchStatus,
                                                           value = FALSE
                                                         ),
                                                         radioGroupButtons(
                                                           inputId = "item_size_legend_check_norm_d",
                                                           label = 'Tamanho do item na legenda:',
                                                           choices = c(
                                                             'Constante' = 'constant',
                                                             'Variavel' = 'trace'
                                                           )
                                                         ),
                                                         radioGroupButtons(
                                                           inputId = "orientation_legend_check_norm_d",
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
                                              inputId = "axis_check_norm_d",
                                              status = switchStatus,
                                              value = TRUE
                                            ),
                                            conditionalPanel(condition = '!input.axis_check_norm_d',
                                                             textInput(
                                                               inputId = 'axis_x_check_norm_d',
                                                               label = 'Digite o nome do eixo X:',
                                                               value = 'Dados'
                                                             ),
                                                             textInput(
                                                               inputId = 'axis_y_check_norm_d',
                                                               label = 'Digite o nome do eixo Y:',
                                                               value = 'Frequência'
                                                             )
                                            )
                                          )
                                       )}
                                ),

                                column(9,
                                       #Gráfico de densidade
                                       h3("Gráfico de densidade", style="text-align:center; font-size:50px;"),
                                       shinycssloaders::withSpinner(
                                         plotlyOutput('plotly_norm_density'),
                                         type = spinnerType,
                                         color = spinnerColor,
                                         size = spinnerSize
                                       )
                                )
                       ),
                       tabPanel(id = 'check_norm_qq', value = 'check_norm_qq',title = 'Gráfico quantile-quantile',
                                column(3,
                                  #Layout do Gráfico quantile-quantile
                                {
                         column(12,
                         accordion(
                           id = 'accordion_check_norm_qq_layout',
                           #Cores de fundo
                           {
                           accordionItem(
                             title = 'Cores de fundo',
                             status = accordionStatus,
                             collapsed = TRUE,
                             materialSwitch(
                               label = 'Cores de fundo padrões:',
                               inputId = "colors_check_norm_qq",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.colors_check_norm_qq',
                                              pickerInput(
                                                inputId = "bgcolor_check_norm_qq",
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
                                              conditionalPanel(condition = 'input.bgcolor_check_norm_qq == "personal" ',
                                                               colourpicker::colourInput(inputId = "personal_bgcolor_check_norm_qq",
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
                               inputId = "legend_check_norm_qq",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = 'input.legend_check_norm_qq',
                                              textInput(
                                                inputId = 'title_legend_check_norm_qq',
                                                label = 'Digite o título da legenda',
                                                value = ''
                                              ),
                                              materialSwitch(
                                               label = 'Borda na legenda: ',
                                               inputId = "border_legend_check_norm_qq",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              materialSwitch(
                                               label = 'Título da legenda em negrito: ',
                                               inputId = "bold_title_legend_check_norm_qq",
                                               status = switchStatus,
                                               value = FALSE
                                              ),
                                              radioGroupButtons(
                                                inputId = "item_size_legend_check_norm_qq",
                                                label = 'Tamanho do item na legenda:',
                                                choices = c(
                                                  'Constante' = 'constant',
                                                  'Variavel' = 'trace'
                                                )
                                              ),
                                              radioGroupButtons(
                                                inputId = "orientation_legend_check_norm_qq",
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
                               inputId = "axis_check_norm_qq",
                               status = switchStatus,
                               value = TRUE
                             ),
                             conditionalPanel(condition = '!input.axis_check_norm_qq',
                                              textInput(
                                                inputId = 'axis_x_check_norm_qq',
                                                label = 'Digite o nome do eixo X:',
                                                value = 'Dados'
                                              ),
                                              textInput(
                                                inputId = 'axis_y_check_norm_qq',
                                                label = 'Digite o nome do eixo Y:',
                                                value = 'Frequência'
                                              )
                             )
                           )
                         )
                         )}
                                ),
                                column(9,
                                       #Gráfico quantile-quantile
                                        h3("Gráfico quantile-quantile", style="text-align:center; font-size:50px;"),
                                        shinycssloaders::withSpinner(
                                          plotlyOutput('plotly_norm_qq'),
                                          type = spinnerType,
                                          color = spinnerColor,
                                          size = spinnerSize
                                        )
                                )
                       ),
                       tabPanel(title = 'Tabela de verificação de normalidade',
                                h3("Tabela de verificação de normalidade", style="text-align:center; font-size:50px;"),
                                tags$head(tags$style(HTML(".cell-border-right{border-right: 1px solid #000}"))),
                                uiOutput('check_norm_table')
                       )
           )
    ),
    column(12,hr())
  )
}