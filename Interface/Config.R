config_page <- function(){
  tabPanel(
    'Configurações',
    icon = icon('glyphicon glyphicon-wrench', lib = 'glyphicon'),
    column(12,
           h2("Configuração de opções extras", style="text-align:center; font-size:50px;"),
           h3(strong('Defina o valor das variáveis globais:')),
           numericInput('change_global_ci', 'Modificar intervalo de confiança', 0.05, 0.001, 0.999, 0.001),
           numericInput('change_global_signif', 'Modificar significancia de aproximacao', 4, 1, 10, 1),
           h3(strong('Defina as cores dos gráficos:')),
           materialSwitch(
             label = 'Cores de fundo padrões:',
             inputId = "default_plot_color",
             status = switchStatus,
             value = TRUE
           ),
           conditionalPanel(condition = '!input.default_plot_color',
                            pickerInput(
                              inputId = "bgcolor_plot_default",
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
                            conditionalPanel(condition = 'input.bgcolor_plot_default == "personal" ',
                                             colourpicker::colourInput(inputId = "personal_bgcolor_plot_default",
                                                                       label = "Choose colour",
                                                                       value = "red",
                                                                       showColour = 'background'
                                             )
                            )
           )
    ),
    column(12,hr())
  )
}