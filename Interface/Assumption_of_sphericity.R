sphericity_page <- function (){
  tabPanel(
    'Avaliando a esfericidade',
    column(3,
      h3(strong("Controle de opções:"), align = 'center'),
    { accordion(
      id = 'accordion_sphericity',
      accordionItem(
        title = 'Testes estatísticos',
        status = accordionStatus,
        collapsed = FALSE,
        numericInput('esfericity_ci', 'Escolha o intervalo de confiança: ',
                            min = 0, max = 1, value = 0.95, step = 0.01
        ),
        actionButton("load_esfericity",
                     strong('Carregue!'),
                     style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                     width = "80%",
                     class = "btn-info"
        ),
        br(),
        conditionalPanel(condition = 'input.load_esfericity >= 1',
                                    pickerInput(
                                      inputId = "sphericity_picker",
                                      label = "Selecione os testes a serem mostrados",
                                      choices = c(
                                        'ANOVA' = 'anova',
                                        'Teste de Mauchly' = 'mauchly',
                                        'Correções' = 'corrections',
                                        'Resultados' = 'results'
                                      ),
                                      selected = c('anova', 'mauchly', 'corrections', 'results'),
                                      options = list(
                                        `actions-box` = TRUE,
                                        size = 10,
                                        `selected-text-format` = "count > 3"
                                      ),
                                      multiple = TRUE
                                    )
        ),
        uiOutput('sphericity_correc_anova')
      )
    )}
    ),
    column(9,
           fluidPage(fluidRow(column(9, align = 'center',
                                     h3("Avaliando a esfericidade", style="text-align:center; font-size:50px;"),
                                     uiOutput('sphericity_results')
           )))
    ),
    column(12,hr())
  )
}