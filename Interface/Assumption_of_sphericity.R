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
        actionButton("load_sphericity",
                     strong('Carregue!'),
                     style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                     width = "80%",
                     class = "btn-info"
        ),
        br(),
        selectInput(
          'sphericity_correc_anova_2',
          label = 'Escolha a correção para a tabela ANOVA: ',
          choices = c('auto', 'GG', 'HF', 'none'),
          selected = 'auto'
        )
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