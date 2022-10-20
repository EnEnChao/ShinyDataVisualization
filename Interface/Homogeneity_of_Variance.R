homogenity_var_page <- function (){
  tabPanel(
    title = 'Homogeniedade das variâncias',
    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
             id = 'accordion_homogenity',
             accordionItem(
               title = 'Testes estatísticos',
               status = accordionStatus,
               collapsed = FALSE,
               selectInput(
                 'homogenity_tests',
                 'Escolha o teste: ',
                 choices = c(
                   'Teste de Levene' = 'levene_test',
                   'Teste F' = 'f_test',
                   'Teste de Bartlett' = 'bartlett_test',
                   'Teste de Fligner-Killeen' = 'fk_test'
                 ),
                 selected = 'levene_test'
               ),
               conditionalPanel(condition = 'input.homogenity_tests == "f_test"',
                                uiOutput('var_f_test')
               ),
               numericInput('homogenity_ci', 'Escolha o intervalo de confiança: ',
                            min = 0, max = 1, value = 0.95, step = 0.01
               )
             )
           )}
    ),
    column(9,
           fluidPage(fluidRow(column(9,
                                     h3("Homogeniedade das Variâncias", style="text-align:center; font-size:50px;"),
                                     br(),
                                     uiOutput('homogenity_results'),
                                     align = 'center'
           )))
    ),
    column(12, hr())
  )
}