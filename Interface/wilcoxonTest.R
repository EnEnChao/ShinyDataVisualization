wilcoxon_t_page <- function (){
    tabPanel('Teste de Wilcoxon',
         column(3,
                h3(strong("Controle do layout:"), align = 'center'),{
           accordion(
             id = 'accordion_wilcoxon_test',
             accordionItem(
               title = 'Testes estatísticos',
               status = accordionStatus,
               collapsed = FALSE,
               selectInput('wilcoxon_test_options', 'Escolha o teste com as variáveis',
                           choices = c('One way' = 'one',
                                       'Two ways' = 'two'),
                           selected = 'one'
               ),
               uiOutput('wilcoxon_test_variable'),
               conditionalPanel(condition = 'input.wilcoxon_test_options == "one"',
                                numericInput('wilcoxon_t_mu', 'Valor verdadeiro da média:', value = 0, step = 1)
               ),
               actionButton("load_wilcoxon_test",
                     strong('Carregue!'),
                     style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                     width = "80%",
                     class = "btn-info"
               )
             )
           )
         }

         ),
           column(9,
                  h3('Teste de Wilcoxon', style="text-align:center; font-size:50px;"),
                  br(),
                  uiOutput('wilcoxon_test_results'),
                  align = 'center'
           ),
           column(12, hr())
  )
}