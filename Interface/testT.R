test_t_page <- function (){
  tabPanel('Teste T',
         column(3,
                h3(strong("Controle do layout:"), align = 'center'),{
           accordion(
             id = 'accordion_t_test',
             accordionItem(
               title = 'Testes estatísticos',
               status = accordionStatus,
               collapsed = FALSE,
               selectInput('test_t_options', 'Escolha o teste com as variáveis',
                           choices = c('One way' = 'one',
                                       'Two ways' = 'two'),
                           selected = 'one'
               ),
               conditionalPanel(condition = 'input.test_t_options == "one"',
                                numericInput('test_t_mu', 'Valor verdadeiro da média:', value = 0, step = 1),
               ),
               uiOutput('test_t_variable'),
               actionButton("load_t_test",
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
                  h3('Teste T', style="text-align:center; font-size:50px;"),
                  br(),
                  uiOutput('t_test_results'),
                  align = 'center'
           ),
           column(12, hr())
  )
}