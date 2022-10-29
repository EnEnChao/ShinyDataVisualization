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
                                       'Two ways' = 'two',
                                       'Pareado' = 'paired'),
                           selected = 'two'
               ),
               conditionalPanel(condition = 'input.test_t_options == "one"',
                                numericInput('test_t_mu', 'Valor verdadeiro da média:', value = 0, step = 1),
               )
             )
           )
         }

         ),
           column(9,
                  h3('Teste T', style="text-align:center; font-size:50px;"),
                  uiOutput('t_test_predict'),
                  align = 'center'
           ),
           column(12, hr())
  )
}

test_t_uni <- function (vector, mu){
  test_t <- t.test(vector, mu = mu)
  test_t2 <- data.frame(p = signif(test_t$p.value, 4), estatística = signif(test_t$estimate, 4), df = signif(test_t$parameter, 4))
  rownames(test_t2) <- paste0('Test T - ', names(vector)[1])
  return(test_t2)
}