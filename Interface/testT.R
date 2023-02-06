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
  test_t2 <- data.frame(p = signif(test_t$p.value, 4), df = signif(test_t$parameter, 4), Tcalc = signif(test_t$estimate, 4))
  Ttab <- qt(intervalo_global_de_confianca, df = test_t2$df, lower.tail = F) %>% abs() %>% signif(digits = significancia_de_aproximacao)
  test_t2 <- cbind(test_t2, Ttab)
  rownames(test_t2) <- paste0('Test T - ', names(vector)[1])
  return(test_t2)
}