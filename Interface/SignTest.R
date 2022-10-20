sign_test_page <- function (){
    tabPanel('Teste do Sinal',
         column(3,
                h3(strong("Controle do layout:"), align = 'center'),{
         }

         ),
           column(9,
                  h3('Teste do Sinal', style="text-align:center; font-size:50px;"),
                  br(),
                  uiOutput('sign_test_results'),
                  align = 'center'
           ),
           column(12, hr())
  )
}