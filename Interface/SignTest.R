sign_test_page <- function (){
    tabPanel('Teste do Sinal',
         column(3,
                h3(strong("Controle do layout:"), align = 'center'),{
         }

         ),
           column(9,
                  h3('Teste do Sinal', style="text-align:center; font-size:50px;"),
                  br(),
                  plotlyOutput('sign_test_outliers'),
                  h3(strong('Estatísticas'), align = 'center'),
                  DTOutput('sign_test_dt'),
                  uiOutput('sign_test_p'),
                  align = 'center'
           ),
           column(12, hr())
  )
}