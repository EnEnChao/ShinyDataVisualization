sign_test_page <- function (){
    tabPanel('Teste do Sinal',
             column(12,
                    h3('Teste do Sinal', style="text-align:center; font-size:50px;"),
                    br(),
                    shinycssloaders::withSpinner(
                      plotlyOutput('sign_test_outliers'),
                      type = spinnerType,
                      color = spinnerColor,
                      size = spinnerSize
                    ),
                    h3(strong('Resultados do Teste de Sinal:'), align = 'center'),
                    shinycssloaders::withSpinner(
                      DTOutput('sign_test_dt'),
                      type = spinnerType,
                      color = spinnerColor,
                      size = spinnerSize
                    ),
                    uiOutput('sign_test_p'),
                    align = 'center'
             ),
             column(12, hr())
  )
}