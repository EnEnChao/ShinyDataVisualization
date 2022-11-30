friedman_test_page <- function (){
  tabPanel(
    'Teste de Friedman',
      tabPanel('Teste de Friedman',
               h3("Teste de Friedman", style="text-align:center; font-size:50px;"),
               column(
                 12,
                 h3(strong('Detectando Outliers')),
                 shinycssloaders::withSpinner(
                   plotlyOutput('friedman_boxplot'),
                   type = spinnerType,
                   color = spinnerColor,
                   size = spinnerSize
                 ),
                 br(),
                 h3(strong('Calculo do Teste de friedman Wallis')),
                 shinycssloaders::withSpinner(
                   DTOutput('friedman_dt'),
                   type = spinnerType,
                   color = spinnerColor,
                   size = spinnerSize
                 ),
                 uiOutput('friedman_interpretation'),
                 h3(strong('Área de Efeito')),
                 shinycssloaders::withSpinner(
                   DTOutput('friedman_effectArea'),
                   type = spinnerType,
                   color = spinnerColor,
                   size = spinnerSize
                 ),
                 uiOutput('friedman_effectArea_interpretation'),
                 br(),
                 h3(strong('Múltiplas comparações entre pares')),
                 column(6,
                        h3(strong('Teste de Wilcoxon')),
                        shinycssloaders::withSpinner(
                          DTOutput('friedman_wilcoxon_test'),
                          type = spinnerType,
                          color = spinnerColor,
                          size = spinnerSize
                        )
                 ),
                 column(6,
                        h3(strong('Teste do Sinal')),
                        shinycssloaders::withSpinner(
                          DTOutput('friedman_sign_test'),
                          type = spinnerType,
                          color = spinnerColor,
                          size = spinnerSize
                        )
                 )
                 , align = 'center'
               )
      ),
    column(12, hr())
  )
}