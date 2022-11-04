friedman_test_page <- function (){
  tabPanel(
    'Teste de Friedman',
      tabPanel('Teste de Friedman',
               h3("Teste de Friedman", style="text-align:center; font-size:50px;"),
               uiOutput('friedman_test_statistics')
      ),
    column(12, hr())
  )
}