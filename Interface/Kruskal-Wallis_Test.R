kruskal_wallis_test_page <- function (){
  tabPanel(
    'Teste de Kruskal-Wallis',
      tabPanel('',
               h3("Teste de Kruskal-Wallis", style="text-align:center; font-size:50px;"),
               uiOutput('kruskal_test_statistics')
      ),
    column(12, hr())
  )
}