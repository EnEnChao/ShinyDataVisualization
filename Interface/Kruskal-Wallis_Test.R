kruskal_wallis_test_page <- function (){
  tabPanel(
    'Teste de Kruskal-Wallis',
    h3("Teste de Kruskal-Wallis", style="text-align:center; font-size:50px;"),
    column(
      12,
      h3(strong('Detectando Outliers')),
        plotlyOutput('kruskal_boxplot'),
        br(),
        h3(strong('Calculo do Teste de Kruskal Wallis')),
        DTOutput('kruskal_dt'),
        uiOutput('kruskal_interpretation'),
        br(),
        h3(strong('Múltiplas comparações entre pares')),
        column(6,
               h3(strong('Teste de Dunn')),
               DTOutput('kruskal_dunn_test')
        ),
        column(6,
               h3(strong('Teste de Wilcoxon')),
               DTOutput('kruskal_wilcoxon_test')
        )
      , align = 'center'
    ),
    column(12, hr())
  )
}