kruskal_wallis_test_page <- function (){
  tabPanel(
    'Teste de Kruskal-Wallis',
    h3("Teste de Kruskal-Wallis", style="text-align:center; font-size:50px;"),
    column(
      12,
      h3(strong('Detectando Outliers')),
      shinycssloaders::withSpinner(
        plotlyOutput('kruskal_boxplot'),
        type = spinnerType,
        color = spinnerColor,
        size = spinnerSize
      ),
      br(),
      h3(strong('Calculo do Teste de Kruskal Wallis')),
      shinycssloaders::withSpinner(
        DTOutput('kruskal_dt'),
        type = spinnerType,
        color = spinnerColor,
        size = spinnerSize
      ),
      uiOutput('kruskal_interpretation'),
      br(),
      h3(strong('Múltiplas comparações entre pares')),
      column(6,
             h3(strong('Teste de Dunn')),
             shinycssloaders::withSpinner(
               DTOutput('kruskal_dunn_test'),
               type = spinnerType,
               color = spinnerColor,
               size = spinnerSize
             )
      ),
      column(6,
             h3(strong('Teste de Wilcoxon')),
             shinycssloaders::withSpinner(
               DTOutput('kruskal_wilcoxon_test'),
               type = spinnerType,
               color = spinnerColor,
               size = spinnerSize
             )
      )
      , align = 'center'
    ),
    column(12, hr())
  )
}