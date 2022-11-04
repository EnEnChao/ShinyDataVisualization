manova_page <- function (){
  tabPanel(
    'MANOVA',
    tabsetPanel(
      tabPanel('MANOVA',
               h3("MANOVA - Análise de Variância Multivariada", style="text-align:center; font-size:50px;"),
               uiOutput('manova_statistics')
      ),
      tabPanel('Verificações Unidimensionais',
               uiOutput('manova_unidimensional_assumptions')
      )
    ),
    column(12, hr())
  )
}