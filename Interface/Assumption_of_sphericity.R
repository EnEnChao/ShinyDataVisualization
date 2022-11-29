sphericity_page <- function (){
  tabPanel(
    'Avaliando a esfericidade',
    column(12,
           h3("Avaliando a esfericidade", style="text-align:center; font-size:50px;"),
           h3(strong('ANOVA')),
           DTOutput('sphericity_anova_test'),
           h3(strong('Teste de Esfericidade de Mauchly')),
           DTOutput('mauchly_test'),
           h3(strong('Correções de esfericidade')),
           fluidRow(
             column(6, h4('Correção Greenhouse-Geisser', align = 'center'), DTOutput('sphericity_corrections_gg')),
             column(6, h4('Correção Huynh-Feldt', align = 'center'), DTOutput('sphericity_corrections_hf'))
           ),
           h3(strong('Resultados:')),
           uiOutput('sphericity_statistics')
           , align = 'center'
    ),
    column(12,hr())
  )
}