anova_mix_page <- function (){
  tabPanel(
    'ANOVA - medidas misturadas',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           )
    ),
    column(9,
           h3('ANOVA - medidas misturadas', style="text-align:center; font-size:50px;"),
           uiOutput('anova_mix_statistics')
           , align = 'center'
    ),
    column(12, hr())
  )
}