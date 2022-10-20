anova_rep_page <- function (){
  tabPanel(
    'ANOVA - medidas repetidas',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           )
    ),
    column(9,
           h3('ANOVA - medidas repetidas', style="text-align:center; font-size:50px;"),
           uiOutput('anova_rep_statistics')
           , align = 'center'
    ),
    column(12, hr())
  )
}