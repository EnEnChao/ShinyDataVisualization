anova_page <- function (){
  tabPanel(
    'ANOVA',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           )
    ),
    column(9,
           h3('ANOVA', style="text-align:center; font-size:50px;"),
           uiOutput('anova_statistics'),
           align = 'center'
    ),
    column(12, hr())
  )
}