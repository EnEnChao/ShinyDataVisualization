anova_mix_page <- function (){
  tabPanel(
    'ANOVA - medidas misturadas',
    column(12,
           h3('ANOVA - medidas misturadas', style="text-align:center; font-size:50px;"),
           uiOutput('anova_mix_statistics')
           , align = 'center'
    ),
    column(12, hr())
  )
}

anova_mix_pairwise_page <- function (){
    tabPanel(
    'ANOVA - medidas misturadas - testes pareados',
    column(12,
           h3('ANOVA - medidas misturadas - testes pareados', style="text-align:center; font-size:50px;"),
           h3('Tabela de ambos grupos pareados'),
           column(6,
                  DTOutput('anova_mix_pairwise_1')
           ),
           column(6,
                  DTOutput('anova_mix_pairwise_2')
           )
           , align = 'center'
    ),
    column(12, hr())
  )
}