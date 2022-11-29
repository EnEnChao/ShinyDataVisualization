anova_mix_page <- function (){
  tabPanel(
    'ANOVA - medidas misturadas',
    column(12,
           h3('ANOVA - medidas misturadas', style="text-align:center; font-size:50px;"),
           column(12,
                  column(6,
                         h3(strong('Testando Normalidade', align = 'center')),
                         plotlyOutput('anova_mix_qq_plot'),
                         uiOutput('anova_mix_shapiro')
                  ),
                  column(6,
                         h3(strong('Verificando Outliers', align = 'center')),
                         plotlyOutput('anova_mix_box_plot')
                  )),
           column(12,
                  column(6,
                         h3(strong('Verificando Homogeneidade de Variância', align = 'center')),
                         DTOutput('anova_mix_levene'),
                  ),
                  column(6,
                         h3(strong('Verificando Homogeneidade de Covariância', align = 'center')),
                         DTOutput('anova_mix_boxm'),
                  )
           ),
           br(),
           column(12,
                  h3(strong('Esfericidade de Mauchly', align = 'center')),
                  DTOutput('anova_mix_mauchly_dt'),
                  uiOutput('anova_mix_mauchly_results'),
                  br(),
                  h3(strong('Resultado do teste de ANOVA', align = 'center')),
                  DTOutput('anova_mix_dt')
           )
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