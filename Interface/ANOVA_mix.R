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
           column(6,
                  h3(strong('Testando Normalidade', align = 'center')),
                  plotlyOutput('anova_mix_qq_plot'),
                  uiOutput('anova_mix_shapiro')

           ),
          column(6,
                 h3(strong('Verificando Outliers', align = 'center')),
                 plotlyOutput('anova_mix_box_plot'),
                 DTOutput('anova_mix_outliers')
          ),br(),
           column(12,
                  h3(strong('Verificando da Homogeneidade de Variância', align = 'center')),
                  uiOutput('anova_mix_levene_results'),br(),
                  h3(strong('Resultado do teste de ANOVA', align = 'center')),
                  DTOutput('anova_mix_dt'),
                  uiOutput('anova_mix_statistics'),
                  h3(strong('Tabela Post Hoc', align = 'center')),
                  DTOutput('anova_mix_posthoc')
           ), align = 'center'
    ),
    column(12, hr())
  )
}