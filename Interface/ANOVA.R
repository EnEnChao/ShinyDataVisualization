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
           column(6,
               h3(strong('Testando Normalidade', align = 'center')),
               plotlyOutput('anova_qq_plot'),
               uiOutput('anova_shapiro')

        ),
        column(6,
               h3(strong('Verificando Outliers', align = 'center')),
               plotlyOutput('anova_box_plot'),
               DTOutput('anova_outliers')
        ), br(),
        column(12,
               h3(strong('Verificando da Homogeneidade de Variância', align = 'center')),
               DTOutput('anova_levene_dt'),
               uiOutput('anova_levene_results'), br(),
               h3(strong('Resultado do teste de ANOVA', align = 'center')),
               DTOutput('anova_dt'),
               uiOutput('anova_p'),
               h3(strong('Tabela Post Hoc', align = 'center')),
               DTOutput('anova_posthoc')
        ),
           align = 'center'
    ),
    column(12, hr())
  )
}