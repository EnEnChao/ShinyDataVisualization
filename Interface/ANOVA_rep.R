anova_rep_page <- function (){
  tabPanel(
    'ANOVA - medidas repetidas',
    column(12,
           h3('ANOVA - medidas repetidas', style="text-align:center; font-size:50px;"),
           column(6,
                  h3(strong('Testando Normalidade', align = 'center')),
                  plotlyOutput('anova_rep_qq_plot'),
                  uiOutput('anova_rep_shapiro')
           ),
           column(6,
                  h3(strong('Verificando Outliers', align = 'center')),
                  plotlyOutput('anova_rep_box_plot')
           ),br(),
           column(12,
                  h3(strong('', align = 'center')),
                  h3(strong('Esfericidade de Mauchly', align = 'center')),
                  DTOutput('anova_rep_mauchly_dt'),
                  uiOutput('anova_rep_mauchly_results'),br(),
                  h3(strong('Resultado do teste de ANOVA', align = 'center')),
                  DTOutput('anova_rep_dt'),
                  uiOutput('anova_rep_p'),
                  h3(strong('Tabela Post Hoc', align = 'center')),
                  DTOutput('anova_rep_posthoc')
           )
      , align = 'center'
    ),
    column(12, hr())
  )
}