anova_rep_page <- function (){
  tabPanel(
    'ANOVA - medidas repetidas',
    column(12,
           h3('ANOVA - medidas repetidas', style="text-align:center; font-size:50px;"),
           column(6,
                  h3(strong('Testando Normalidade', align = 'center')),
                  shinycssloaders::withSpinner(
                    plotlyOutput('anova_rep_qq_plot'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  ),
                  uiOutput('anova_rep_shapiro')
           ),
           column(6,
                  h3(strong('Verificando Outliers', align = 'center')),
                  shinycssloaders::withSpinner(
                    plotlyOutput('anova_rep_box_plot'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  )
           ),br(),
           column(12,
                  h3(strong('', align = 'center')),
                  h3(strong('Esfericidade de Mauchly', align = 'center')),
                  shinycssloaders::withSpinner(
                    DTOutput('anova_rep_mauchly_dt'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  ),
                  uiOutput('anova_rep_mauchly_results'),br(),
                  h3(strong('Resultado do teste de ANOVA', align = 'center')),
                  shinycssloaders::withSpinner(
                    DTOutput('anova_rep_dt'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  ),
                  uiOutput('anova_rep_p'),
                  h3(strong('Tabela Post Hoc', align = 'center')),
                  shinycssloaders::withSpinner(
                    DTOutput('anova_rep_posthoc'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  )
           )
      , align = 'center'
    ),
    column(12, hr())
  )
}