anova_page <- function (){
  tabPanel(
    'ANOVA',
    column(12,
           h3('ANOVA', style="text-align:center; font-size:50px;"),
           column(6,
               h3(strong('Testando Normalidade', align = 'center')),
                  shinycssloaders::withSpinner(
                    plotlyOutput('anova_qq_plot'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  ),
               uiOutput('anova_shapiro')

        ),
        column(6,
               h3(strong('Verificando Outliers', align = 'center')),
               shinycssloaders::withSpinner(
                 plotlyOutput('anova_box_plot'),
                 type = spinnerType,
                 color = spinnerColor,
                 size = spinnerSize
               ),
        ), br(),
        column(12,
               h3(strong('Verificando da Homogeneidade de Variância', align = 'center')),
               shinycssloaders::withSpinner(
                 DTOutput('anova_levene_dt'),
                 type = spinnerType,
                 color = spinnerColor,
                 size = spinnerSize
               ),
               uiOutput('anova_levene_results'), br(),
               h3(strong('Resultado do teste de ANOVA', align = 'center')),
               shinycssloaders::withSpinner(
                 DTOutput('anova_dt'),
                 type = spinnerType,
                 color = spinnerColor,
                 size = spinnerSize
               ),
               uiOutput('anova_p'),
               h3(strong('Tabela Post Hoc', align = 'center')),
               shinycssloaders::withSpinner(
                 DTOutput('anova_posthoc'),
                 type = spinnerType,
                 color = spinnerColor,
                 size = spinnerSize
               )
        ),
           align = 'center'
    ),
    column(12, hr())
  )
}