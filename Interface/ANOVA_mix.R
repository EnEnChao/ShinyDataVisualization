anova_mix_page <- function (){
  tabPanel(
    'ANOVA - medidas misturadas',
    column(12,
           h3('ANOVA - medidas misturadas', style="text-align:center; font-size:50px;"),
           column(12,
                  column(6,
                         h3(strong('Testando Normalidade', align = 'center')),
                         shinycssloaders::withSpinner(
                           plotlyOutput('anova_mix_qq_plot'),
                           type = spinnerType,
                           color = spinnerColor,
                           size = spinnerSize
                         ),
                         uiOutput('anova_mix_shapiro')
                  ),
                  column(6,
                         h3(strong('Verificando Outliers', align = 'center')),
                         shinycssloaders::withSpinner(
                           plotlyOutput('anova_mix_box_plot'),
                           type = spinnerType,
                           color = spinnerColor,
                           size = spinnerSize
                         )
                  )),
           column(12,
                  column(6,
                         h3(strong('Verificando Homogeneidade de Variância', align = 'center')),
                         shinycssloaders::withSpinner(
                           DTOutput('anova_mix_levene'),
                           type = spinnerType,
                           color = spinnerColor,
                           size = spinnerSize
                         )
                  ),
                  column(6,
                         h3(strong('Verificando Homogeneidade de Covariância', align = 'center')),
                         shinycssloaders::withSpinner(
                           DTOutput('anova_mix_boxm'),
                           type = spinnerType,
                           color = spinnerColor,
                           size = spinnerSize
                         )
                  )
           ),
           br(),
           column(12,
                  h3(strong('Esfericidade de Mauchly', align = 'center')),
                  shinycssloaders::withSpinner(
                    DTOutput('anova_mix_mauchly_dt'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  ),
                  uiOutput('anova_mix_mauchly_results'),
                  br(),
                  h3(strong('Resultado do teste de ANOVA', align = 'center')),
                  shinycssloaders::withSpinner(
                    DTOutput('anova_mix_dt'),
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

anova_mix_pairwise_page <- function (){
    tabPanel(
    'ANOVA - medidas misturadas - testes pareados',
    column(12,
           h3('ANOVA - medidas misturadas - testes pareados', style="text-align:center; font-size:50px;"),
           h3('Tabela de ambos grupos pareados'),
           column(6,
                  shinycssloaders::withSpinner(
                    DTOutput('anova_mix_pairwise_1'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  )
           ),
           column(6,
                  shinycssloaders::withSpinner(
                    DTOutput('anova_mix_pairwise_2'),
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