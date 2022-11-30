manova_page <- function (){
  tabPanel(
    'MANOVA',
    h3("MANOVA - Análise de Variância Multivariada", style="text-align:center; font-size:50px;"),
    column(12,
           h3(strong('Dados')),
           shinycssloaders::withSpinner(
             plotlyOutput('manova_boxplot'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
           ),
           h3(strong('Checando Normalidade Multivariada')),
           shinycssloaders::withSpinner(
             DTOutput('manova_normality_multi'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
           ),
           uiOutput('manova_interpret_normality_multi'),
           align = 'center'
       ),
       br(),
       column(6,
              h3(strong('Checando Tamanho da Amostra')),
              shinycssloaders::withSpinner(
                DTOutput('manova_sample_size_dt'),
                type = spinnerType,
                color = spinnerColor,
                size = spinnerSize
              ),
              uiOutput('manova_sample_size_assumpt'),
              br(),
              h3(strong('Checando Homogeniedade das Covariâncias')),
              shinycssloaders::withSpinner(
                DTOutput('manova_covariancia_dt'),
                type = spinnerType,
                color = spinnerColor,
                size = spinnerSize
              ),
              uiOutput('manova_covariancia_assumpt'),
              align = 'center'
       ),
       column(6,
              h3(strong('Identificando Multicollinearidade')),
              shinycssloaders::withSpinner(
                DTOutput('manova_multicollinearity'),
                type = spinnerType,
                color = spinnerColor,
                size = spinnerSize
              ),
              h3(strong('Checando Homogeniedade das Variâncias')),
              shinycssloaders::withSpinner(
                DTOutput('manova_variance'),
                type = spinnerType,
                color = spinnerColor,
                size = spinnerSize
              ),
              align = 'center'
       ),br(),
     column(12,
            h3(strong('Resultado do teste de MANOVA')),
            shinycssloaders::withSpinner(
                verbatimTextOutput('manova_dt'),
                type = spinnerType,
                color = spinnerColor,
                size = spinnerSize
              ),
            uiOutput('manova_res'),
            align = 'center'
     ),
    column(12, hr())
  )
}

manova_univariable_assumptions_page <- function (){
  tabPanel(
    'MANOVA Verificações Univariadas',
    h3("MANOVA - Verificações Unidimensionais", style="text-align:center; font-size:50px;"),
    column(12,
           h3(strong('Checando Normalidade')),
           shinycssloaders::withSpinner(
             plotlyOutput('manova_normality_uni'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
           ),
           h3(strong('Testes pareados de Shapiro Wilk')),
           shinycssloaders::withSpinner(
             DTOutput('manova_shapiro_uni'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
           ),
           h3(strong('Checando Linearidade')),
           shinycssloaders::withSpinner(
             plotOutput('manova_linearity_plot'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
           ),
           align = 'center'
    ),
    column(12, hr())
  )
}