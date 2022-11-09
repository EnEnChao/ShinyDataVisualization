manova_page <- function (){
  tabPanel(
    'MANOVA',
    h3("MANOVA - Análise de Variância Multivariada", style="text-align:center; font-size:50px;"),
    column(12,
           h3(strong('Dados')),
              plotlyOutput('manova_boxplot'),
              h3(strong('Detectando Outliers Multivariados')),
              DTOutput('manova_outliers_multi'),
              h3(strong('Checando Normalidade Multivariada')),
              DTOutput('manova_normality_multi'),
              uiOutput('manova_interpret_normality_multi'),
              align = 'center'
       ),
       br(),
       column(6,
              h3(strong('Checando Tamanho da Amostra')),
              DTOutput('manova_sample_size_dt'),
              uiOutput('manova_sample_size_assumpt'),
              br(),
              h3(strong('Checando Homogeniedade das Covariâncias')),
              DTOutput('manova_covariancia_dt'),
              uiOutput('manova_covariancia_assumpt'),
              align = 'center'
       ),
       column(6,
              h3(strong('Identificando Multicollinearidade')),
              DTOutput('manova_multicollinearity'),
              h3(strong('Checando Homogeniedade das Variâncias')),
              DTOutput('manova_variance'),
              align = 'center'
       ),br(),
     column(12,
            h3(strong('Resultado do teste de MANOVA')),
            verbatimTextOutput('manova_dt'),
            uiOutput('manova_res'),
            align = 'center'
     ),
    column(12, hr())
  )
}

manova_unidimensional_assumptions_page <- function (){
  tabPanel(
    'MANOVA Verificações Unidimensionais',
    h3("MANOVA - Verificações Unidimensionais", style="text-align:center; font-size:50px;"),
    column(12,
           h3(strong('Checando Normalidade')),
           plotlyOutput('manova_normality_uni'),
           h3(strong('Testes pareados de Shapiro Wilk')),
           DTOutput('manova_shapiro_uni'),
           h3(strong('Checando Linearidade')),
           plotOutput('manova_linearity_plot'),
           align = 'center'
    ),
    column(12, hr())
  )
}