ancova_page <- function (){
  tabPanel(
    'ANCOVA',
    h3("ANCOVA - Análise de Covariância", style="text-align:center; font-size:50px;"),
    column(6,
          h3(strong('Teste de Linearidade'), align = 'center'),
           shinycssloaders::withSpinner(
                       plotOutput('ancova_linearity'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     )
    ),
    column(6,
           h3(strong('Regressão de Homogeniedade'), align = 'center'),
           shinycssloaders::withSpinner(
                       DTOutput('ancova_regression'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
           uiOutput('ancova_regression_results'),
           align = 'center'
    ),
    column(12,), br(),
    column(12,
          column(6,
                 h3(strong('Homogeniedade das Variâncias'), align = 'center'),
                 shinycssloaders::withSpinner(
                       DTOutput('ancova_levene_test'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 uiOutput('ancova_levene_res'),
                 align = 'center'
          ),
          column(6,
                 h3(strong('Teste de Normalidade'), align = 'center'),
                 shinycssloaders::withSpinner(
                       DTOutput('ancova_shapiro_test'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 uiOutput('ancova_shapiro_res'),
                 align = 'center'
          )
    ),
    column(12,
           h3(strong('Resultados:'), align = 'center'),
           br(),
           shinycssloaders::withSpinner(
                       DTOutput('ancova_dt_res'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
           uiOutput('ancova_results'),
           h3(strong('Tabela Posthoc:'), align = 'center'),
           shinycssloaders::withSpinner(
                       DTOutput('ancova_posthoc'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
           align = 'center'
    ),
    column(12, hr())
  )
}
#Constroi a tabela de levene
ancova_levene_test <- function (df){
  levene <- leveneTest(aov(vd ~ cov + vi, data = df)$residuals ~ df$vi)
  levene <- data.frame(Estatística = levene$`F value`[1], Df1 = levene$Df[1], Df2 = levene$Df[2], p = levene$`Pr(>F)`[1])
  levene <- signif(levene, significancia_de_aproximacao)
  rownames(levene) <- 'Teste de Levene'

  return(levene)
}

#Constroi a tabela de shapiro wilk
ancova_shapiro_test <- function (df){
  shapiro <- shapiro.test(aov(vd ~ cov + vi, data = df)$residuals)
  shapiro <- data.frame(Estatística = shapiro$statistic, p = shapiro$p.value)
  shapiro <- signif(shapiro, significancia_de_aproximacao)
  rownames(shapiro) <- 'Teste de Shapiro-Wilk'
  return(shapiro)
}

#Constroi a tabela posthoc
posthoc_ancova_table <- function (df){
  posthoc <- as.data.frame(
    df %>% emmeans_test(vd ~ vi, covariate = cov, p.adjust.method = 'bonferroni')
  )
  posthoc <- posthoc[-c(1, 2, 8, 9)]
  posthoc$statistic <- signif(posthoc$statistic, significancia_de_aproximacao)
  posthoc$p <- signif(posthoc$p, significancia_de_aproximacao)
  names(posthoc) <- c('Grupo 1','Grupo 2', 'df', 'Estatistica', 'p')
  posthoc$`Significância` <- lapply(posthoc$p, function (x){
    return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
  })

  return(posthoc)
}