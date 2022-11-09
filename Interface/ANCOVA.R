ancova_page <- function (){
  tabPanel(
    'ANCOVA',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
             id = 'accordion_ancova',
             accordionItem(
               title = 'Configurações do gráfico',
               status = accordionStatus,
               collapsed = TRUE,
               numericInput(
                 inputId = 'ancova_ci',
                 label = 'Intervalo de confiança',
                 min = 0, max = 1, value = 0.05
               ),
               selectInput(
                 inputId = 'ancova_sumsq',
                 label = 'Algoritmo para soma dos quadrados',
                 choices = c(2, 3),
                 selected = 2
               )
             )
           )}
           )
    ),
    column(9,
           fluidPage(fluidRow(column(
             h3("ANCOVA - Análise de Covariância", style="text-align:center; font-size:50px;"),
             tabPanel(title = 'Gráfico',
                      h3(strong('ANCOVA'), align = 'center'),
        column(6,
               h3(strong('Teste de Linearidade'), align = 'center'),
               plotOutput('ancova_linearity')
        ),
        column(6,
               h3(strong('Regressão de Homogeniedade'), align = 'center'),
               DTOutput('ancova_regression'),
               uiOutput('ancova_regression_results')

        ),
        column(12,), br(),
        column(12,
          column(6,
                 h3(strong('Homogeniedade das Variâncias'), align = 'center'),
                 DTOutput('ancova_levene_test')
          ),
          column(6,
                 h3(strong('Teste de Normalidade'), align = 'center'),
                 DTOutput('ancova_shapiro_test')
          )
        ),
        column(12,
          h3(strong('Tabela Posthoc:'), align = 'center'),
          DTOutput('ancova_posthoc'),
          h3(strong('Resultados:'), align = 'center'),
          br(),
          uiOutput('ancova_results')
        )
        ),
        width = 9
      )))
    ),
    column(12, hr())
  )
}
#Constroi a tabela de levene
ancova_levene_test <- function (df){
  levene <- leveneTest(aov(vd ~ cov + vi, data = df)$residuals ~ df$vi)
  levene <- data.frame(F = levene$`F value`[1], Df1 = levene$Df[1], Df2 = levene$Df[2], p = levene$`Pr(>F)`[1])
  levene <- signif(levene, 4)
  rownames(levene) <- 'Teste de Levene'

  return(levene)
}

#Constroi a tabela de shapiro wilk
ancova_shapiro_test <- function (df){
  shapiro <- shapiro.test(aov(vd ~ cov + vi, data = df)$residuals)
  shapiro <- data.frame(Estatística = shapiro$statistic, p = shapiro$p.value)
  shapiro <- signif(shapiro, 4)
  rownames(shapiro) <- 'Teste de Shapiro-Wilk'
  return(shapiro)
}

#Constroi a tabela posthoc
posthoc_ancova_table <- function (df){
  posthoc <- as.data.frame(
    df %>% emmeans_test(vd ~ vi, covariate = cov, p.adjust.method = 'bonferroni')
  )
  posthoc <- posthoc[,-c(1, 2)]
  posthoc$statistic <- signif(posthoc$statistic, 4)
  posthoc$p <- signif(posthoc$p, 4)
  posthoc$p.adj <- signif(posthoc$p.adj, 4)
  names(posthoc) <- c('Grupo 1','Grupo 2', 'df', 'Estatistica', 'p', 'p.adj', 'Significância')

  return(posthoc)
}