#Dado uma tabela com diversas colunas, junta em uma só
consolidated_data <- function (data_info){
  data_aux <- data.frame(Dados = data_info[[1]], Classificação = names(data_info)[1])

    for (i in seq(ncol(data_info) - 1)){
      data_aux <- rbind(data_aux, data.frame(Dados = data_info[[i + 1]], Classificação = names(data_info)[i + 1]))
    }
  return(data_aux)
}

setUniValues <- function (values, data){
  values$nrow <- nrow(data)
  values$ncol <- ncol(data)
  values$names <- colnames(data)
  values$data_info <- data
  values$c_data_info <- consolidated_data(data)
  values$abs_min <- min(values$c_data_info$Dados)
  values$abs_max <- max(values$c_data_info$Dados)
  }

get_bin_freq <- function (data_info, bins = NULL, min = NULL, max = NULL, algorithm = 'Sturges', ...){

  if(is.null(bins) && is.null(algorithm))
    return(NULL)

  if (is.null(bins)) {
    dados <- sapply(data_info, function (x) hist(x, breaks = algorithm, plot = FALSE, ...))
    freq <- dados[2,]
    dados <- dados[1,]

    data <- data.frame(Dados = NULL, Classificação = NULL, Freq = NULL)

    for (i in  seq(length(freq))){
      data <- rbind(data, data.frame(Dados = dados[i], Freq = freq[i], Classificação = names(dados)[i]))
    }

    return (data)
  }
  else{
    breaks <- list(seq(min, max + bins, by = bins))[[1]]

    freq <- sapply(data_info, function (x) hist(x, breaks = breaks, include.lowest=TRUE, plot = FALSE))[2,]

    data <- data.frame(Dados = breaks[0:(length(breaks) - 1)], Classificação = names(freq)[1], Freq = freq[1])
    names(data) <- c('Dados', 'Classificação', 'Freq')
    data <- data[which(data$Freq != 0),]

    for (i in seq(length(freq) - 1)){
      data_aux <- data.frame(Dados = breaks[0:(length(breaks) - 1)], Classificação = names(freq)[i + 1], Freq = freq[i + 1])
      names(data_aux) <- c('Dados', 'Classificação', 'Freq')
      data_aux <- data_aux[which(data_aux$Freq != 0),]

      data <- rbind(data, data_aux)

    }
    return(data)
  }
}

ellipse_data <- function (data_info, colnames, ci){
  ellipse <- car::dataEllipse(
    x = data_info$Dados[which(data_info$Classificação == colnames[1])],
    y = data_info$Freq[which(data_info$Classificação == colnames[1])],
    levels = ci, draw = FALSE
  )
  data <- data.frame(x = ellipse[,1], y = ellipse[,2],
                     Classificação = colnames[1])

  for (i in seq(length(colnames) - 1)){
      ellipse <- car::dataEllipse(
        x = data_info$Dados[which(data_info$Classificação == colnames[i + 1])],
        y = data_info$Freq[which(data_info$Classificação == colnames[i + 1])],
        levels = ci, draw = FALSE
      )
    data_aux <- data.frame(x = ellipse[,1], y = ellipse[,2],
                           Classificação = colnames[i + 1]
    )
    data <- rbind(data, data_aux)
  }
  return(data)
}

renderCheckNormTable <- function (values, options){
  ci <- 0.05
  data <- values$data_info
  fig <- data.frame(
    sapply(data, function (x) signif(shapiro.test(x)$p.value, 4)),
    'Teste Shapiro-Wilk' = sapply(data, function (x) {
      p <- shapiro.test(x)$p.value
      if(p > ci)
        'Normal'
      else
        'Não normal'
    }),
    sapply(data, function (x) signif(ks.test(x, 'pnorm')$p.value, 4)),
    sapply(data, function (x){
      p <- ks.test(x, 'pnorm')$p.value
            if(p > ci)
        'Normal'
      else
        'Não normal'
    })
  )
  names(fig) <- c('Teste Shapiro-Wilk', 'Decisão - teste Shapiro-Wilk', 'Teste Kolmogorov-Smirnov', 'Desisão - teste Kolmogorov-Smirnov')
  fig

}

setAncovaValues <- function (values, options){
  dt <- values$bidimensional_data
  var1 <- options$ancova_variable
  cov1 <- options$ancova_covariable
  group1 <- options$ancova_group_variable

  dt <- data.frame(var = sapply(dt[var1], function (x) as.double(x)), cov = sapply(dt[cov1], function (x) as.double(x)), group = sapply(dt[group1], function (x) as.character(x)))
  values$names_bi <- names(dt)
  names(dt) <- c('vard', 'cov', 'vari')
  values$data_info_bi <- dt
}

ancova_table <- function (values, options){
  values$model_ancova <- aov(vard ~ cov + vari, data = values$data_info_bi)

  dt <- Anova(values$model_ancova, type = options$ancova_sumsq)
  dt <- data.frame('Soma de quadrados' = dt$`Sum Sq`, 'DF' = dt$Df, 'F' = dt$`F value`, 'p' = dt$`Pr(>F)`)

  if(options$ancova_sumsq == 2)
    rownames(dt) <- c(values$names_bi[c(2, 3)], 'Residuos')
  else
    rownames(dt) <- c('Intercept',values$names_bi[c(2, 3)], 'Residuos')
  dt <- round(dt, 4)

  return(dt)
}

levene_table <- function (values, options){
  levene <- leveneTest(aov(vard ~ cov + vari, data = values$data_info_bi)$residuals ~ values$data_info_bi$vari)
  levene <- data.frame(F = levene$`F value`[1], Df1 = levene$Df[1], Df2 = levene$Df[2], p = levene$`Pr(>F)`[1])
  levene <- round(levene, 4)
  rownames(levene) <- 'Teste de Levene'
  levene
}
shapiro_table <- function (values, options){
  shapiro <- shapiro.test(aov(vard ~ cov + vari, data = values$data_info_bi)$residuals)
  shapiro <- data.frame(Estatística = shapiro$statistic, p = shapiro$p.value)
  shapiro <- round(shapiro, 4)
  rownames(shapiro) <- 'Teste de Shapiro-Wilk'
  shapiro
}

posthoc_table <- function (values, options){
  dt <- values$data_info_bi
  posthoc <- as.data.frame(
    dt %>% emmeans_test(vard ~ vari, covariate = cov, p.adjust.method = 'bonferroni')
  )
  posthoc <- posthoc[,-c(1, 2)]
  posthoc$statistic <- round(posthoc$statistic, 4)
  posthoc$p <- round(posthoc$p, 4)
  posthoc$p.adj <- round(posthoc$p.adj, 4)
  names(posthoc) <- c('Grupo 1','Grupo 2', 'df', 'Estatistica', 'p', 'p.adj', 'Significância')
  posthoc
}


ftest <- function (first, sec, data){
  data <- data.frame(data[which(data$`Classificação` == first | data$`Classificação` == sec),]$Dados, data[which(data$Classificação == first | data$Classificação == sec),]$`Classificação`)
  names(data) <- c('Dados', 'Classificacao')

  res <- var.test(Dados ~ Classificacao, data = data)
  res <- data.frame(F = res$statistic,Num_df = res$parameter[1], Denom_df = res$parameter[2] ,p = res$p.value)

}