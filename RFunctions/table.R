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
    # names(data) <- c('Dados', 'Classificação', 'Freq')

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

ancova_table <- function (values, options){
  dt <- values$bidimensional_data
  var1<- options$ancova_variable
  cov1 <- options$ancova_covariable
  group1 <- options$ancova_group_variable

  print('A')
  dt <- data.frame(var = sapply(dt[var1], function (x) as.double(x)), cov = sapply(dt[cov1], function (x) as.double(x)), group = sapply(dt[group1], function (x) as.character(x)))
  print('B')
  names <- names(dt)
  names(dt) <- c('var', 'cov', 'group')


  dt2 <- dt %>% anova_test(cov ~ var * group)
  dt2[,6] <- sapply(dt2[,5], function (x) if(x > options$ancova_ci) 'Significânte' else 'Não significante' )
  names(dt2)[6] <- 'Significância'
  dt2$Effect <- c(names[which(names(dt) == 'var')], names[which(names(dt) == 'group')],
                  paste0(names[which(names(dt) == 'group')], ':', names[which(names(dt) == 'var')] ))
  dt2
}

levene_table <- function (values, options){
  dt <- values$bidimensional_data
  var1<- options$ancova_variable
  cov1 <- options$ancova_covariable
  group1 <- options$ancova_group_variable


  dt <- data.frame(var = sapply(dt[var1], function (x) as.double(x)), cov = sapply(dt[cov1], function (x) as.double(x)), group = sapply(dt[group1], function (x) as.character(x)))
  names(dt) <- c('var', 'cov', 'group')

  model <- lm(cov ~ var + group, data = dt)
  model.metrics <- augment(model) %>% select(c(-.hat, -.sigma, -.fitted))
  levene <- model.metrics %>% levene_test(.resid ~ group)

  levene$statistic <- as.double(round(levene$statistic, 4))
  levene$p <- as.double(round(levene$p, 4))

  levene <- data.frame(F = levene$statistic, df1 = levene$df1, df2 = levene$df2, p = levene$p)
  names(levene) <- c('F', 'Df 1', 'Df 2', 'p')

  levene
}
shapiro_table <- function (values, options){
  dt <- values$bidimensional_data
  var1<- options$ancova_variable
  cov1 <- options$ancova_covariable
  group1 <- options$ancova_group_variable


  dt <- data.frame(var = sapply(dt[var1], function (x) as.double(x)), cov = sapply(dt[cov1], function (x) as.double(x)), group = sapply(dt[group1], function (x) as.character(x)))
  names(dt) <- c('var', 'cov', 'group')

  model <- lm(cov ~ var + group, data = dt)
  model.metrics <- augment(model) %>% select(c(-.hat, -.sigma, -.fitted))
  shapiro <- shapiro_test(model.metrics$.resid)

  shapiro$statistic <- as.double(round(shapiro$statistic, 4))
  shapiro$p.value <- as.double(round(shapiro$p.value, 4))

  shapiro <- data.frame(F = shapiro$statistic, p = shapiro$p.value)
  names(shapiro) <- c('F', 'p')

  shapiro
}