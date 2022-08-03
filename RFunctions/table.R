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

ancova_table <- function (values){
  dt <- values$bidimensional_data
  dt2 <- data.frame(
    anova_test(data = dt, dt[,1] ~ dt[,3] * dt[,2])
  )
  dt2[,6] <- sapply(dt2[,6], function (x) if(x != '*') 'Significânte' else 'Não significante' )
  names(dt2)[6] <- 'Significância'
  dt2$Effect <- c(names(dt)[3], names(dt)[2], paste0(names(dt)[3], ':', names(dt)[2]) )
  dt2
}