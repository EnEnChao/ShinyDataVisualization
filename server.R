source('RFunctions/table.R')
source('RFunctions/PlotFunct.R')

server <- function (input, output, session){

  #----------- VARIÁVEIS -----------
  values <- reactiveValues()
  options <- reactiveValues()

  values$usr_title <- NULL
  values$bidimensional_data <- NULL
  values$data_info <- data.frame()
  values$condensed_data_info <- data.frame()

  #Carrosel com as imagens iniciais
  output$home_images <- renderSlickR({
    imgs <- list.files("www/HomeImages", pattern=".png", full.names = TRUE)
    slickR(imgs, objLinks = 'https://www.gov.br/inmetro/pt-br')+ settings(dots = TRUE, autoplay = TRUE, slidesToShow = 2, slidesToScroll = 1)
  })

  #Iniciar alguns textos dinâmicos
  output$table_import_data_output <- renderUI(tagList(h2(strong('Importe os seus dados na barra de controle à esquerda:'),align = 'center'), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()))
  output$title_name_insert <- renderUI(h2(strong('Digite os dados:')))

  output$table_import_bi_data_output <- renderUI(tagList(h2(strong('Importe os seus dados na barra de controle à esquerda:'),align = 'center'), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()))
  output$title_name_insert_bi <- renderUI(h2(strong('Digite os dados:')))
  frase_erro <- 'Dados inseridos incorretamente, verificar o manual para mais informações.'

  #Iniciar as planilhas
  output$user_data <- renderRHandsontable({ rhandsontable(data = data.frame(matrix(data = '', nrow = 1000, ncol =50))) })
  observeEvent(input$inserted_bi_type,
    output$user_data_bi <- switch(input$inserted_bi_type,
                                  'uni_data' = {
                                    dt_aux <- data.frame(matrix('', 1000, 1))
                                    names(dt_aux) <- 'A'
                                    renderRHandsontable({ rhandsontable(dt_aux) })
                                  },
                                  'two_col' = {
                                    dt_aux <- data.frame(matrix('', 1000, 2))
                                    names(dt_aux) <- c('A', 'B')
                                    renderRHandsontable({ rhandsontable(dt_aux) })
                                  },
                                  'anova' = {
                                    dt_aux <- data.frame(matrix('', 1000, 2))
                                    names(dt_aux) <- c('A', 'B')
                                    renderRHandsontable({ rhandsontable(dt_aux) })
                                  },
                                  'anova_rep' = {
                                    dt_aux <- data.frame(matrix('', 1000, 3))
                                    names(dt_aux) <- c('A', 'B', 'C')
                                    renderRHandsontable({ rhandsontable(dt_aux) })
                                  },
                                  'anova_mix' = {
                                    dt_aux <- data.frame(matrix('', 1000, 4))
                                    names(dt_aux) <- c('A', 'B', 'C', 'D')
                                    renderRHandsontable({ rhandsontable(dt_aux) })
                                  },
                                  'ancova' = {
                                    dt_aux <- data.frame(matrix('', 1000, 3))
                                    names(dt_aux) <- c('A', 'B', 'C')
                                    renderRHandsontable({ rhandsontable(dt_aux) })
                                  },
                                  'manova' = {
                                    dt_aux <- data.frame(matrix('', 1000, 5))
                                    names(dt_aux) <- c('A', 'B', 'C', 'D', 'E')
                                    renderRHandsontable({ rhandsontable(dt_aux) })
                                  },
    )
  )

  #Esconder todos os paineis
  hideTab(inputId = "tabs", target = "Gráficos 2D")
  hideTab(inputId = "tabs", target = "Gráficos 3D")
  hideTab(inputId = "tabs", target = "Avaliando os dados")
  hideTab(inputId = "tabs", target = "Informações gerais")

  hideTab(inputId = "tabs", target = "Transformar seus dados")
  hideTab(inputId = "tabs", target = "Comparando duas médias")
  hideTab(inputId = "tabs", target = "Comparando multiplas médias")

  hideTab(inputId = "tabs", target = 'Gráfico em Mesh')

  observeEvent(input$change_global_ci, intervalo_global_de_confianca <<- input$change_global_ci)
  observeEvent(input$change_global_signif, significancia_de_aproximacao <<- input$change_global_signif)
  #-------------------Load Tutorial-------------------#
  observeEvent(input$tutorial_button,{
      showModal(modalDialog(
        title = "Artigo",
        tags$iframe(style="height:600px; width:100%", src="Shiny Data Visualization - Tutorial.pdf"),
        easyClose = TRUE,
        footer = NULL
      ))
  })

  #-------------------Load Unidimensional Data-------------------#
  observeEvent(input$load_unidimensional,{
    showTab(inputId = "tabs", target = "Gráficos 2D")
    showTab(inputId = "tabs", target = "Gráficos 3D")
    showTab(inputId = "tabs", target = "Informações gerais")

    # Caso a opção selecionada tenha sido um dos exemplos
    if (input$file_selector_uni == 'example'){
      #Caso construction
      if(input$examp_select == 'labs')
        dt <- data.frame(read.xlsx('Data/Laboratorios.xlsx'))
      if(input$examp_select == 'construction')
        dt <- data.frame(construction[6:9])
      else if(input$examp_select == 'florida')
        dt <- data.frame(Florida)
      else if(input$examp_select == 'distributions')
      {
        dt <- data.frame(
          'Normal' = rnorm(100),
          'Poisson' = rpois(100, 4),
          'Binomial' = rbinom(100, 2, 0.5),
          'Exponencial' = rexp(100, 1),
          'Chi-Quadrado' = rchisq(100, df = 0, ncp = 2.)
        )
      }
      else if(input$examp_select == 'midwest'){
        dt <- data.frame(midwest[5:11])
        dt <- dt[-2]
      }
      else if(input$examp_select == 'lincoln_temperature'){
        dt <- data.frame(
          Temperatura.Media = lincoln_weather$`Mean Temperature [F]`,
          Temperatura.Maxima = lincoln_weather$`Max Temperature [F]`,
          Temperatura.Minima = lincoln_weather$`Min Temperature [F]`
        )
      }
    }

    else if (input$file_selector_uni == 'import') {
      if (!is.null(input$file_imported)) {
        if (!is.null(read.xlsx(input$file_imported$datapath))) {
          dt <- data.frame(read.xlsx(input$file_imported$datapath)) }
        else dt <- NULL
      }else dt <- NULL }

    if(!is.null(dt)) {
      #Carregar o data frame
      names(dt) <- gsub('\\.', ' ', names(dt))
      setUniValues(values, dt)
      output$table_import_data_output <- renderUI(
        shinycssloaders::withSpinner(
          DTOutput("table_import_data_output2"),
          type = spinnerType,
          color = spinnerColor,
          size = spinnerSize
        )
      )
      output$table_import_data_output2 <- renderDT(dt)
    }

    #Título inserido no import
    values$usr_title <- paste0(input$title_id_import)
    output$title_name_import <- renderUI(h2(strong(values$usr_title)))
  })
  observeEvent(input$load_spreadsheet, {
    showTab(inputId = "tabs", target = "Gráficos 2D")
    showTab(inputId = "tabs", target = "Gráficos 3D")
    showTab(inputId = "tabs", target = "Informações gerais")

    dt <- data.frame(hot_to_r(input$user_data))
    empty_columns <- colSums(dt == "") == nrow(dt)
    dt <- dt[, !empty_columns]

    if(ncol(dt) != 0) {
      empty_rows <- rowSums(dt == "") == ncol(dt)
      dt <- dt[!empty_rows,]
    } else dt <- NULL

    if(!is.null(dt)) {
      names(dt) <- dt[1,]
      names(dt) <- gsub('\\.', ' ', names(dt))
      dt <- dt[-1,]

      dt <- sapply(dt, function(x) as.double(x))
      dt <- as.data.frame(dt)
      setUniValues(values, dt)
    } else output$rest_of_sidebar <- renderMenu(NULL)

    #Título inserido no import
    values$usr_title <- paste0(input$title_id_insert)
    output$title_name_insert <- renderUI(h2(strong(values$usr_title)))

  })

  #-------------------Load Bidimensional Data-------------------#
  observeEvent(input$load_bidimensional,{
    type <- NULL
    if (input$file_selector_bi == 'example'){
      #Uma variável
      if(input$examp_select_bi == 'Sepal.Lenght'){
        dt <- data.frame(`Tamanho da Sépala` = iris$Sepal.Length)
        type <- 'uni_data'
      }
      if(input$examp_select_bi == 'waste'){
        dt <- data.frame(`Quantidade de Resíduos` = waste$waste)
        type <- 'uni_data'
      }
      #Duas Médias
      if(input$examp_select_bi == 'mice'){
        dt <- mice2[,c(2, 3)]
        type <- 'two_col'
      }
      if(input$examp_select_bi == 'genderweight'){
        dt <- data.frame(sapply(levels(genderweight$group), function (x){ genderweight[which(genderweight$group == x),]$weight }))
        type <- 'two_col'
      }
      #ANOVA
      if(input$examp_select_bi == 'gas2') {
        dt <- data.frame(read.xlsx('Data/exemplo1ANCOVA.xlsx'))
        dt <- dt[,-2]
        type <- 'anova'
      }
      if(input$examp_select_bi == 'PlantGrowth'){
        dt <- PlantGrowth
        type <- 'anova'
      }
      if(input$examp_select_bi == 'selfesteem'){
        s2 <- datarium::selfesteem %>%
          gather(key = "time", value = "score", t1, t2, t3) %>%
            convert_as_factor(id, time)
        dt <- data.frame(score = s2[3], time = s2[2], id = as.numeric(s2[[1]]))
        type <- 'anova_rep'
      }
      if(input$examp_select_bi == 'anxiety'){
        s2 <- anxiety %>% gather(key = "time", value = "score", t1, t2, t3) %>% convert_as_factor(id, time)
        dt <- data.frame(score = s2[4], group = s2[2], time = s2[3], id = as.numeric(s2[[1]]))
        type <- 'anova_mix'
      }
      if(input$examp_select_bi == 'gas3') {
        dt <- data.frame(read.xlsx('Data/exemplo1ANCOVA.xlsx'))
        type <- 'ancova'
      }
      if(input$examp_select_bi == 'stress') {
        data("stress", package = "datarium")
        dt <- data.frame(score = stress$score, age = stress$age,exercise = stress$exercise)
        type <- 'ancova'
      }
      if(input$examp_select_bi == 'iris_manova'){
        dt <- iris
        type <- 'manova'
      }
      if(input$examp_select_bi == 'plant_variation'){
        dt <- data.frame(read.xlsx('Data/manova_data.xlsx'))
        type <- 'manova'
      }
    }
    else if(input$file_selector_bi == 'import') {
      dt <- data.frame(read.xlsx(input$file_imported_bi$datapath))
      type <- input$imported_bi_type
    }
    if ((type %in% c('uni_data', 'two_col', 'anova','anova_rep', 'anova_mix', 'ancova', 'manova')) & checandoDados(dt, type)){
      if(type %in% c('uni_data', 'two_col')){
        showTab(inputId = "tabs", target = "Comparando duas médias")
        showTab(inputId = "tabs", target = "Avaliando os dados")
        hideTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
        hideTab(inputId = "tabs", target = "Comparando multiplas médias")
      }
      else{
        hideTab(inputId = 'tabsetid_checking_data', target = 'Transformando os dados para normalidade')
        hideTab(inputId = "tabs", target = "Comparando duas médias")
        showTab(inputId = "tabs", target = "Avaliando os dados")
        showTab(inputId = "tabs", target = "Comparando multiplas médias")
      }
      switch(
        type,
        'uni_data' = {
          showTab(inputId = 'tabsetid_checking_data', target = 'Transformando os dados para normalidade')
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          showTab(inputId = 'tabsetid_two_means', target = 'Teste T')
          showTab(inputId = 'tabsetid_two_means', target = 'Teste de Wilcoxon')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_two_means', selected = 'Teste T')
        },
        'two_col' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Transformando os dados para normalidade')
          showTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          showTab(inputId = 'tabsetid_two_means', target = 'Teste T')
          showTab(inputId = 'tabsetid_two_means', target = 'Teste de Wilcoxon')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_two_means', selected = 'Teste T')
        },
        'anova' = {
          showTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          hideTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'ANOVA')
        },
        'anova_rep' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          showTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          showTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'ANOVA - medidas repetidas')
        },
        'anova_mix' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          showTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'ANOVA - medidas misturadas')
        },
        'ancova' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          hideTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'ANCOVA')
        },
        'manova' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          hideTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          showTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          showTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'MANOVA')
        }
      )
       output$table_import_bi_data_output <- renderUI(
         shinycssloaders::withSpinner(
           DTOutput("table_import_bi_data_output2"),
           type = spinnerType,
           color = spinnerColor,
           size = spinnerSize
         )
       )
      output$table_import_bi_data_output2 <- renderDT(dt)

      values$usr_title <- paste0(input$title_id_import_bi)
      output$title_name_import_bi <- renderUI(
        h2(strong(values$usr_title))
      )
      values$bidimensional_data <- dt
      values$bidimensional_data_type <- type
    }
    else {
      output$table_import_bi_data_output <- renderUI(h3(frase_erro, align = 'center'))
      hideTab(inputId = "tabs", target = "Comparando duas médias")
      hideTab(inputId = "tabs", target = "Avaliando os dados")
      hideTab(inputId = "tabs", target = "Comparando multiplas médias")
    }
  })
  observeEvent(input$load_spreadsheet_bi,{
    updateTabsetPanel(session = session, inputId = 'bidimensional_data_input', selected = 'Importe seus dados')
    dt <- data.frame(hot_to_r(input$user_data_bi))
    type <- input$inserted_bi_type
    nomes <- dt[1,]
    names(dt) <- nomes
    dt <- dt[-1,]
    if(type != 'uni_data') {
      dt2 <- lapply(seq(ncol(dt)), function(x) { which(dt[x] == '') })
      dt <- dt[-Reduce(intersect, dt2),]
    }
    else {
      dt <- as.data.frame(dt)
      dt <- dt[-which(dt == ''),]
      dt <- as.data.frame(dt)
    }
    output$table_import_bi_data_output <- renderUI(
         shinycssloaders::withSpinner(
           DTOutput("table_import_bi_data_output2"),
           type = spinnerType,
           color = spinnerColor,
           size = spinnerSize
         )
       )
      output$table_import_bi_data_output2 <- renderDT(dt)
    switch(
      type,
      'uni_data' = dt[[1]] <- strtoi(dt[[1]]),
      'two_col' = {
          dt[[1]] <- strtoi(dt[[1]])
          dt[[2]] <- strtoi(dt[[2]])
      },
      'anova' = dt[[1]] <- strtoi(dt[[1]]),
      'anova_rep' = {
          dt[[1]] <- strtoi(dt[[1]])
          dt[[3]] <- strtoi(dt[[3]])
      },
      'anova_mix' = dt[[1]] <- strtoi(dt[[1]]),
      'ancova' = {
          dt[[1]] <- strtoi(dt[[1]])
          dt[[2]] <- strtoi(dt[[2]])
      }
    )
    if ((type %in% c('uni_data', 'two_col', 'anova','anova_rep', 'anova_mix', 'ancova', 'manova')) & checandoDados(dt, type)){
      if(type %in% c('uni_data', 'two_col')){
        showTab(inputId = "tabs", target = "Comparando duas médias")
        showTab(inputId = "tabs", target = "Avaliando os dados")
        hideTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
        hideTab(inputId = "tabs", target = "Comparando multiplas médias")
      }
      else{
        hideTab(inputId = 'tabsetid_checking_data', target = 'Transformando os dados para normalidade')
        hideTab(inputId = "tabs", target = "Comparando duas médias")
        showTab(inputId = "tabs", target = "Avaliando os dados")
        showTab(inputId = "tabs", target = "Comparando multiplas médias")
      }
      switch(
        type,
        'uni_data' = {
          showTab(inputId = 'tabsetid_checking_data', target = 'Transformando os dados para normalidade')
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          showTab(inputId = 'tabsetid_two_means', target = 'Teste T')
          showTab(inputId = 'tabsetid_two_means', target = 'Teste de Wilcoxon')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_two_means', selected = 'Teste T')
        },
        'two_col' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Transformando os dados para normalidade')
          showTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          showTab(inputId = 'tabsetid_two_means', target = 'Teste T')
          showTab(inputId = 'tabsetid_two_means', target = 'Teste de Wilcoxon')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_two_means', selected = 'Teste T')
        },
        'anova' = {
          showTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          hideTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'ANOVA')
        },
        'anova_rep' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          showTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          showTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'ANOVA - medidas repetidas')
        },
        'anova_mix' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          showTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'ANOVA - medidas misturadas')
        },
        'ancova' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          hideTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          showTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'ANCOVA')
        },
        'manova' = {
          hideTab(inputId = 'tabsetid_checking_data', target = 'Homogeneidade das variâncias')
          hideTab(inputId = 'tabsetid_checking_data', target = 'Avaliando a esfericidade')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas misturadas - testes pareados')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANOVA - medidas repetidas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'ANCOVA')
          showTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA')
          showTab(inputId = 'tabsetid_multiple_means', target = 'MANOVA Verificações Univariadas')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Kruskal-Wallis')
          hideTab(inputId = 'tabsetid_multiple_means', target = 'Teste de Friedman')
          updateTabsetPanel(session = session, inputId = 'tabsetid_checking_data', selected = 'Distribuição de dados Normais')
          updateTabsetPanel(session = session, inputId = 'tabsetid_multiple_means', selected = 'MANOVA')
        }
      )
      values$usr_title <- paste0(input$title_id_import_bi)
      values$bidimensional_data <- dt
      values$bidimensional_data_type <- type
    }
    else {
      output$table_import_bi_data_output <- renderUI(h3(frase_erro, align = 'center'))
      hideTab(inputId = "tabs", target = "Comparando duas médias")
      hideTab(inputId = "tabs", target = "Avaliando os dados")
      hideTab(inputId = "tabs", target = "Comparando multiplas médias")
    }
  })

  observeEvent(values$bidimensional_data, {
    useShinyjs()
    if(values$bidimensional_data_type == 'uni_data') {
      updateSelectInput(session, 'test_t_options', choices = c('One way' = 'one'))
      updateSelectInput(session, 'wilcoxon_test_options', choices = c('One way' = 'one'))
      hideTab(inputId = 'tabsetid_two_means', target = 'Teste do Sinal')
    }
    else if(values$bidimensional_data_type == 'two_col'){
      if(length(na.omit(values$bidimensional_data[[1]])) == length(na.omit(values$bidimensional_data[[2]]))) {
        updateSelectInput(session, 'test_t_options', choices = c('One way' = 'one', 'Two ways' = 'two', 'Pareado' = 'paired'), selected = 'two')
        updateSelectInput(session, 'wilcoxon_test_options', choices = c('One way' = 'one', 'Mann–Whitney' = 'rank_sum', 'Pareado' = 'paired'), selected = 'rank_sum')
        showTab(inputId = 'tabsetid_two_means', target = 'Teste do Sinal')
      }
      else{
        updateSelectInput(session, 'test_t_options', choices = c('One way' = 'one', 'Two ways' = 'two'), selected = 'two')
        updateSelectInput(session, 'wilcoxon_test_options', choices = c('One way' = 'one', 'Mann–Whitney' = 'rank_sum'), selected = 'rank_sum')
        hideTab(inputId = 'tabsetid_two_means', target = 'Teste do Sinal')
      }
    }
  })

  #-------------------Testes Estatísticos-------------------#
  observe(if(!is.null(values$bidimensional_data)){
    #---------------Avaliando os dados---------------#
  {
    #-------------------Transform to Normality-------------------#
    if(values$bidimensional_data_type == 'uni_data') {
      data <- values$bidimensional_data
      initial_plot <- plot_ly(x = ~density(data[seq_len(nrow(data)),])$x, y = ~density(data[seq_len(nrow(data)),])$y, type = 'scatter', fill = 'tozeroy', alpha = 0.7) %>%
        layout(xaxis = list(title = 'Dados'), yaxis = list(title = 'Frequência'))
      output$transform_norm_results_original <- renderPlotly(initial_plot)
      output$transform_norm_results_method_statistics <- renderUI(h4('O coeficiente de distorção é : ', signif(skewness(data[[1]], na.rm = TRUE), significancia_de_aproximacao)))
      observeEvent(input$load_transform_norm, {

      df <- data
        logy <- if(input$transform_norm_distributions_logy == 0) 1 else input$transform_norm_distributions_logy
      df[1] <- switch(
        input$transform_norm_distributions,
        'none' = data[1],
        'sqrt' = if(input$transform_norm_distributions_skewed) sqrt(data[1]) else sqrt(max(data[1] + 1) - data[1]),
        'log10' = if(input$transform_norm_distributions_skewed) log10(data[1]) else log10(max(data[1] + 1) - data[1]),
        'logy' = if(input$transform_norm_distributions_skewed) data[1] * logy else (max(data[1] + 1) - data[1]) * logy,
        '1/x' = if(input$transform_norm_distributions_skewed) 1/(data[1]) else 1/(max(data[1] + 1) - data[1])
      )

      if (input$transform_norm_distributions != 'none'){
         output$transform_norm_results_new <- renderUI(tagList(
            uiOutput('transform_norm_results_new_name'),
            shinycssloaders::withSpinner(
             plotlyOutput('transform_norm_results_new_plot'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
           )
         ))
        transformation <- input$transform_norm_distributions
        output$transform_norm_results_new_name <- renderUI(h3('Com a transformação: ',transformation))
        output$transform_norm_results_new_plot <- renderPlotly(plot_ly(x = ~density(df[seq_len(nrow(df)),])$x, y = ~density(df[seq_len(nrow(df)),])$y, type = 'scatter', mode = 'markers', fill = 'tozeroy', alpha = 0.7) %>%
        layout(xaxis = list(title = 'Dados'), yaxis = list(title = 'Frequência')))
        output$transform_norm_results_method_statistics <- renderUI(tagList(
           h4('O coeficiente de distorção é : ', signif(skewness(data[1], na.rm = TRUE), significancia_de_aproximacao)),
           h4('O novo coeficiente de distorção é : ', signif(skewness(df[1], na.rm = TRUE), significancia_de_aproximacao))
        ))

        # Download the new df
        dfDownload <- df
        output$transform_norm_download <- renderUI(downloadButton('transform_norm_download2','Baixe a nova tabela!'))
        output$transform_norm_download2 <- downloadHandler(
          filename = function() { "transformed_df.xlsx"},
          content = function(file) {write_xlsx(dfDownload, path = file)}
        )
      }
      else{
        output$transform_norm_results_method_statistics <- renderUI(tagList(h4('O coeficiente de distorção é : ', signif(skewness(data[1], na.rm = TRUE), significancia_de_aproximacao))))
        output$transform_norm_results_new <- renderUI(p(''))
        output$transform_norm_download <- renderUI(p(''))
      }
    })
  }

    #-------------------Assessing Normality-------------------#
    {
      output$plotly_norm_density <- renderPlotly(renderAssessingNormDensity(values, options))
      output$plotly_norm_qq <- renderPlotly(renderAssessingNormQQ(values, options))

      df <- values$bidimensional_data
      switch(
        values$bidimensional_data_type,
        'uni_data' = {
          shap <- shapiro.test(values$bidimensional_data[[1]])
          shap_assumption_norality <- data.frame(
            `Estatística` = signif(shap$statistic, significancia_de_aproximacao),
            p = signif(shap$p.value, significancia_de_aproximacao),
            Normalidade = ifelse(shap$p.value > intervalo_global_de_confianca, 'Normal', 'Não normal')
          )
          output$check_norm_table_shapiro <- renderDT(shap_assumption_norality)
          kolmogorov <- ks.test(values$bidimensional_data[[1]], 'pnorm')
          output$check_norm_table_kolmogorov <- renderDT(data.frame(
            `Estatística` = signif(kolmogorov$statistic, significancia_de_aproximacao),
            p = signif(kolmogorov$p.value, significancia_de_aproximacao),
            Normalidade = ifelse(kolmogorov$p.value > intervalo_global_de_confianca, 'Normal', 'Não normal')
          ))
        },
        'two_col' = {
          shap <- list(shapiro.test(values$bidimensional_data[[1]]), shapiro.test(values$bidimensional_data[[2]]))
          shap_assumption_norality <- data.frame(
            `Estatística` = signif(c(shap[[1]]$statistic, shap[[2]]$statistic), significancia_de_aproximacao),
            p = signif(c(shap[[1]]$p.value, shap[[2]]$p.value), significancia_de_aproximacao),
            Normalidade = ifelse(c(shap[[1]]$p.value, shap[[2]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
          )
          shap_assumption_norality <- data.frame(Nomes = names(values$bidimensional_data), shap_assumption_norality)
          output$check_norm_table_shapiro <- renderDT(shap_assumption_norality)
          kolmogorov <- list(ks.test(values$bidimensional_data[[1]], 'pnorm'), ks.test(values$bidimensional_data[[2]], 'pnorm'))
          kolmogorov <- data.frame(
            `Estatística` = signif(c(kolmogorov[[1]]$statistic, kolmogorov[[2]]$statistic), significancia_de_aproximacao),
            p = signif(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value), significancia_de_aproximacao),
            Normalidade = ifelse(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
          )
          kolmogorov <- data.frame(Nomes = names(values$bidimensional_data), kolmogorov)
          output$check_norm_table_kolmogorov <- renderDT(kolmogorov)
        },
        'anova' = {
          residuos <- residuals(lm(values$bidimensional_data[[1]] ~ values$bidimensional_data[[2]]))
          shap <- shapiro.test(as.numeric(residuos))
          shap_assumption_norality <- data.frame(
            `Estatística` = signif(shap$statistic, significancia_de_aproximacao),
            p = signif(shap$p.value, significancia_de_aproximacao),
            Normalidade = ifelse(shap$p.value > intervalo_global_de_confianca, 'Normal', 'Não normal')
          )
          output$check_norm_table_shapiro <- renderDT(shap_assumption_norality)
          kolmogorov <- ks.test(residuos, 'pnorm')
          output$check_norm_table_kolmogorov <- renderDT(data.frame(
            `Estatística` = signif(kolmogorov$statistic, significancia_de_aproximacao),
            p = signif(kolmogorov$p.value, significancia_de_aproximacao),
            Normalidade = ifelse(kolmogorov$p.value > intervalo_global_de_confianca, 'Normal', 'Não normal')
          ))
        },
        'anova_rep' = {
          residuos <- residuals(lm(values$bidimensional_data[[1]] ~ values$bidimensional_data[[2]]))
          shap <- shapiro.test(as.numeric(residuos))
          shap_assumption_norality <- data.frame(
            `Estatística` = signif(shap$statistic, significancia_de_aproximacao),
            p = signif(shap$p.value, significancia_de_aproximacao),
            Normalidade = ifelse(shap$p.value > intervalo_global_de_confianca, 'Normal', 'Não normal')
          )
          output$check_norm_table_shapiro <- renderDT(shap_assumption_norality)
          kolmogorov <- ks.test(residuos, 'pnorm')
          output$check_norm_table_kolmogorov <- renderDT(data.frame(
            `Estatística` = signif(kolmogorov$statistic, significancia_de_aproximacao),
            p = signif(kolmogorov$p.value, significancia_de_aproximacao),
            Normalidade = ifelse(kolmogorov$p.value > intervalo_global_de_confianca, 'Normal', 'Não normal')
          ))
        },
        'anova_mix' = {
          residuos <- residuals(lm(values$bidimensional_data[[1]] ~ values$bidimensional_data[[2]] * values$bidimensional_data[[3]]))
          shap <- shapiro.test(as.numeric(residuos))
          shap_assumption_norality <- data.frame(
            `Estatística` = signif(shap$statistic, significancia_de_aproximacao),
            p = signif(shap$p.value, significancia_de_aproximacao),
            Normalidade = ifelse(shap$p.value > intervalo_global_de_confianca, 'Normal', 'Não normal')
          )
          output$check_norm_table_shapiro <- renderDT(shap_assumption_norality)
          kolmogorov <- ks.test(residuos, 'pnorm')
          output$check_norm_table_kolmogorov <- renderDT(data.frame(
            `Estatística` = signif(kolmogorov$statistic, significancia_de_aproximacao),
            p = signif(kolmogorov$p.value, significancia_de_aproximacao),
            Normalidade = ifelse(kolmogorov$p.value > intervalo_global_de_confianca, 'Normal', 'Não normal')
          ))
        },
        'ancova' = {
          residuos1 <- residuals(lm(values$bidimensional_data[[1]] ~ values$bidimensional_data[[3]]))
          residuos2 <- residuals(lm(values$bidimensional_data[[2]] ~ values$bidimensional_data[[3]]))
          shap <- list(shapiro.test(residuos1), shapiro.test(residuos2))
          shap_assumption_norality <- data.frame(
            Nomes = c(names(values$bidimensional_data)[1], names(values$bidimensional_data)[2]),
            `Estatística` = signif(c(shap[[1]]$statistic, shap[[2]]$statistic), significancia_de_aproximacao),
            p = signif(c(shap[[1]]$p.value, shap[[2]]$p.value), significancia_de_aproximacao),
            Normalidade = ifelse(c(shap[[1]]$p.value, shap[[2]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
          )
          output$check_norm_table_shapiro <- renderDT(shap_assumption_norality)
          kolmogorov <- list(ks.test(residuos1, 'pnorm'), ks.test(residuos2, 'pnorm'))
          kolmogorov <- data.frame(
            Nomes = c(names(values$bidimensional_data)[1], names(values$bidimensional_data)[2]),
            `Estatística` = signif(c(kolmogorov[[1]]$statistic, kolmogorov[[2]]$statistic), significancia_de_aproximacao),
            p = signif(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value), significancia_de_aproximacao),
            Normalidade = ifelse(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
          )
          output$check_norm_table_kolmogorov <- renderDT(kolmogorov)
        },
        'manova' = {
          residuos <- lapply(
            names(values$bidimensional_data)[seq_len(ncol(values$bidimensional_data) - 1)],
            function (x){
              residuals(lm(values$bidimensional_data[[x]] ~ values$bidimensional_data[[ncol(values$bidimensional_data)]]))
            })
          shap <- lapply(residuos, function (x) shapiro.test(x))
          shap_assumption_norality <- switch(
            ncol(values$bidimensional_data) - 2,
            data.frame(
              Nomes = names(values$bidimensional_data)[1:2],
              `Estatística` = signif(c(shap[[1]]$statistic, shap[[2]]$statistic), significancia_de_aproximacao),
              p = signif(c(shap[[1]]$p.value, shap[[2]]$p.value), significancia_de_aproximacao),
              Normalidade = ifelse(c(shap[[1]]$p.value, shap[[2]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
            ),
            data.frame(
              Nomes = names(values$bidimensional_data)[1:3],
              `Estatística` = signif(c(shap[[1]]$statistic, shap[[2]]$statistic, shap[[3]]$statistic), significancia_de_aproximacao),
              p = signif(c(shap[[1]]$p.value, shap[[2]]$p.value, shap[[3]]$p.value), significancia_de_aproximacao),
              Normalidade = ifelse(c(shap[[1]]$p.value, shap[[2]]$p.value, shap[[3]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
            ),
            data.frame(
              Nomes = names(values$bidimensional_data)[1:4],
              `Estatística` = signif(c(shap[[1]]$statistic, shap[[2]]$statistic, shap[[3]]$statistic, shap[[4]]$statistic), significancia_de_aproximacao),
              p = signif(c(shap[[1]]$p.value, shap[[2]]$p.value, shap[[3]]$p.value, shap[[4]]$p.value), significancia_de_aproximacao),
              Normalidade = ifelse(c(shap[[1]]$p.value, shap[[2]]$p.value, shap[[3]]$p.value, shap[[4]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
            ),
          )
          output$check_norm_table_shapiro <- renderDT(shap_assumption_norality)
          kolmogorov <- lapply(residuos, function (x) ks.test(x, 'pnorm'))
          kolmogorov <- switch(
            ncol(values$bidimensional_data) - 2,
            data.frame(
              Nomes = names(values$bidimensional_data)[1:2],
              `Estatística` = signif(c(kolmogorov[[1]]$statistic, kolmogorov[[2]]$statistic), significancia_de_aproximacao),
              p = signif(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value), significancia_de_aproximacao),
              Normalidade = ifelse(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
            ),
            data.frame(
              Nomes = names(values$bidimensional_data)[1:3],
              `Estatística` = signif(c(kolmogorov[[1]]$statistic, kolmogorov[[2]]$statistic, kolmogorov[[3]]$statistic), significancia_de_aproximacao),
              p = signif(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value, kolmogorov[[3]]$p.value), significancia_de_aproximacao),
              Normalidade = ifelse(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value, kolmogorov[[3]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
            ),
            data.frame(
              Nomes = names(values$bidimensional_data)[1:4],
              `Estatística` = signif(c(kolmogorov[[1]]$statistic, kolmogorov[[2]]$statistic, kolmogorov[[3]]$statistic, kolmogorov[[4]]$statistic), significancia_de_aproximacao),
              p = signif(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value, kolmogorov[[3]]$p.value, kolmogorov[[4]]$p.value), significancia_de_aproximacao),
              Normalidade = ifelse(c(kolmogorov[[1]]$p.value, kolmogorov[[2]]$p.value, kolmogorov[[3]]$p.value, kolmogorov[[4]]$p.value) > intervalo_global_de_confianca, 'Normal', 'Não normal')
            ),
          )
          output$check_norm_table_kolmogorov <- renderDT(kolmogorov)
        }
      )
  }

    # -------------------Homogenity of Variance-------------------#
    if(values$bidimensional_data_type %in% c('two_col', 'anova')) {
      choosen <- input$homogenity_tests
      ci <- intervalo_global_de_confianca
      data <- switch(
        values$bidimensional_data_type,
        'two_col' = contingency_data(values$bidimensional_data),
        'anova' = values$bidimensional_data,
        'ancova' = values$bidimensional_data[1:2]
      )
      names(data) <- c('Dados', 'Classificação')

      if (choosen == 'f_test') {
        if(nrow(data.frame(table(data$`Classificação`))) == 2){
          dois_grupos <- names(table(data$`Classificação`))
          first <- dois_grupos[1]
          sec <- dois_grupos[2]
          output$homogenity_method_name <- renderUI(h3('Teste F para comparação de duas variáveis'))

          data <- data.frame(data[which(data$`Classificação` == first | data$`Classificação` == sec),]$Dados, data[which(data$Classificação == first | data$Classificação == sec),]$`Classificação`)
          names(data) <- c('Dados', 'Classificacao')

          res <- var.test(Dados ~ Classificacao, data = data, conf.level = ci)
          dt_ftest <- signif(data.frame(F = res$statistic, Num_df = res$parameter[1], Denom_df = res$parameter[2], p = res$p.value), significancia_de_aproximacao)

          output$homogenity_table <- renderDT(dt_ftest)
          output$homogenity_method_results <- renderUI(
            tagList(
              h4('Com um intervalo de confiança de ', ci * 100, '%:'),
              h4(signif(res$conf.int[1], significancia_de_aproximacao), signif(res$conf.int[2], significancia_de_aproximacao)), br(),
              if (signif(res$p.value, significancia_de_aproximacao) == 0) h4('O valor de p é ', strong('aproximadadente 0'), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                                                  '. Assim sugere que ', strong('há diferênças significantes'), ' entre as duas variâncias')
              else if (1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, significancia_de_aproximacao)), ', o que é ', strong('maior do que o nivel de significância', 1 - ci),
                                                '. Assim sugere que ', strong('não há diferênças significantes'), ' entre as duas variâncias')
              else h4('O valor de p = ', strong(signif(res$p.value, significancia_de_aproximacao)), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                      '. Assim sugere que ', strong('há diferênças significantes'), ' entre as duas variâncias')
            )
          )
      }
      }
      else if (choosen == 'bartlett_test') {
        res <- bartlett.test(Dados ~ Classificação, data = data)
        output$homogenity_method_name <- renderUI(h3('Teste de Bartlett para comparação múltiplas variáveis'))
        output$homogenity_table <- renderDT(signif(data.frame(F = res$statistic, df = res$parameter, p = res$p.value), significancia_de_aproximacao))
        output$homogenity_method_results <- renderUI(
          tagList(
            if (signif(res$p.value, significancia_de_aproximacao) == 0) h4('O valor de p é ', strong('aproximadadente 0'), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                                                '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
            else if (1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, significancia_de_aproximacao)), ', o que é ', strong('maior do que o nivel de significância', 1 - ci),
                                              '. Assim sugere que ', strong('não há diferênças significantes'), ' entre as variâncias das variáveis')
            else h4('O valor de p = ', strong(signif(res$p.value, significancia_de_aproximacao)), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                    '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
          )
        )

      }
      else if (choosen == 'levene_test') {
        output$homogenity_method_name <- renderUI(h3('Teste de Levene para comparação múltiplas variáveis'))
        res <- leveneTest(Dados ~ Classificação, data = data)
        output$homogenity_table <- renderDT(signif(data.frame(df1 = res$Df[1], df2 = res$Df[2], F = res$`F value`, Sig = res$`Pr(>F)`), significancia_de_aproximacao))
        output$homogenity_method_results <- renderUI(
          tagList(
            if (signif(res$`Pr(>F)`[1], significancia_de_aproximacao) == 0) h4('O valor de p é ', strong('aproximadadente 0'), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                                                    '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
            else if (1 - ci < res$`Pr(>F)`[1]) h4('O valor de p = ', strong(signif(res$`Pr(>F)`[1], significancia_de_aproximacao)), ', o que é ', strong('maior do que o nivel de significância', 1 - ci),
                                                  '. Assim sugere que ', strong('não há diferênças significantes'), ' entre as variâncias das variáveis')
            else h4('O valor de p = ', strong(signif(res$`Pr(>F)`[1], significancia_de_aproximacao)), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                    '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
          )
        )
      }
      else if (choosen == 'fk_test') {
        res <- fligner.test(Dados ~ Classificação, data = data)
        output$homogenity_method_name <- renderUI(h3('Teste de Fligner-Killeen para comparação múltiplas variáveis'))
        output$homogenity_table <- renderDT(signif(data.frame(Chi_Quadrado = res$statistic, df = res$parameter, p = res$p.value), significancia_de_aproximacao))
        output$homogenity_method_results <- renderUI(
          tagList(
            if (signif(res$p.value, significancia_de_aproximacao) == 0) h4('O valor de p é ', strong('aproximadadente 0'), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                                                '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
            else if (1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, significancia_de_aproximacao)), ', o que é ', strong('maior do que o nivel de significância', 1 - ci),
                                              '. Assim sugere que ', strong('não há diferênças significantes'), ' entre as variâncias das variáveis')
            else h4('O valor de p = ', strong(signif(res$p.value, significancia_de_aproximacao)), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                    '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
          )
        )
      }
    }

    #-------------------Assumption of Sphericity-------------------#
    if (values$bidimensional_data_type %in% c('anova_mix', 'anova_rep')) {

      dt <- values$bidimensional_data
      names <- names(dt)

      #Pegando os dados
      anova <- switch(
        values$bidimensional_data_type,
        'anova_mix' = {
          names(dt) <- c('vd', 'vi1', 'vi2', 'id')
          anova_test(data = dt, dv = vd, wid = id, between = vi1, within = vi2)
        },
        'anova_rep' = {
          names(dt) <- c('vd', 'vi', 'id')
          anova_test(data = dt, dv = vd, wid = id, within = vi)
        }
      )
      #ANOVA table
      anova_table <- get_anova_table(anova)
      mauchly <- anova$`Mauchly's Test for Sphericity`
      if(nrow(anova_table) == 1) {
        mauchly[[1]] <- mauchly[2]
        mauchly <- mauchly[-4]
      }
      else
        mauchly <- mauchly[-4]
      output$mauchly_test <- renderDT(mauchly)

      #Correções de Esfericidade
      Corr_GG <- get_anova_table(anova, correction = "GG")
      Corr_HF <- get_anova_table(anova, correction = "HF")
      if(nrow(Corr_GG) == 1){
        Corr_GG[[1]] <- names[2]
        Corr_GG <- Corr_GG[-6]
        Corr_GG$p <- signif(Corr_GG$p, significancia_de_aproximacao)
        Corr_HF[[1]] <- names[2]
        Corr_HF <- Corr_HF[-6]
        Corr_HF$p <- signif(Corr_HF$p, significancia_de_aproximacao)
      }
      else{
        Corr_GG[[1]] <- c(names[2], names[3], paste0(names[2], ' - ', names[3]))
        Corr_GG <- Corr_GG[-6]
        Corr_GG$p <- signif(Corr_GG$p, significancia_de_aproximacao)
        Corr_HF[[1]] <- c(names[2], names[3], paste0(names[2], ' - ', names[3]))
        Corr_HF <- Corr_HF[-6]
        Corr_HF$p <- signif(Corr_HF$p, significancia_de_aproximacao)
      }
      output$sphericity_corrections_gg <- renderDT(Corr_GG)
      output$sphericity_corrections_hf <- renderDT(Corr_HF)
      mauchly_p <- ifelse(nrow(mauchly) == 1, mauchly$p, mauchly$p[2])
      output$sphericity_statistics <- renderUI(h4(
        'Pelo teste de esfericidade de mauchly, ', strong('p =', mauchly_p), '.', if(mauchly_p > intervalo_global_de_confianca)
           h4('As variâncias das diferenças entre os grupo',strong('são iguais'),' conforme o intervalo de confiança, assim podemos assumir a esfericidade.')
        else  h4('As variâncias das diferenças entre os grupo ',strong('não são iguais'),' assim não podemos assumir a esfericidade.'), br()))
  }
  }
    #-------------Comparando duas médias-------------#
  {
    #-------Comparando duas médias - One way---------#
    if (values$bidimensional_data_type == 'uni_data'){
      #-------------------T Test-------------------#
    {
      df <- values$bidimensional_data
      output$t_test_predict <- renderUI(tagList(
        column(6,
               h3(strong('Testando Normalidade', align = 'center')),
               plotlyOutput('t_test_uni_normality'),
               uiOutput('t_test_uni_normality_results'), align = 'center'
        ),
        column(6,
               h3(strong('Verificando Outliers', align = 'center')),
               plotlyOutput('t_test_uni_boxplot'),
               align = 'center'
          ),
        column(12,
               h3(strong('Resultados', align = 'center')),
               DTOutput('t_test_uni_dt'),
               uiOutput('t_test_uni_effect_size')
        )
      ))
      #Testes de Normalidade
      output$t_test_uni_normality <- renderPlotly(ggplotly(ggqqplot(df[,1], color = '#F8766D')))
      t_test_shapiro_uni <- signif(shapiro.test(df[,1])$p.value, significancia_de_aproximacao)
      output$t_test_uni_normality_results <- renderUI(tagList(p('O valor de p utilizando o teste de Shapiro Wilk é de: ', strong(t_test_shapiro_uni), ifelse(t_test_shapiro_uni > intervalo_global_de_confianca, '(Estatísticamente normal)', '(Estatísticamente não normal)')),
                                                      if(t_test_shapiro_uni <= intervalo_global_de_confianca) p('Recomenda-se utilizar o teste de Wilcoxon.')
      ))
      #Testes de Outlier
      output$t_test_uni_boxplot <- renderPlotly(plot_ly(data.frame(), y = df[,1], type = 'box', boxpoints = "all", fillcolor = '#FEE4E2', name = names(df)[1], marker = list(color = '#F8766D', outliercolor = 'gray'), line = list(color = '#F8766D')))
      # output$t_test_uni_outliers <- renderUI(if(nrow(identify_outliers(df[1])) == 0) p('Não exstem outliers') else p('Existem ',nrow(identify_outliers(df[1])), ' outliers.'))

      #Resultados da computação do Teste T
      df_test_t_uni <- test_t_uni(df[1], input$test_t_mu)
      output$t_test_uni_dt <- renderDT(df_test_t_uni)
      output$t_test_uni_effect_size <- renderUI(tagList(
        p('O valor de p para o teste T é: ', strong(df_test_t_uni$p),
          if(df_test_t_uni$p > intervalo_global_de_confianca) p('Ou seja, a média dos dados é estatisticamente igual a ',input$test_t_mu)
          else p('Ou seja, a média dos dados é estatisticamente diferente de ',input$test_t_mu)
        ),
        p('A área de efeito da variável ', (strong(names(df)[1])), ', com mu = ', strong(input$test_t_mu), ' é de: ', strong(signif(abs(mean(df[,1]) - input$test_t_mu) / sd(df[,1]), significancia_de_aproximacao)))
      ))
    }
      #-------------------Wilcoxon Test-------------------#
    {
      output$wilcoxon_test_predict <- renderUI(tagList(
      column(6,
             h3(strong('Testando Simetria ao redor da mediana')),
             plotlyOutput('wilcoxon_test_uni_symmetry'),align = 'center'
      ),
      column(6,
             h3(strong('Verificando Outliers')),
             plotlyOutput('wilcoxon_test_uni_boxplot'),
             uiOutput('wilcoxon_test_uni_outliers'),align = 'center'
        ),
      column(12,
             h3(strong('Resultados', align = 'center')),
             h4(strong('Teste de Wilcoxon - ',names(values$bidimensional_data)[1]), align = 'center'),
             DTOutput('wilcoxon_test_uni_dt'),
             uiOutput('wilcoxon_test_uni_effect_size')
      )
    ))
      output$wilcoxon_test_uni_symmetry <- renderPlotly(ggplotly(gghistogram(df, x = names(df)[1], y = "..density..", fill = "#FEE4E2",bins = 4, add_density = TRUE)))
      output$wilcoxon_test_uni_boxplot <- renderPlotly(plot_ly(data.frame(), y = df[,1], type = 'box', boxpoints = "all", fillcolor = '#FEE4E2', name = names(df)[1], marker = list(color = '#F8766D', outliercolor = 'gray'), line = list(color = '#F8766D')))
      output$wilcoxon_test_uni_outliers <- renderUI(if(nrow(identify_outliers(df[1])) == 0) p('Não exstem outliers') else p('Existem ',nrow(identify_outliers(df[1])), ' outliers.'))

      w_test1 <- rstatix::wilcox_test(data = data.frame(data = df[[1]]),data ~ 1, mu = input$wilcoxon_t_mu)
      w_test_df1 <- data.frame(p = signif(w_test1$p[[1]], 4), estatística = signif(w_test1$statistic[[1]], significancia_de_aproximacao))
      rownames(w_test_df1) <- paste0('Test de Wilcoxon - ', names(df)[1])
      output$wilcoxon_test_uni_dt <- renderDT(w_test_df1)
      w_effectsize1 <- wilcox_effsize(data.frame(data = df[[1]]), data ~ 1, mu = input$wilcoxon_t_mu)
      output$wilcoxon_test_uni_effect_size <- renderUI(tagList(
        p('O valor de p para o teste de Wilcoxon é: ', strong(w_test_df1$p),
          if(w_test_df1$p > intervalo_global_de_confianca) p('Ou seja, a média dos dados é estatisticamente igual a ',input$wilcoxon_t_mu)
          else p('Ou seja, a média dos dados é estatisticamente diferente de ',input$wilcoxon_t_mu)
        ),
        p('A área de efeito da variável ', (strong(names(df)[1])), ', com mu = ', input$wilcoxon_t_mu, ' é de: ', strong(signif(w_effectsize1$effsize[[1]], significancia_de_aproximacao)))
      ))
    }
    }
    #-------Comparando duas médias - Two way---------#
    else if (values$bidimensional_data_type == 'two_col'){
        #-------------------T Test-------------------#
        {
          if(input$test_t_options == 'one') {
            df <- values$bidimensional_data
            output$t_test_predict <- renderUI(tagList(
              h3(strong('Testando Normalidade', align = 'center')),
              column(6,
                     h4(names(df)[1]),
                     shinycssloaders::withSpinner(
                       plotlyOutput('t_test_normality_1'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                     uiOutput('t_test_normality_results_1'), align = 'center'
              ),
              column(6,
                     h4(names(df)[2]),
                     shinycssloaders::withSpinner(
                       plotlyOutput('t_test_normality_2'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                     uiOutput('t_test_normality_results_2'), align = 'center'
                ),
              column(12,
                     br(),
                     h3(strong('Verificando Outliers', align = 'center'))
              ),
              column(6,
                     shinycssloaders::withSpinner(
                       plotlyOutput('t_test_boxplot_1'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                     uiOutput('t_test_outliers_1'), align = 'center'
              ),
              column(6,
                     shinycssloaders::withSpinner(
                       plotlyOutput('t_test_boxplot_2'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                     uiOutput('t_test_outliers_2'), align = 'center'
              ),
              column(12,
                     h3(strong('Resultados do Teste T:', align = 'center')),
                     column(6,
                            h4(strong('Teste T - ',names(values$bidimensional_data)[1]), align = 'center'),
                            shinycssloaders::withSpinner(
                       DTOutput('t_test_dt_1'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                            uiOutput('t_test_effect_size1')
                     ),
                     column(6,
                            h4(strong('Teste T - ',names(values$bidimensional_data)[2]), align = 'center'),
                            shinycssloaders::withSpinner(
                       DTOutput('t_test_dt_2'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                            uiOutput('t_test_effect_size2')
                     )
              )
            ))
            output$t_test_normality_1 <- renderPlotly(ggplotly(ggqqplot(df[,1], color = '#F8766D')))
            t_test_shapiro_1 <- signif(shapiro.test(df[,1])$p.value, significancia_de_aproximacao)
            output$t_test_normality_results_1 <- renderUI(p('O valor de p utilizando o teste de Shapiro Wilk é de: ', strong(t_test_shapiro_1), ifelse(t_test_shapiro_1 > intervalo_global_de_confianca, '(Estatísticamente normal)', '(Estatísticamente não normal)')))
            output$t_test_normality_2 <- renderPlotly(ggplotly(ggqqplot(df[,2], color = '#28B3B6')))
            t_test_shapiro_2 <- signif(shapiro.test(df[,2])$p.value, significancia_de_aproximacao)
            output$t_test_normality_results_2 <- renderUI(p('O valor de p utilizando o teste de Shapiro Wilk é de: ', strong(t_test_shapiro_2), ifelse(t_test_shapiro_2 > intervalo_global_de_confianca, '(Estatísticamente normal)', '(Estatísticamente não normal)')))
            output$t_test_boxplot_1 <- renderPlotly(plot_ly(data.frame(), y = df[,1], type = 'box', boxpoints = "all", fillcolor = '#FEE4E2', name = names(df)[1], marker = list(color = '#F8766D', outliercolor = 'gray'), line = list(color = '#F8766D')))
            output$t_test_outliers_1 <- renderUI(if(nrow(identify_outliers(df[1])) == 0) p('Não exstem outliers') else p('Existem ',nrow(identify_outliers(df[1])), ' outliers.'))
            output$t_test_boxplot_2 <- renderPlotly(plot_ly(data.frame(), y = df[,2], type = 'box', boxpoints = "all", fillcolor = '#D4F0F0', name = names(df)[2], marker = list(color = '#28B3B6', outliercolor = 'gray'), line = list(color = '#28B3B6')))
            output$t_test_outliers_2 <- renderUI(if(nrow(identify_outliers(df[2])) == 0) p('Não exstem outliers') else p('Existem ',nrow(identify_outliers(df[2])), ' outliers.'))

            test_t_uni_1 <- test_t_uni(df[1], input$test_t_mu)
            test_t_uni_2 <- test_t_uni(df[2], input$test_t_mu)

            output$t_test_dt_1 <- renderDT(test_t_uni_1)
            output$t_test_dt_2 <- renderDT(test_t_uni_2)

            output$t_test_effect_size1 <- renderUI(tagList(
              p('O valor de p para o teste T é: ', strong(test_t_uni_1$p),
                if(test_t_uni_1$p > intervalo_global_de_confianca) p('Ou seja, a média dos dados é estatisticamente igual a ',input$test_t_mu)
                else p('Ou seja, a média dos dados é estatisticamente diferente de ',input$test_t_mu)
              ),
              p('A área de efeito da variável ', (strong(names(df)[1])), ', com mu = ', strong(input$test_t_mu), ' é de: ', strong(signif(abs(mean(df[,1]) - input$test_t_mu) / sd(df[,1]), significancia_de_aproximacao)))
            ))
            output$t_test_effect_size2 <- renderUI(tagList(
              p('O valor de p para o teste T é: ', strong(test_t_uni_2$p),
                if(test_t_uni_2$p > intervalo_global_de_confianca) p('Ou seja, a média dos dados é estatisticamente igual a ',input$test_t_mu)
                else p('Ou seja, a média dos dados é estatisticamente diferente de ',input$test_t_mu)
              ),
              p('A área de efeito da variável ', (strong(names(df)[2])), ', com mu = ', strong(input$test_t_mu), ' é de: ', strong(signif(abs(mean(df[,2]) - input$test_t_mu) / sd(df[,2]), significancia_de_aproximacao)))
            ))
          }
          if(input$test_t_options == 'two' | (input$test_t_options == 'paired'  & !(any(is.na(contingency_data(values$bidimensional_data)))) )){
            output$t_test_predict <- renderUI(tagList(
              column(6, h3(strong('Testando Normalidade', align = 'center'))),
              column(6, h3(strong('Verificando Outliers', align = 'center'))),
              column(12,
                     shinycssloaders::withSpinner(
                       plotlyOutput('t_test_plotly'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     )
              ),
              column(6, uiOutput('t_test_normality_results')),

              column(12,
                     h3(strong('Teste de homocedasticidade')),
                     shinycssloaders::withSpinner(
                       DTOutput('t_test_homostacity'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                     uiOutput('t_test_homostacity_results'),
                     h3(strong('Resultados do teste T: ')),
                     shinycssloaders::withSpinner(
                       DTOutput('t_test_dt'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                     uiOutput('t_test_effect_size')
                ,align = 'center'
              )
            ))
            dt <- contingency_data(values$bidimensional_data)

            fig1 <- renderAssessingNormQQ(values)
            shap <- dt %>% group_by(Classificação) %>% shapiro_test(Dados) %>% as.data.frame()
            shap <- shap[-c(2,3)]

            output$t_test_normality_results <- renderUI(tagList(p('Os valores de p utilizando o teste de Shapiro Wilk de',
                                                          strong(shap[1,1]),' é de: ', strong(shap[1,2]), ifelse(signif(shap[1,2], significancia_de_aproximacao) > intervalo_global_de_confianca, '(Estatísticamente normal)', '(Estatísticamente não normal)'), ' e ',
                                                          strong(shap[2,1]),' é de: ', strong(shap[2,2]), ifelse(signif(shap[2,2], significancia_de_aproximacao) > intervalo_global_de_confianca, '(Estatísticamente normal).', '(Estatísticamente não normal).')),
                                                                if(signif(shap[1,2], significancia_de_aproximacao) <= intervalo_global_de_confianca | signif(shap[2,2], significancia_de_aproximacao) <= intervalo_global_de_confianca) p('Recomenda-se utilizar o teste de Wilcoxon ou o teste do Sinal.')
            )

            )
            # output$t_test_boxplot <- renderPlotly(plot_ly(data = dt, y =~ Dados, x =~ Classificação, color =~ Classificação, type = 'box'))
            fig2 <- plot_ly(data = dt, y =~ Dados, x =~ Classificação, color =~ Classificação, type = 'box')
            # outliers_dt <- dt %>% group_by(Classificação) %>% identify_outliers(Dados) %>% data.frame()
            # output$t_test_outliers <- if(nrow(outliers_dt) != 0) renderDT(outliers_dt)

            output$t_test_plotly <- renderPlotly(subplot(fig1, fig2, margin = 0.1))
            ftest <- var.test(Dados ~ Classificação, dt)
            ftest_dt <- data.frame('Estimativa' = signif(ftest$estimate, significancia_de_aproximacao), 'p' = signif(ftest$p.value, significancia_de_aproximacao), 'Estatística' = signif(ftest$statistic, significancia_de_aproximacao))
            output$t_test_homostacity <- renderDT(ftest_dt)
            output$t_test_homostacity_results <- renderUI(p('O valor de p é: ',strong(signif(ftest_dt$p, significancia_de_aproximacao)), 'ou seja,', ifelse(ftest_dt$p > intervalo_global_de_confianca, 'a variância entre os grupos é estatísticamente igual.', 'a variância entre os grupos é estatísticamente diferente.')))

            test_w <- dt %>% t_test(Dados ~ Classificação, paired = input$test_t_options == 'paired', var.equal = ftest_dt$p > intervalo_global_de_confianca | input$test_t_options == 'paired')
            test_w_df <- data.frame(p = signif(test_w$p, significancia_de_aproximacao), estatística = signif(test_w$statistic, significancia_de_aproximacao), df = signif(test_w$df, significancia_de_aproximacao))
            rownames(test_w_df) <- if(input$test_t_options == 'two') paste0('Teste T') else if(input$test_t_options == 'paired') paste0('Teste T - Pareado')
            output$t_test_dt <- renderDT(test_w_df)
            cohensD <- (dt %>% cohens_d(Dados ~ Classificação, paired = input$test_t_options == 'paired'))$effsize
            output$t_test_effect_size <-renderUI(tagList(p('O valor p do teste T é: ',strong(test_w_df$p) , ' ou seja, ', ifelse(test_w_df$p > intervalo_global_de_confianca, 'as variâncias de ambos os grupos são estatísticamente iguais.', 'os dados médios de ambos os grupos são estatísticamente diferentes.'),
                                                   br(),
                                                   'A área de efeito entre as variáveis ', strong(names(values$bidimensional_data)[1]),', e ',strong(names(values$bidimensional_data)[2]), ' é de: ', strong(signif(cohensD, significancia_de_aproximacao)),br()),
                                                   if(ftest_dt$p <= intervalo_global_de_confianca & input$test_t_options != 'paired') p('O algoritmo para calcular a área de efeito foi o ', strong('o algoritmo de Welch,'), ' como as variâncias foram diferentes.')
            ))
          }
        }
        #-------------------Wilcoxon Test-------------------#
        {
      if(input$wilcoxon_test_options == 'one') {
        df <- values$bidimensional_data
        output$wilcoxon_test_predict <- renderUI(tagList(
          h3(strong('Testando Simetria ao redor da mediana', align = 'center')),
          column(6,
                 h4(names(df)[1]),
                 shinycssloaders::withSpinner(
                       plotlyOutput('wilcoxon_test_symmetry_1'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 align = 'center'
          ),
          column(6,
                 h4(names(df)[2]),
                 shinycssloaders::withSpinner(
                       plotlyOutput('wilcoxon_test_symmetry_2'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 align = 'center'
            ),
          column(12,
                 br(),
                 h3(strong('Verificando Outliers', align = 'center'))
          ),
          column(6,
                 shinycssloaders::withSpinner(
                       plotlyOutput('wilcoxon_test_boxplot_1'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 uiOutput('wilcoxon_test_outliers_1'), align = 'center'
          ),
          column(6,
                 shinycssloaders::withSpinner(
                       plotlyOutput('wilcoxon_test_boxplot_2'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 uiOutput('wilcoxon_test_outliers_2'), align = 'center'
          ),
          column(12,
                 h3(strong('Resultados do Teste de Wilcoxon:', align = 'center')),
                 column(6,
                        h4(strong('Teste de Wilcoxon - ',names(values$bidimensional_data)[1]), align = 'center'),
                        shinycssloaders::withSpinner(
                       DTOutput('wilcoxon_test_dt_1'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                        uiOutput('wilcoxon_test_effect_size1')
                 ),
                 column(6,
                        h4(strong('Teste de Wilcoxon - ',names(values$bidimensional_data)[2]), align = 'center'),
                        shinycssloaders::withSpinner(
                       DTOutput('wilcoxon_test_dt_2'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                        uiOutput('wilcoxon_test_effect_size2')
                 )
          )
        ))
        output$wilcoxon_test_symmetry_1 <- renderPlotly(ggplotly(gghistogram(df, x = names(df)[1], y = "..density..", fill = "#FEE4E2",bins = 4, add_density = TRUE)))
        output$wilcoxon_test_symmetry_2 <- renderPlotly(ggplotly(gghistogram(df, x = names(df)[2], y = "..density..", fill = "#D4F0F0",bins = 4, add_density = TRUE)))
        output$wilcoxon_test_boxplot_1 <- renderPlotly(plot_ly(data.frame(), y = df[,1], type = 'box', boxpoints = "all", fillcolor = '#FEE4E2', name = names(df)[1], marker = list(color = '#F8766D', outliercolor = 'gray'), line = list(color = '#F8766D')))
        output$wilcoxon_test_outliers_1 <- renderUI(if(nrow(identify_outliers(df[1])) == 0) p('Não exstem outliers') else p('Existem ',nrow(identify_outliers(df[1])), ' outliers.'))
        output$wilcoxon_test_boxplot_2 <- renderPlotly(plot_ly(data.frame(), y = df[,2], type = 'box', boxpoints = "all", fillcolor = '#D4F0F0', name = names(df)[2], marker = list(color = '#28B3B6', outliercolor = 'gray'), line = list(color = '#28B3B6')))
        output$wilcoxon_test_outliers_2 <- renderUI(if(nrow(identify_outliers(df[2])) == 0) p('Não exstem outliers') else p('Existem ',nrow(identify_outliers(df[2])), ' outliers.'))

        w_test1 <- rstatix::wilcox_test(data = data.frame(data = df[[1]]),data ~ 1, mu = input$wilcoxon_t_mu)
        w_test_df1 <- data.frame(p = signif(w_test1$p[[1]], 4), estatística = signif(w_test1$statistic[[1]], significancia_de_aproximacao))
        rownames(w_test_df1) <- paste0('Test de Wilcoxon - ', names(df)[1])
        output$wilcoxon_test_dt_1 <- renderDT(w_test_df1)
        w_effectsize1 <- wilcox_effsize(data.frame(data = df[[1]]), data ~ 1, mu = input$wilcoxon_t_mu)
        output$wilcoxon_test_effect_size1 <- renderUI(
          tagList(
            p('O valor de p para o teste de Wilcoxon é: ', strong(w_test_df1$p),
              if(w_test_df1$p > intervalo_global_de_confianca) p('Ou seja, a média dos dados é estatisticamente igual a ',input$wilcoxon_t_mu)
              else p('Ou seja, a média dos dados é estatisticamente diferente de ',input$wilcoxon_t_mu)
            ),
            p('A área de efeito da variável ', (strong(names(df)[1])), ', com mu = ', input$wilcoxon_t_mu, ' é de: ', strong(signif(w_effectsize1$effsize[[1]], significancia_de_aproximacao)))
          )
        )
        w_test2 <- rstatix::wilcox_test(data = data.frame(data = df[[2]]),data ~ 1, mu = input$wilcoxon_t_mu)
        w_test_df2 <- data.frame(p = signif(w_test2$p[[1]], significancia_de_aproximacao), estatística = signif(w_test2$statistic[[1]], significancia_de_aproximacao))
        rownames(w_test_df2) <- paste0('Test de Wilcoxon - ', names(df)[2])
        output$wilcoxon_test_dt_2 <- renderDT(w_test_df2)
        w_effectsize2 <- wilcox_effsize(data.frame(data = df[[2]]), data ~ 1, mu = input$wilcoxon_t_mu)
        output$wilcoxon_test_effect_size2 <- renderUI(tagList(
          p('O valor de p para o teste de Wilcoxon é: ', strong(w_test_df2$p),
            if(w_test_df2$p > intervalo_global_de_confianca) p('Ou seja, a média dos dados é estatisticamente igual a ',input$wilcoxon_t_mu)
            else p('Ou seja, a média dos dados é estatisticamente diferente de ',input$wilcoxon_t_mu)
          ),
          p('A área de efeito da variável ', (strong(names(df)[2])), ', com mu = ', input$wilcoxon_t_mu, ' é de: ', strong(signif(w_effectsize2$effsize[[1]], significancia_de_aproximacao)))
        ))
      }
      else if(input$wilcoxon_test_options == 'rank_sum' | (input$wilcoxon_test_options == 'paired' & !(any(is.na(contingency_data(values$bidimensional_data)))) )){
        output$wilcoxon_test_predict <- renderUI(
          column(12,
                 uiOutput('more_assumptions'),
                 h3(strong('Verificando Outliers')),
                 shinycssloaders::withSpinner(
                       plotlyOutput('wilcoxon_test_plotly'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 shinycssloaders::withSpinner(
                       DTOutput('wilcoxon_test_outliers'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 h3(strong('Resultados do Teste de Wilcoxon:')),
                 shinycssloaders::withSpinner(
                       DTOutput('wilcoxon_test_dt'),
                       type = spinnerType,
                       color = spinnerColor,
                       size = spinnerSize
                     ),
                 uiOutput('wilcoxon_test_effect_size'),
                 align = 'center',
          )
        )
        dt <- contingency_data(values$bidimensional_data)

        if(input$wilcoxon_test_options == 'paired'){
          output$more_assumptions <- renderUI(tagList(
            h3(strong('Testando Simetria ao redor da mediana')),
            plotOutput('wilcoxon_test_symmetry')
          ))
          dt2 <- add_column(values$bidimensional_data, differences = values$bidimensional_data[[2]] - values$bidimensional_data[[1]], .before = 3)
          output$wilcoxon_test_symmetry <- renderPlot(gghistogram(dt2, x = "differences", y = "..density..", fill = "steelblue",bins = 5, add_density = TRUE))
        }

        outliers_dt <- dt %>% group_by(Classificação) %>% identify_outliers(Dados) %>% data.frame()
        output$wilcoxon_test_outliers <- if(nrow(outliers_dt) != 0) renderDT(outliers_dt)

        output$wilcoxon_test_plotly <- renderPlotly(plot_ly(data = dt, y =~ Dados, x =~ Classificação, color =~ Classificação, type = 'box'))

        #Remove Outliers
        # if(input$wilcoxon_test_options != 'paired')
        #   dt <- removeOutliers(dt)

        test_w <- rstatix::wilcox_test(dt, Dados ~ Classificação, paired = input$wilcoxon_test_options == 'paired')
        test_w_df <- data.frame(p = signif(test_w$p, 4), estatística = signif(test_w$statistic[[1]], significancia_de_aproximacao))
        rownames(test_w_df) <- if(input$wilcoxon_test_options == 'two') paste0('Teste de Wilcoxon') else if(input$wilcoxon_test_options == 'paired') paste0('Teste de Wilcoxon - Pareado')
        output$wilcoxon_test_dt <- renderDT(test_w_df)
        w_effectsize <- rstatix::wilcox_effsize(dt, Dados ~ Classificação, paired = input$wilcoxon_test_options == 'paired')$effsize[[1]]
        output$wilcoxon_test_effect_size <- renderUI(p('O valor p do teste de Wilxoxon é: ', test_w_df$p, ' ou seja, ', ifelse(test_w_df$p > intervalo_global_de_confianca, 'os dados médios de ambos os grupos são estatísticamente iguais.', 'os dados médios de ambos os grupos são estatísticamente diferentes.'),
                                                   br(),
          'A área de efeito entre as variáveis ', (strong(names(values$bidimensional_data)[1])),', e ',(strong(names(values$bidimensional_data)[2])),  ', é de: ', strong(signif(w_effectsize, significancia_de_aproximacao))))
      }
    }
        #-------------------Sign Test-------------------#
        if(length(na.omit(values$bidimensional_data[[1]])) == length(na.omit(values$bidimensional_data[[2]]))){
          dt <- values$bidimensional_data
          colnames(dt) <- c('var1', 'var2')
          dt <- contingency_data(dt)
          output$sign_test_outliers <- renderPlotly(plot_ly(data = dt, y =~ Dados, x =~ Classificação, color =~ Classificação, type = 'box'))

          sign_test <- dt %>% rstatix::sign_test(Dados ~ Classificação)
          sign_test_df <- data.frame(p = signif(sign_test$p[[1]], significancia_de_aproximacao), estatística = signif(sign_test$statistic[[1]], significancia_de_aproximacao), df = signif(sign_test$df[[1]], significancia_de_aproximacao))
          rownames(sign_test_df) <- paste0('Test do Sinal')
          output$sign_test_dt <- renderDT(sign_test_df)
          output$sign_test_p <- renderUI(p('O valor p do teste do Sinal é é: ', sign_test_df$p, ' ou seja, ', ifelse(sign_test_df$p > intervalo_global_de_confianca, 'os dados médios de ambos os grupos são estatísticamente iguais.', 'os dados médios de ambos os grupos são estatísticamente diferentes.')),
          )
        }
      }
  }
    #--------------------ANOVA's---------------------#
  { if (values$bidimensional_data_type %in% c('anova', 'ancova')) {
    #-------------------ANOVA-------------------#
  {
    if(values$bidimensional_data_type == 'anova')
      df <- values$bidimensional_data
    else
      df <- values$bidimensional_data[c(1, 3)]
    names <- names(df)
    names(df) <- c('Dados', 'Grupos')
    model <- lm(df$Dados ~ df$Grupos)

    #Normalidade
    if(nrow(data.frame(table(df$Grupos))) > 5)
      output$anova_qq_plot <- renderPlotly(ggplotly(ggqqplot(residuals(model), color = "#E7B800")))
    else
      output$anova_qq_plot <- renderPlotly(ggplotly(ggqqplot(data = df, x = 'Dados', color = 'Grupos', ggtheme = theme_minimal())))
    shap <- signif(rstatix::shapiro_test(residuals(model))$p.value, significancia_de_aproximacao)
    output$anova_shapiro <- renderUI(tagList(h4('O valor p para o teste de Shapiro-Wilk é: ', strong(shap), ifelse(shap > intervalo_global_de_confianca, ' (Estatísticamente normal)', ' (Estatísticamente não normal)')),
                                     if(shap <= intervalo_global_de_confianca) p('Recomenda-se utilizar o teste de Kruskal-Wallis.')
    ))
    #Outliers
    output$anova_box_plot <- renderPlotly(plot_ly(df, x = df[, 2], y = df[, 1], type = 'box', color = df[, 2]))

    #Teste de Levene
    levene_anova <- (df %>% rstatix::levene_test(Dados ~ Grupos))$p %>% signif(significancia_de_aproximacao)
    levene_anova <- signif(data.frame(df %>% rstatix::levene_test(Dados ~ Grupos)), significancia_de_aproximacao)
    names(levene_anova) <- c('DF1', 'DF2', 'Estatística', 'p')
    output$anova_levene_dt <- renderDT(levene_anova)
    output$anova_levene_results <- renderUI(h4('O valor de p do teste de Levene é: ', strong(levene_anova$p), '. Ou seja, ',
                                              ifelse(levene_anova$p > intervalo_global_de_confianca, 'não existe difereça entre as variâncias entre os grupos.', 'existe difereça entre as variâncias entre os grupos.')))

    #ANOVA
    anova_dt <- df %>% anova_test(Dados ~ Grupos)
    anova_dt <- anova_dt[, 4:7]
    anova_dt <- anova_dt[, -3]
    names(anova_dt) <- c('Estatística', 'p', 'ges')
    output$anova_dt <- renderDT(anova_dt)
    output$anova_p <- renderUI(h4('O valor de p do anova é: ', strong(anova_dt$p), '. Ou seja, ',
                                 ifelse(anova_dt$p > intervalo_global_de_confianca, 'não existe difereça significativa entre os grupos.', 'existe difereça significativa entre, pelo menos dois grupos.')))

    posthoc <- df %>% tukey_hsd(Dados ~ Grupos)
    posthoc$estimate <- signif(posthoc$estimate, significancia_de_aproximacao)
    posthoc$p.adj <- signif(posthoc$p.adj, significancia_de_aproximacao)
    posthoc <- posthoc[-c(1,4, 6, 7, 9)]
    names(posthoc) <- c('Grupo 1', 'Grupo 2', 'Estatística', 'p')
    posthoc$`Significância` <- lapply(posthoc$p, function (x){
      return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
    })
    output$anova_posthoc <- renderDT(posthoc)
  }
    #-------------------Kruskal-Wallis-------------------#
  {
    if(values$bidimensional_data_type == 'anova')
      df <- values$bidimensional_data
    else
      df <- values$bidimensional_data[c(1, 3)]
    #Boxplot
    output$kruskal_boxplot <- renderPlotly(plot_ly(df, y = df[[1]], color = df[[2]], type = 'box'))
    #Cálculo do Teste
    dfkruskal_dt <- df %>% rstatix::kruskal_test(df[[1]] ~ df[[2]])
    dfkruskal_dt <- dfkruskal_dt[3:6]
    dfkruskal_dt[1] <- signif(dfkruskal_dt[1], significancia_de_aproximacao)
    dfkruskal_dt[3] <- signif(dfkruskal_dt[3], significancia_de_aproximacao)
    output$kruskal_dt <- renderDT(dfkruskal_dt)

    #Area de Efeito
    dfkruskal_effectArea <- df %>% rstatix::kruskal_effsize(df[[1]] ~ df[[2]])
    dfkruskal_effectArea <- signif(dfkruskal_effectArea$effsize, significancia_de_aproximacao)
    output$kruskal_interpretation <- renderUI(tagList(h4('O valor de p do teste de Kruskal Wallis é: ', strong(dfkruskal_dt$p), '. Ou seja, ',
                                                ifelse(dfkruskal_dt$p > intervalo_global_de_confianca, 'não existe difereça significativa entre os grupos.', 'existe difereça significativa entre, pelo menos dois grupos.')),
                                              h4('O valor da área de efeito do teste é: ', strong(dfkruskal_effectArea))))
    names(df) <- c('Dados', 'Grupos')

    #Dumm's test
    df_dumm_test <- df %>% rstatix::dunn_test(Dados ~ Grupos, p.adjust.method = "bonferroni")
    df_dumm_test <- df_dumm_test[c(2, 3, 6, 7)]
    df_dumm_test[3] <- signif(df_dumm_test[3], significancia_de_aproximacao)
    df_dumm_test[4] <- signif(df_dumm_test[4], significancia_de_aproximacao)
    names(df_dumm_test) <- c('Grupo 1', 'Grupo 2', 'Estatística', 'p')
    df_dumm_test$`Significância` <- lapply(df_dumm_test$p, function (x){
      return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
    })
    output$kruskal_dunn_test <- renderDT(df_dumm_test)

    #Wilcoxon's test
    df_wilcoxon_test <- df %>% rstatix::wilcox_test(Dados ~ Grupos, p.adjust.method = "bonferroni")
    df_wilcoxon_test <- df_wilcoxon_test[c(2, 3, 6, 7)]
    df_wilcoxon_test[3] <- signif(df_wilcoxon_test[3], significancia_de_aproximacao)
    df_wilcoxon_test[4] <- signif(df_wilcoxon_test[4], significancia_de_aproximacao)
    names(df_wilcoxon_test) <- c('Grupo 1', 'Grupo 2', 'Estatística', 'p')
    df_wilcoxon_test$`Significância` <- lapply(df_wilcoxon_test$p, function (x){
      return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
    })
    output$kruskal_wilcoxon_test <- renderDT(df_wilcoxon_test)
  }
  }
   }
     #---------------ANOVA repeated measures----------------#
    if(values$bidimensional_data_type == 'anova_rep'){
      #--------------ANOVA - repeated measures-------------#
    {
      df <- values$bidimensional_data
      names <- names(df)
      names(df) <- c('vd', 'vi', 'wid')
      model <- lm(df$vd ~ df$vi)

      #Normalidade
      output$anova_rep_qq_plot <- renderPlotly(ggplotly(ggqqplot(residuals(model), color = "#E7B800")))
      shap <- signif(rstatix::shapiro_test(residuals(model))$p.value, significancia_de_aproximacao)
      output$anova_rep_shapiro <- renderUI(tagList(h4('O valor p para o teste de Shapiro-Wilk é: ', strong(shap), ifelse(shap > intervalo_global_de_confianca, ' (Estatísticamente normal)', ' (Estatísticamente não normal)')),
                                     if(shap <= intervalo_global_de_confianca) p('Recomenda-se utilizar o teste de Friedman.')
      ))

      #Outliers
      output$anova_rep_box_plot <- renderPlotly(plot_ly(df, x = df[,2], y = df[,1], type = 'box', color = df[,2]))

      #Teste de Esfericidade de Mauchly
      anova_dt <- df %>% anova_test(dv = vd, within = vi, wid = wid)
      mauchly <- anova_dt$`Mauchly's Test for Sphericity`
      mauchly <- mauchly[-4]
      names(mauchly) <- c('Efeito', 'W', 'p')
      mauchly[1] <- names[2]
      output$anova_rep_mauchly_dt <- renderDT(mauchly)
      output$anova_rep_mauchly_results <- renderUI(h4(
        'Pelo teste de esfericidade de mauchly, ', strong('p =', mauchly$p), '.', if(mauchly$p > intervalo_global_de_confianca)
           h4('As variâncias das diferenças entre os grupo',strong('são iguais'),' conforme o intervalo de confiança, assim podemos assumir a esfericidade.')
        else  h4('As variâncias das diferenças entre os grupo ',strong('não são iguais'),' assim não podemos assumir a esfericidade.'), br()))

      #ANOVA
      anova_dt <- get_anova_table(anova_dt)
      anova_dt[1] <- names(values$bidimensional_data)[2]
      anova_dt <- anova_dt[-c(2, 3, 6)]
      names(anova_dt) <- c('Efeito', 'Estatística', 'p', 'ges')
      anova_dt$`Estatística` <- signif(anova_dt$`Estatística`, significancia_de_aproximacao)
      anova_dt$p <- signif(anova_dt$p, significancia_de_aproximacao)
      output$anova_rep_dt <- renderDT(anova_dt)
      output$anova_rep_p <- renderUI(h4('O valor de p do anova é: ', strong(anova_dt$p), '. Ou seja, ',
                                 ifelse(anova_dt$p > intervalo_global_de_confianca, 'não existe difereça significativa entre os grupos.', 'existe difereça significativa entre, pelo menos dois grupos.')))

      #Posthoc
      posthoc <- data.frame(df %>% pairwise_t_test(vd ~ vi, paired = TRUE, p.adjust.method = "bonferroni"))
      posthoc <- posthoc[-c( 1, 4, 5,10)]
      posthoc <- posthoc[-6]
      names(posthoc) <- c('Grupo 1', 'Grupo 2', 'Estatística', 'DF', 'p')
      posthoc$`Significância` <- lapply(posthoc$p, function (x){
        return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
      })
      posthoc$`Estatística` <- signif(posthoc$`Estatística`, significancia_de_aproximacao)
      posthoc$p <- signif(posthoc$p, significancia_de_aproximacao)
      output$anova_rep_posthoc <- renderDT(posthoc)
    }
      #-------------------Friedman Test-------------------#
    {
      df <- values$bidimensional_data
      #Boxplot
      output$friedman_boxplot <- renderPlotly(plot_ly(df, y = df[[1]], color = df[[2]], type = 'box'))
      #Cálculo do Teste
      names(df) <- c('Dados', 'Grupo', 'id')
      df_friedman_dt <- (df %>% rstatix::friedman_test(Dados ~ Grupo | id) %>% data.frame())[3:6]
      df_friedman_dt[3] <- signif(df_friedman_dt[3], significancia_de_aproximacao)
      df_friedman_dt<- df_friedman_dt[-1]
      names(df_friedman_dt) <- c('Estatística', 'DF', 'p')
      output$friedman_dt <- renderDT(df_friedman_dt)

      output$friedman_interpretation <- renderUI(h4('O valor de p do teste de Friedman é: ', strong(df_friedman_dt$p), '. Ou seja, ',
                                 ifelse(df_friedman_dt$p > intervalo_global_de_confianca, 'não existe difereça significativa entre os grupos.', 'existe difereça significativa entre, pelo menos dois grupos.')))

      #Area de Efeito
      df_friedman_effectArea <- (df %>% rstatix::friedman_effsize(Dados ~ Grupo | id) %>% data.frame())[-(1:2)]
      df_friedman_effectArea[1] <- signif(df_friedman_effectArea[1], significancia_de_aproximacao)
      output$friedman_effectArea_interpretation <- renderUI(p('O valor da área de efeito do teste é: ', strong(df_friedman_effectArea[1])))

      #Sign's test
      df_friedman_sign_test <- df %>% rstatix::sign_test(Dados ~ Grupo, p.adjust.method = "bonferroni")
      df_friedman_sign_test <- df_friedman_sign_test[c(2, 3, 6, 7, 8)]
      # df_friedman_sign_test <- df_friedman_sign_test[,-4]
      df_friedman_sign_test[3] <- signif(df_friedman_sign_test[3], significancia_de_aproximacao)
      df_friedman_sign_test[5] <- signif(df_friedman_sign_test[5], significancia_de_aproximacao)
      names(df_friedman_sign_test) <- c('Grupo 1', 'Grupo 2', 'Estatística','DF', 'p')
      df_friedman_sign_test$`Significância` <- lapply(df_friedman_sign_test$p, function (x){
        return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
      })
      output$friedman_sign_test <- renderDT(df_friedman_sign_test)

      #Wilcoxon's test
      df_friedman_wilcoxon_test <- df %>% rstatix::wilcox_test(Dados ~ Grupo, p.adjust.method = "bonferroni", paired = TRUE)
      df_friedman_wilcoxon_test <- df_friedman_wilcoxon_test[c(2, 3, 6, 7)]
      df_friedman_wilcoxon_test[3] <- signif(df_friedman_wilcoxon_test[3], significancia_de_aproximacao)
      df_friedman_wilcoxon_test[4] <- signif(df_friedman_wilcoxon_test[4], significancia_de_aproximacao)
      names(df_friedman_wilcoxon_test) <- c('Grupo 1', 'Grupo 2', 'Estatística', 'p')
      df_friedman_wilcoxon_test$`Significância` <- lapply(df_friedman_wilcoxon_test$p, function (x){
        return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
      })
      output$friedman_wilcoxon_test <- renderDT(df_friedman_wilcoxon_test)
    }
    }
    #----------------ANOVA - Mixed Measures-------------#
    if(values$bidimensional_data_type == 'anova_mix'){
        df <- values$bidimensional_data
        names <- names(df)
        names(df) <- c('vd', 'vi1', 'vi2', 'id')
        model <- lm(df$vd ~ df$vi1 * df$vi2)

        #Normalidade
        output$anova_mix_qq_plot <- renderPlotly(ggplotly(ggqqplot(residuals(model), color = "#E7B800")))
        shap <- (df %>% group_by(vi1, vi2) %>% shapiro_test(vd))$p %>% mean() %>% signif(significancia_de_aproximacao)
        output$anova_mix_shapiro <- renderUI(h4('O valor p do teste de Shapiro-Wilk para estest dados são: ', shap, align = 'center'))

        #Outliers
        output$anova_mix_box_plot <- renderPlotly(plot_ly(df, y =~ vd, x =~ vi1, color =~ vi2, type = 'box') %>%
                                                     layout(boxmode = 'group', xaxis = list(title = names[2]), yaxis = list(title = names[1])))

        #Teste de Esfericidade de Mauchly
        anova_dt <- anova_test(data = df, dv = vd, wid = id,between = vi1, within = vi2)
        mauchly <- anova_dt$`Mauchly's Test for Sphericity`
        mauchly[[1]] <- c(names[2], paste0(names[2], ' - ', names[3]))
        output$anova_mix_mauchly_dt <- renderDT(mauchly[-4])
        output$anova_mix_mauchly_results <- renderUI(h4(
        'Pelo teste de esfericidade de mauchly, ', strong('p =', mauchly$p[2]), '.', if(mauchly$p[2] > intervalo_global_de_confianca)
           h4('As variâncias das diferenças entre os grupo',strong('são iguais'),' conforme o intervalo de confiança, assim podemos assumir a esfericidade.')
        else  h4('As variâncias das diferenças entre os grupo ',strong('não são iguais'),' assim não podemos assumir a esfericidade.'), br()))

        #Teste de Homogeneidade das Variâncias
        homogenity_var <- df %>% group_by(vi1) %>% levene_test(vd ~ vi2) %>% data.frame()
        names(homogenity_var) <- c(names[2], 'DF1', 'DF2', 'Estatística', 'p')
        homogenity_var$Estatística <- signif(homogenity_var$Estatística, significancia_de_aproximacao)
        homogenity_var$p <- signif(homogenity_var$p, significancia_de_aproximacao)
        homogenity_var$`Significância` <- lapply(homogenity_var$p, function (x){
          return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
        })
        output$anova_mix_levene <- renderDT(homogenity_var)

        #Teste de Homogeneidade das Covariâncias
        box_m <- box_m(df[, "vd", drop = FALSE], df$vi2)
        box_m <- box_m[-3:-4]
        box_m <- signif(box_m, significancia_de_aproximacao)
        names(box_m) <- c('Estatística', 'p')
        box_m$`Significância` <- lapply(box_m$p, function (x){
          return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
        })
        output$anova_mix_boxm <- renderDT(box_m)

        #ANOVA
        anova_table <-get_anova_table(anova_dt)
        anova_table[[1]] <- c(names[2], names[3], paste0(names[2], ' - ', names[3]))
        anova_table <- anova_table[-6]
        names(anova_table) <- c('Efeito', 'DFn', 'DFd', 'Estatística', 'p', 'ges')
        output$anova_mix_dt <- renderDT(anova_table)

        #Intepretação dos resultados
        output$anova_mix_results <- renderUI(h4('O valor de p do anova é: ', strong(anova_table$p[3]), '. Ou seja, ',
                                 ifelse(anova_table$p[3]> intervalo_global_de_confianca, 'não existe difereça significativa entre os grupos.', 'existe difereça significativa entre, pelo menos dois grupos.')))


        pwc1 <- df %>% group_by(vi1) %>% pairwise_t_test(vd ~ vi2, p.adjust.method = "bonferroni") %>% data.frame()
        pwc1 <- pwc1[-c(2, 5, 6, 8, 9, 10)]
        names(pwc1) <- c(names[2], 'Grupo 1', 'Grupo 2', 'p')
        pwc1$`Significância` <- lapply(pwc1$p, function (x){
          return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
        })
        output$anova_mix_pairwise_1 <- renderDT(pwc1)

        pwc2 <- df %>% group_by(vi2) %>% pairwise_t_test(vd ~ vi1, p.adjust.method = "bonferroni") %>% data.frame()
        pwc2 <- pwc2[-c(2, 5, 6, 8, 9, 10)]
        names(pwc2) <- c(names[3], 'Grupo 1', 'Grupo 2', 'p')
        pwc2$`Significância` <- lapply(pwc2$p, function (x){
          return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
        })
        output$anova_mix_pairwise_2 <- renderDT(pwc2)
      }
    #--------------------ANCOVA----------------------#
  {
    if(values$bidimensional_data_type == 'ancova'){
      df2 <- values$bidimensional_data
      nomes <- names(values$bidimensional_data)
      names(df2) <- c('vd', 'cov', 'vi')
      # output$ancova_linearity <- renderPlotly(renderANCOVA(values, options))
      output$ancova_linearity <- renderPlot(
        ggscatter(df2, x = "cov", y = "vd", color = "vi", add = "reg.line")+
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = vi))
      )
      regression <- as.data.frame(anova_test(df2, vd ~ vi*cov))
      regression[1] <- nomes
      regression <- regression[-6]
      output$ancova_regression <- renderDT(regression)
      output$ancova_regression_results <- renderUI(h4('O valor de', strong(' F (',regression$DFn[3], ', ',regression$DFd[3],')',' = ', regression$F[3]), ' e o valor de P é: ', strong(regression$p[3])))

      #Teste de levene
      levene <- ancova_levene_test(df2)
      output$ancova_levene_test <- renderDT(levene)
      output$ancova_levene_res <- renderUI(h4(
        ifelse(as.double(levene$p) > intervalo_global_de_confianca,
                  'O teste de Levene não foi significante',
                  'O teste de Levene foi significante'
        ),
        ' (',levene$p,ifelse(levene$p > intervalo_global_de_confianca,' > ', ' <= '),intervalo_global_de_confianca,'), ',
        ifelse(as.double(levene$p) > intervalo_global_de_confianca,
                 'assim podemos assumir a igualdade da variância dos resíduos para todos os grupos.',
                'não podemos assumir a igualdade da variância dos resíduos.'
        )
      ))

      #Teste de shapiro wilk
      shapiro <- ancova_shapiro_test(df2)
      output$ancova_shapiro_test <- renderDT(shapiro)

      output$ancova_shapiro_res <- renderUI(h4(
        ifelse(as.double(shapiro$p) > intervalo_global_de_confianca,
                  'O teste de Shapiro-Wilk não foi significante',
                  'O teste de Shapiro-Wilk foi significante'
        ),
        ' (',as.double(shapiro$p),ifelse(shapiro$p > intervalo_global_de_confianca,' > ', ' <= '),intervalo_global_de_confianca,'), ',
        ifelse(as.double(shapiro$p) > intervalo_global_de_confianca,
                 'assim podemos assumir a normalidade dos residuos.',
                'não podemos assumir a normalidade dos resíduos.'
        )
      ))

      #ANCOVA
      ancova_dt <- df2 %>% anova_test(vd ~ cov + vi)
      ancova_dt <- get_anova_table(ancova_dt) %>% as.data.frame()
      ancova_dt <- ancova_dt[-6]
      names(ancova_dt) <- c('Efeito', 'DFn', 'DFd', 'F', 'p', 'ges')
      ancova_dt$p <- signif(ancova_dt$p, significancia_de_aproximacao)
      ancova_dt[1] <- nomes[2:3]
      output$ancova_dt_res <- renderDT(ancova_dt)

      output$ancova_results <- renderUI(h4(
        ifelse(ancova_dt$p[2] > intervalo_global_de_confianca,
                  'O valor do teste de ANCOVA não foi significante',
                  'O valor do teste de ANCOVA foi significante'
        ),
        ' (',ancova_dt$p[2], ifelse(ancova_dt$p[2] > intervalo_global_de_confianca,' > ', ' <= '),intervalo_global_de_confianca,'), ',
        ifelse(ancova_dt$p[2] > intervalo_global_de_confianca,
                 'ou seja, não existe difereça significativa entre os grupos.',
                'ou seja, existe difereça significativa entre, pelo menos dois grupos.'
        )
      ))
      #Posthoc
      output$ancova_posthoc <- renderDT(posthoc_ancova_table(df2))
    }
    }
    #--------------------MANOVA----------------------#
  {
    if (values$bidimensional_data_type == 'manova'){
      df <- values$bidimensional_data
      df_ncol <- ncol(df)

      #Adicionando uma coluna na posição ncol(df), contendo um identificador para cada linha
      df <- df %>% df_select(vars = names(df)[seq_len(ncol(df))]) %>% add_column(id = seq_len(nrow(df)), .before = 1)

      #Boxplot
      output$manova_boxplot <- renderPlotly(ggboxplot(df, x = names(df)[df_ncol + 1], y = names(df)[2:df_ncol], merge = TRUE, palette = "jco") %>% ggplotly())

      #Tabela con o tesde de Mahalanobis, verificando os outliers multiplos
      # output$manova_outliers_multi <- df %>% group_by(var = names(df)[df_ncol + 1]) %>% mahalanobis_distance(-id) %>% filter(is.outlier == TRUE) %>% as.data.frame() %>% renderDT()

      #Teste de Normalidade
      manova_normality_multi_df <- df %>% df_select(vars = names(df)[2:df_ncol]) %>% mshapiro_test() %>% as.data.frame()
      names(manova_normality_multi_df) <- c('Estatística', 'p')
      manova_normality_multi_df <- signif(manova_normality_multi_df, significancia_de_aproximacao)
      output$manova_normality_multi <-  renderDT(manova_normality_multi_df)
      output$manova_interpret_normality_multi <-  renderUI(
        h4('O valor de p utilizando o teste múltiplo de Shapiro Wilk é de: ', strong(manova_normality_multi_df$p),
          ifelse(manova_normality_multi_df$p > intervalo_global_de_confianca, '(Estatísticamente normal)', '(Estatísticamente não normal)'))
      )

      #Verificação da Multicolinearidade
      dt_multicollinearity <- df %>% cor_test(vars = names(df)[2:df_ncol]) %>% as.data.frame()
      dt_multicollinearity <- dt_multicollinearity[-c(3, 6, 7, 8)]
      dt_multicollinearity[3:4] <- signif(dt_multicollinearity[3:4], significancia_de_aproximacao)
      names(dt_multicollinearity) <- c('Grupo 1', 'Grupo 2', 'Estatística', 'p')
      dt_multicollinearity$`Significância` <- lapply(dt_multicollinearity$p, function (x){
        return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
      })
      output$manova_multicollinearity <- dt_multicollinearity %>% renderDT()

      #Verificação do tamanho da amostra
      sample_size <- data.frame(table(df[ncol(df)]))
      output$manova_sample_size_dt <- renderDT(sample_size)
      output$manova_sample_size_assumpt <- renderUI(tagList(
         if(min(sample_size$Freq) > df_ncol - 1)
           p('A suposição de tamanho da amostra adequada é satisfeita, como ', strong(min(sample_size$Freq)), strong(' > '), strong(df_ncol - 1), '.')
         else
           p('A suposição de tamanho da amostra adequada não é satisfeita, como ', strong(min(sample_size$Freq)), strong(' <= '), strong(df_ncol - 1), '.')
      ))
      #Verificação da covariância
      dt_cov <- box_m(df[, names(df)[2:df_ncol]], df[,df_ncol + 1]) %>% as.data.frame()
      dt_cov <- data.frame(`Estatística` = dt_cov$statistic, p = dt_cov$p.value, df = dt_cov$parameter, `Método` = 'Box\'s M-test')
      dt_cov[c(1, 2)] <- signif(dt_cov[c(1, 2)], significancia_de_aproximacao)
      output$manova_covariancia_dt <-  renderDT(dt_cov)
      output$manova_covariancia_assumpt <- renderUI(tagList(
        p('O valor de p de acordo com o teste M de Box é: ', strong(dt_cov$p),
          ifelse(dt_cov$p > intervalo_global_de_confianca_Mbox, '. Ou seja, as covariâncias são estatísticamente iguais entre os grupos.',
                 '. Ou seja, as covariâncias são estatísticamente diferentes entre, pelo menos dois grupos.'))
      ))

      # Checando Homogeniedade das Variâncias
      df_gather <- df %>% gather(key = "variable", value = "value", names(df)[2:df_ncol]) %>% group_by(variable)
      names(df_gather) <- c('id', 'group', 'variable', 'value')
      df_gather <- df_gather %>% levene_test(value ~ group) %>% data.frame()
      df_gather[4:5] <- signif(df_gather[4:5], significancia_de_aproximacao)
      names(df_gather) <- c('Variáveis', 'DF1', 'DF2', 'Estatística', 'p')
      df_gather$`Significância` <- lapply(df_gather$p, function (x){
        return(ifelse(x > intervalo_global_de_confianca, 'Não significativo', 'Significativo'))
      })
      output$manova_variance <- renderDT(df_gather)

      #Computação do teste de MANOVA
      vars_dependentes <- switch(
        as.character(df_ncol),
        '3' = { cbind(df[[2]], df[[3]]) },
        '4' = { cbind(df[[2]], df[[3]], df[[4]]) },
        '5' = { cbind(df[[2]], df[[3]], df[[4]], df[[5]]) }
      )
      var_independente <- df[[ncol(df)]]
      manova_model <- manova(vars_dependentes ~ var_independente, data = df)
      output$manova_dt <- renderPrint(summary(manova_model))

      #Verificações Unidimensionais
      #Testes de Normalidade
      fig1 <- ggplotly(ggqqplot(df, names(df)[2], color = names(df)[df_ncol + 1], ggtheme = theme_bw()))
      fig2 <- ggplotly(ggqqplot(df, names(df)[3], color = names(df)[df_ncol + 1], ggtheme = theme_bw()) + theme(legend.position='none'))
      output$manova_normality_uni <- switch(
        as.character(df_ncol),
        '3' = renderPlotly(subplot(fig1, fig2, margin = 0.01)),
        '4' = {
          fig3 <- ggplotly(ggqqplot(df, names(df)[4], color = names(df)[df_ncol + 1], ggtheme = theme_bw()))
          renderPlotly(subplot(fig1, fig2, fig3, margin = 0.01, nrows = 2))
        },
        '5' = {
          fig3 <- ggplotly(ggqqplot(df, names(df)[4], color = names(df)[df_ncol + 1], ggtheme = theme_bw()))
          fig4 <- ggplotly(ggqqplot(df, names(df)[5], color = names(df)[df_ncol + 1], ggtheme = theme_bw()))
          renderPlotly(subplot(fig1, fig2, fig3, fig4, margin = 0.01, nrows = 2))
        }
      )
      #Shapiro Wilk
      dt_shapiro_uni <- df %>% group_by(var = names(df)[df_ncol + 1]) %>% shapiro_test(vars = names(df)[2:df_ncol]) %>% arrange(variable) %>% as.data.frame()
      dt_shapiro_uni[3:4] <- signif(dt_shapiro_uni[3:4], significancia_de_aproximacao)
      dt_shapiro_uni <- dt_shapiro_uni[2:4]
      names(dt_shapiro_uni) <- c('Variável', 'Estatística', 'p')
      dt_shapiro_uni$Normalidade <- lapply(dt_shapiro_uni$p, function (x){
        return(ifelse(x > intervalo_global_de_confianca, 'Normal', 'Não normal'))
      })
      output$manova_shapiro_uni <- dt_shapiro_uni %>% renderDT()

      #Testes de Linearidade
      results <- df %>% df_select(vars = names(df)[2:df_ncol]) %>% group_by(var = names(df)[df_ncol + 1]) %>% doo(~ggpairs(.) + theme_bw(), result = "plots")
      output$manova_linearity_plot <- renderPlot(results$plots[[1]])
      }
  }
  })

  #-------------------Load Tridimensional Data-------------------#
  observeEvent(input$load_tridimensional, {
    showTab(inputId = "tabs", target = "Gráfico em Mesh")

    values$usr_title <- paste0(input$title_id_import_tri)
    if(input$mesh_file_selector == 'example') {
      if(input$examp_select_mesh == 'gas') {
        options$num_sheets_imported_tri <- 5
        values$data_info_tri <- sapply(seq(options$num_sheets_imported_tri), function(x) {
          as.matrix(data.frame(read_excel(path = 'Data/DADOS_MESH_3D.xlsx', sheet = x)))
        })
      }
      else {
        options$num_sheets_imported_tri <- 20
        for (i in 1:options$num_sheets_imported_tri) {
          file <- paste0('Data/Machine Learning/', options$examp_select_mesh, '/', options$examp_select_mesh, '_', i, '.txt')
          values$data_info_tri[[i]] <- as.matrix(data.frame(read_table(file)))
        }
      }
      output$mesh_insert_result <- renderUI(
          h3(strong('Arquivo carregado: \"', input$examp_select_mesh,' - ', values$usr_title, '\"'))
         )
    }
    else if(input$mesh_file_selector == 'import') {
      options$num_sheets_imported_tri <- input$num_sheets_imported_tri
      if(input$num_sheets_imported_tri == 1){
        values$data_info_tri <- list(NULL, as.matrix(data.frame(read_excel(path = input$file_imported_tri$datapath, sheet = 1))))
        values$data_info_tri <- values$data_info_tri[-1]
      }
      else {
        values$data_info_tri <- sapply(seq(input$num_sheets_imported_tri), function(x) {
          as.matrix(data.frame(read_excel(path = input$file_imported_tri$datapath, sheet = x)))
        })
      }
      output$mesh_insert_result <- renderUI(
          h3(strong('Arquivo carregado: \"', values$usr_title, '\"'))
      )
    }

  })
#----------------------------VARIAVEIS GRÁFICOS ----------------------------
  observe({
    #Tabela
    options$transpose_table <- input$transpose_table

    #Histograma
  { #Histograma linear
  { options$stack_histogram <- input$switch_stack_histogram
    options$bargap_histogram <- input$bargap_histogram
    options$bargap_histogram_level <- input$bargap_histogram_level / 10
    options$bandwidth_histogram <- if (input$bins_histogram) 0 else input$bandwidth_histogram
    options$opacity_histogram <- input$opacity_histogram

    #Histograma de densidade
    options$show_density_histogram <- input$show_density_histogram
    options$density_histogram_area <- if (input$density_histogram_area) 'tozeroy' else 'none'
    options$density_histogram_scale <- input$density_histogram_scale
    options$density_histogram_line_opacity <- input$density_histogram_line_opacity
    options$density_histogram_area_opacity <- input$density_histogram_area_opacity }

    #Histograma - Ridges
  { options$bandwidth_ridges_histogram <- input$bandwidth_ridges_histogram
    options$scale_ridges_histogram <- input$scale_ridges_histogram
    options$show_quartis_ridges_histogram <- input$show_quartis_ridges_histogram
    options$n_quartis_ridges_histogram <- input$n_quartis_ridges_histogram
    # options$opacity_ridges_histogram <- input$opacity_ridges_histogram

    #Pontos
    options$points_ridges_histogram <- input$points_ridges_histogram
    options$points_size_ridges_histogram <- input$points_size_ridges_histogram
    options$points_opacity_ridges_histogram <- input$points_opacity_ridges_histogram
    options$points_shape_ridges_histogram <- input$points_shape_ridges_histogram
    options$points_position_ridges_histogram <- input$points_position_ridges_histogram

    #Cor
    options$ridges_color <- input$ridges_color
    options$reverse_ridges_histogram <- if (input$reverse_ridges_histogram) -1 else 1 } }

    # Box plot
  { # Algoritmo
    options$box_algorithm <- input$box_algorithm
    options$meanline_box <- input$meanline_box
    #Pontos
    options$point_box <- if (input$point_box) 'all' else 'outliers'
    options$jitter_box <- input$jitter_box
    options$jitter_pointpos <- input$jitter_pointpos
    options$points_size_box <- input$points_size_box
    options$points_width_box <- input$points_width_box
    options$points_opacity_box <- input$points_opacity_box
    options$dot_box_shape <- input$dot_box_shape }

    #Violin
  { #Algoritmo
    options$violin_algorithm <- input$violin_algorithm
    options$meanline_violin <- input$meanline_violin
    #Largura de banda (violino)
    options$bandwidth_violin <- if (input$bandwidth_violin) 0 else input$bandwidth_violin_size }

    # Gráfico de pontos
  { #Pontos
    options$point_violin <- if (input$point_violin) 'all' else 'outliers'
    options$jitter_violin <- input$jitter_violin
    options$jitter_pointpos <- input$jitter_pointpos
    options$points_size_violin <- input$points_size_violin
    options$points_width_violin <- input$points_width_violin
    options$points_opacity_violin <- input$points_opacity_violin
    options$dot_violin_shape <- input$dot_violin_shape

    #Pontos Dot Plot
    options$shape_markers_dot_plot <- input$shape_markers_dot_plot
    options$line_markers_dot_plot <- input$line_markers_dot_plot
    options$size_markers_dot_plot <- input$size_markers_dot_plot
    options$opacity_markers_dot_plot <- input$opacity_markers_dot_plot

    #linhas Dot plot
    options$line_dot_plot <- input$line_dot_plot
    options$bins_dot_plot <- input$bins_dot_plot
    options$opacity_line_dot_plot <- input$opacity_line_dot_plot

    #Ellipse Dot plot
    options$ellipse_dot_plot <- input$ellipse_dot_plot
    options$ellipse_line_format <- input$ellipse_line_format
    options$ellipse_area_dot_plot <- input$ellipse_area_dot_plot
    options$area_opacity_ellipse <- input$area_opacity_ellipse
    options$ci_ellipse <- input$ci_ellipse }

    #Gráfico de BeeSwarm
  { #Pontos BeeSwarm
    options$shape_markers_beeswarm <- input$shape_markers_beeswarm
    options$line_markers_beeswarm <- input$line_markers_beeswarm
    options$size_markers_beeswarm <- input$size_markers_beeswarm
    options$opacity_markers_beeswarm <- input$opacity_markers_beeswarm

    #Configurações BeeSwarm
    options$size_beeswarm <- input$size_beeswarm
    options$spacing_markers_beeswarm <- input$spacing_markers_beeswarm
    options$side_beeswarm <- input$side_beeswarm
    options$subplots_beeswarm <- input$subplots_beeswarm
    options$width_beeswarm <- input$width_beeswarm
    options$beeswarm_method <- input$beeswarm_method
    options$priority_beeswarm <- input$priority_beeswarm }

    #Gráfico de Densidade
  { if (input$area_density_plot) options$area_density <- 'tozeroy' else options$area_density <- 'none'
    if (input$line_density_plot) options$line_density <- 'line' else options$line_density <- 'none'
    options$algorithm_density_plot <- input$algorithm_density_plot }

    # Gráfico de erro
  { #Barra de erro
    options$error_bar <- input$bar_error_bar
    options$opacity_error_bar <- input$opacity_error_bar

    #Linha de erro
    options$error_line <- input$error_line
    options$opacity_error_line <- input$opacity_error_line
    options$error_algorithm <- input$error_algorithm
    if (options$error_algorithm == 'ci')
      options$alpha_ci <- input$slider_ci

    #Pontos
    options$markers_shape_error_bar <- input$markers_shape_error_bar
    options$line_markers_error_bar <- input$line_markers_error_bar
    options$size_markers_error_bar <- input$size_markers_error_bar
    options$opacity_markers_error_bar <- input$opacity_markers_error_bar }

    #Histograma 3D
  { #Gráfico
    options$bins_histogram3d <- input$bins_histogram3d
    options$opacity_histogram3d <- input$opacity_histogram3d

    #Linhas
    options$width_line_histogram3d <- input$width_line_histogram3d

    #Pontos
    options$markers_histogram3d <- input$markers_histogram3d
    options$size_markers_histogram3d <- input$size_markers_histogram3d
    options$shape_markers_histogram3d <- input$shape_markers_histogram3d }

    #Gráfico de densidade 3D
  { options$width_markers_density3d <- input$width_markers_density3d
    options$algorithm_density_plot3d <- input$algorithm_density_plot3d }

    #Gráfico de pontos 3D
  { #Pontos
    options$bins_scatter3d <- input$bins_scatter3d
    options$line_markers_scatter3d <- input$line_markers_scatter3d
    options$shape_markers_scatter3d <- input$shape_markers_scatter3d
    options$size_markers_scatter3d <- input$size_markers_scatter3d
    options$opacity_markers_scatter3d <- input$opacity_markers_scatter3d

    #Elipse
    options$ellipse_scatter3d <- input$ellipse_scatter3d
    options$ci_ellipse3d <- input$ci_ellipse3d
    options$ellipse3d_line_format <- input$ellipse3d_line_format
    #Area ellipse
    options$area_ellipse3d <- input$area_ellipse3d
    options$opacity_area_ellipse3d <- input$opacity_area_ellipse3d
    #Line ellipse
    options$line_ellipse3d <- input$line_ellipse3d
    options$opacity_line_ellipse3d <- input$opacity_line_ellipse3d
    options$width_line_ellipse3d <- input$width_line_ellipse3d }

    #Gráfico de pontos BeeSwarm 3D
  { #Pontos BeeSwarm
    options$shape_markers_beeswarm3d <- input$shape_markers_beeswarm3d
    options$line_markers_beeswarm3d <- input$line_markers_beeswarm3d
    options$size_markers_beeswarm3d <- input$size_markers_beeswarm3d
    options$opacity_markers_beeswarm3d <- input$opacity_markers_beeswarm3d

    #Configurações BeeSwarm
    options$spacing_markers_beeswarm3d <- input$spacing_markers_beeswarm3d
    options$side_beeswarm3d <- input$side_beeswarm3d
    options$beeswarm_method3d <- input$beeswarm_method3d
    options$priority_beeswarm3d <- input$priority_beeswarm3d }

    #Gráfico de Barras 3D
  { options$algorithm_bar3d <- input$algorithm_bar3d
    options$spacing_bar_bar3d <- input$spacing_bar_bar3d
    options$opacity_bar_bar3d <- input$opacity_bar_bar3d }

    #ANCOVA
  { options$ancova_line_width <- input$ancova_line_width
    options$ancova_marker_opacity <- input$ancova_marker_opacity
    options$ancova_marker_size <- input$ancova_marker_size
  }

    #Gráfico em Mesh
    {
      options$examp_select_mesh <- input$examp_select_mesh
      options$checkbox_mesh <- input$checkbox_mesh
      options$nrow_x_mesh3d <- input$nrow_x_mesh3d
    }
  })
#----------------------------LAYOUT GRÁFICOS ----------------------------
  observe({
      options$bgColorPlotly <- if (!input$default_plot_color) {
        if (input$bgcolor_plot_default == 'personal') input$personal_bgcolor_plot_default else input$bgcolor_plot_default
      }
      else bgColorPlotly

      if(input$histogram_tabs == 'linear_histogram') {
        #Cores
        options$colors_linear_histogram <- input$colors_linear_histogram
        options$bgcolor_linear_histogram <- input$bgcolor_linear_histogram
        options$personal_bgcolor_linear_histogram <- input$personal_bgcolor_linear_histogram

        #Eixos
        options$axis_x_linear_histogram <- input$axis_x_linear_histogram
        options$axis_y_linear_histogram <- input$axis_y_linear_histogram

        #Legenda
        options$legend_linear_histogram <- input$legend_linear_histogram
        options$border_legend_linear_histogram <- input$border_legend_linear_histogram
        options$title_legend_linear_histogram <- input$title_legend_linear_histogram
        options$bold_title_legend_linear_histogram <- input$bold_title_legend_linear_histogram
        options$item_size_legend_linear_histogram <- input$item_size_legend_linear_histogram
        options$orientation_legend_linear_histogram <- input$orientation_legend_linear_histogram
      }
      if(input$histogram_tabs == 'ridges_histogram') {
        #Cores
        options$colors_ridges_histogram <- input$colors_ridges_histogram
        options$bgcolor_ridges_histogram <- input$bgcolor_ridges_histogram
        options$personal_bgcolor_ridges_histogram <- input$personal_bgcolor_ridges_histogram

        #Eixos
        options$axis_x_ridges_histogram <- input$axis_x_ridges_histogram
        options$axis_y_ridges_histogram <- input$axis_y_ridges_histogram
        #Legenda
        options$legend_ridges_histogram <- input$legend_ridges_histogram
        options$border_legend_ridges_histogram <- input$border_legend_ridges_histogram
        options$title_legend_ridges_histogram <- input$title_legend_ridges_histogram
        options$bold_title_legend_ridges_histogram <- input$bold_title_legend_ridges_histogram
        options$item_size_legend_ridges_histogram <- input$item_size_legend_ridges_histogram
        options$orientation_legend_ridges_histogram <- input$orientation_legend_ridges_histogram
    }
    {
      #Cores
      options$colors_box <- input$colors_box
      options$bgcolor_box <- input$bgcolor_box
      options$personal_bgcolor_box <- input$personal_bgcolor_box

      #Eixos
      options$axis_x_box <- input$axis_x_box
      options$axis_y_box <- input$axis_y_box

      #Legenda
      options$legend_box <- input$legend_box
      options$border_legend_box <- input$border_legend_box
      options$title_legend_box <- input$title_legend_box
      options$bold_title_legend_box <- input$bold_title_legend_box
      options$item_size_legend_box <- input$item_size_legend_box
      options$orientation_legend_box <- input$orientation_legend_box
    }
    {
      #Cores
      options$colors_violin <- input$colors_violin
      options$bgcolor_violin <- input$bgcolor_violin
      options$personal_bgcolor_violin <- input$personal_bgcolor_violin

      #Eixos
      options$axis_x_violin <- input$axis_x_violin
      options$axis_y_violin <- input$axis_y_violin

      #Legenda
      options$legend_violin <- input$legend_violin
      options$border_legend_violin <- input$border_legend_violin
      options$title_legend_violin <- input$title_legend_violin
      options$bold_title_legend_violin <- input$bold_title_legend_violin
      options$item_size_legend_violin <- input$item_size_legend_violin
      options$orientation_legend_violin <- input$orientation_legend_violin
    }
    {
    {
        #Cores
        options$colors_simple_dot_plot <- input$colors_simple_dot_plot
        options$bgcolor_simple_dot_plot <- input$bgcolor_simple_dot_plot
        options$personal_bgcolor_simple_dot_plot <- input$personal_bgcolor_simple_dot_plot

        #Eixos
        options$axis_x_simple_dot_plot <- input$axis_x_simple_dot_plot
        options$axis_y_simple_dot_plot <- input$axis_y_simple_dot_plot

      #Legenda
      options$legend_simple_dot_plot <- input$legend_simple_dot_plot
      options$border_legend_simple_dot_plot <- input$border_legend_simple_dot_plot
      options$title_legend_simple_dot_plot <- input$title_legend_simple_dot_plot
      options$bold_title_legend_simple_dot_plot <- input$bold_title_legend_simple_dot_plot
      options$item_size_legend_simple_dot_plot <- input$item_size_legend_simple_dot_plot
      options$orientation_legend_simple_dot_plot <- input$orientation_legend_simple_dot_plot
      }
    {
        #Cores
        options$colors_beeswarm_dot_plot <- input$colors_beeswarm_dot_plot
        options$bgcolor_beeswarm_dot_plot <- input$bgcolor_beeswarm_dot_plot
        options$personal_bgcolor_beeswarm_dot_plot <- input$personal_bgcolor_beeswarm_dot_plot

        #Eixos
        options$axis_x_beeswarm_dot_plot <- input$axis_x_beeswarm_dot_plot
        options$axis_y_beeswarm_dot_plot <- input$axis_y_beeswarm_dot_plot

      #Legenda
      options$legend_beeswarm_dot_plot <- input$legend_beeswarm_dot_plot
      options$border_legend_beeswarm_dot_plot <- input$border_legend_beeswarm_dot_plot
      options$title_legend_beeswarm_dot_plot <- input$title_legend_beeswarm_dot_plot
      options$bold_title_legend_beeswarm_dot_plot <- input$bold_title_legend_beeswarm_dot_plot
      options$item_size_legend_beeswarm_dot_plot <- input$item_size_legend_beeswarm_dot_plot
      options$orientation_legend_beeswarm_dot_plot <- input$orientation_legend_beeswarm_dot_plot
    }
    }
    {
      #Cores
      options$colors_density_plot <- input$colors_density_plot
      options$bgcolor_density_plot <- input$bgcolor_density_plot
      options$personal_bgcolor_density_plot <- input$personal_bgcolor_density_plot

      #Eixos
      options$axis_x_density_plot <- input$axis_x_density_plot
      options$axis_y_density_plot <- input$axis_y_density_plot

      #Legenda
      options$legend_density_plot <- input$legend_density_plot
      options$border_legend_density_plot <- input$border_legend_density_plot
      options$title_legend_density_plot <- input$title_legend_density_plot
      options$bold_title_legend_density_plot <- input$bold_title_legend_density_plot
      options$item_size_legend_density_plot <- input$item_size_legend_density_plot
      options$orientation_legend_density_plot <- input$orientation_legend_density_plot
    }
    {
      #Cores
      options$colors_error_bar <- input$colors_error_bar
      options$bgcolor_error_bar <- input$bgcolor_error_bar
      options$personal_bgcolor_error_bar <- input$personal_bgcolor_error_bar

      #Eixos
      options$axis_x_error_bar <- input$axis_x_error_bar
      options$axis_y_error_bar <- input$axis_y_error_bar

      #Legenda
      options$legend_error_bar <- input$legend_error_bar
      options$border_legend_error_bar <- input$border_legend_error_bar
      options$title_legend_error_bar <- input$title_legend_error_bar
      options$bold_title_legend_error_bar <- input$bold_title_legend_error_bar
      options$item_size_legend_error_bar <- input$item_size_legend_error_bar
      options$orientation_legend_error_bar <- input$orientation_legend_error_bar
    }
    {
      #Cores
      options$colors_check_norm_d <- input$colors_check_norm_d
      options$bgcolor_check_norm_d <- input$bgcolor_check_norm_d
      options$personal_bgcolor_check_norm_d <- input$personal_bgcolor_check_norm_d

      #Eixos
      options$axis_x_check_norm_d <- input$axis_x_check_norm_d
      options$axis_y_check_norm_d <- input$axis_y_check_norm_d

      #Legenda
      options$legend_check_norm_d <- input$legend_check_norm_d
      options$border_legend_check_norm_d <- input$border_legend_check_norm_d
      options$title_legend_check_norm_d <- input$title_legend_check_norm_d
      options$bold_title_legend_check_norm_d <- input$bold_title_legend_check_norm_d
      options$item_size_legend_check_norm_d <- input$item_size_legend_check_norm_d
      options$orientation_legend_check_norm_d <- input$orientation_legend_check_norm_d
    }
    {
      #Cores
      options$colors_check_norm_qq <- input$colors_check_norm_qq
      options$bgcolor_check_norm_qq <- input$bgcolor_check_norm_qq
      options$personal_bgcolor_check_norm_qq <- input$personal_bgcolor_check_norm_qq

      #Eixos
      options$axis_x_check_norm_qq <- input$axis_x_check_norm_qq
      options$axis_y_check_norm_qq <- input$axis_y_check_norm_qq

      #Legenda
      options$legend_check_norm_qq <- input$legend_check_norm_qq
      options$border_legend_check_norm_qq <- input$border_legend_check_norm_qq
      options$title_legend_check_norm_qq <- input$title_legend_check_norm_qq
      options$bold_title_legend_check_norm_qq <- input$bold_title_legend_check_norm_qq
      options$item_size_legend_check_norm_qq <- input$item_size_legend_check_norm_qq
      options$orientation_legend_check_norm_qq <- input$orientation_legend_check_norm_qq
    }
    {
      #Cores
      options$colors_histogram_3d <- input$colors_histogram_3d
      options$bgcolor_histogram_3d <- input$bgcolor_histogram_3d
      options$personal_bgcolor_histogram_3d <- input$personal_bgcolor_histogram_3d

      #Eixos
      options$axis_x_histogram_3d <- input$axis_x_histogram_3d
      options$axis_y_histogram_3d <- input$axis_y_histogram_3d
      options$axis_z_histogram_3d <- input$axis_z_histogram_3d

      #Legenda
      options$legend_histogram_3d <- input$legend_histogram_3d
      options$border_legend_histogram_3d <- input$border_legend_histogram_3d
      options$title_legend_histogram_3d <- input$title_legend_histogram_3d
      options$bold_title_legend_histogram_3d <- input$bold_title_legend_histogram_3d
      options$item_size_legend_histogram_3d <- input$item_size_legend_histogram_3d
      options$orientation_legend_histogram_3d <- input$orientation_legend_histogram_3d
    }
    {
      #Cores
      options$colors_density_plot_3d <- input$colors_density_plot_3d
      options$bgcolor_density_plot_3d <- input$bgcolor_density_plot_3d
      options$personal_bgcolor_density_plot_3d <- input$personal_bgcolor_density_plot_3d

      #Eixos
      options$axis_x_density_plot_3d <- input$axis_x_density_plot_3d
      options$axis_y_density_plot_3d <- input$axis_y_density_plot_3d
      options$axis_z_density_plot_3d <- input$axis_z_density_plot_3d

      #Legenda
      options$legend_density_plot_3d <- input$legend_density_plot_3d
      options$border_legend_density_plot_3d <- input$border_legend_density_plot_3d
      options$title_legend_density_plot_3d <- input$title_legend_density_plot_3d
      options$bold_title_legend_density_plot_3d <- input$bold_title_legend_density_plot_3d
      options$item_size_legend_density_plot_3d <- input$item_size_legend_density_plot_3d
      options$orientation_legend_density_plot_3d <- input$orientation_legend_density_plot_3d
    }
    {
      #Cores
      options$colors_dot_plot_3d <- input$colors_dot_plot_3d
      options$bgcolor_dot_plot_3d <- input$bgcolor_dot_plot_3d
      options$personal_bgcolor_dot_plot_3d <- input$personal_bgcolor_dot_plot_3d

      #Eixos
      options$axis_x_dot_plot_3d <- input$axis_x_dot_plot_3d
      options$axis_y_dot_plot_3d <- input$axis_y_dot_plot_3d
      options$axis_z_dot_plot_3d <- input$axis_z_dot_plot_3d

      #Legenda
      options$legend_dot_plot_3d <- input$legend_dot_plot_3d
      options$border_legend_dot_plot_3d <- input$border_legend_dot_plot_3d
      options$title_legend_dot_plot_3d <- input$title_legend_dot_plot_3d
      options$bold_title_legend_dot_plot_3d <- input$bold_title_legend_dot_plot_3d
      options$item_size_legend_dot_plot_3d <- input$item_size_legend_dot_plot_3d
      options$orientation_legend_dot_plot_3d <- input$orientation_legend_dot_plot_3d
    }
    {
      #Cores
      options$colors_bar_plot_3d <- input$colors_bar_plot_3d
      options$bgcolor_bar_plot_3d <- input$bgcolor_bar_plot_3d
      options$personal_bgcolor_bar_plot_3d <- input$personal_bgcolor_bar_plot_3d

      #Eixos
      options$axis_x_bar_plot_3d <- input$axis_x_bar_plot_3d
      options$axis_y_bar_plot_3d <- input$axis_y_bar_plot_3d
      options$axis_z_bar_plot_3d <- input$axis_z_bar_plot_3d

      #Legenda
      options$legend_bar_plot_3d <- input$legend_bar_plot_3d
      options$border_legend_bar_plot_3d <- input$border_legend_bar_plot_3d
      options$title_legend_bar_plot_3d <- input$title_legend_bar_plot_3d
      options$bold_title_legend_bar_plot_3d <- input$bold_title_legend_bar_plot_3d
      options$item_size_legend_bar_plot_3d <- input$item_size_legend_bar_plot_3d
      options$orientation_legend_bar_plot_3d <- input$orientation_legend_bar_plot_3d
    }
    {
      #Cores
      options$colors_ancova_plot <- input$colors_ancova_plot
      options$bgcolor_ancova_plot <- input$bgcolor_ancova_plot
      options$personal_bgcolor_ancova_plot <- input$personal_bgcolor_ancova_plot

      #Eixos
      options$axis_x_ancova_plot <- input$axis_x_ancova_plot
      options$axis_y_ancova_plot <- input$axis_y_ancova_plot

      #Legenda
      options$legend_ancova_plot <- input$legend_ancova_plot
      options$border_legend_ancova_plot <- input$border_legend_ancova_plot
      options$title_legend_ancova_plot <- input$title_legend_ancova_plot
      options$bold_title_legend_ancova_plot <- input$bold_title_legend_ancova_plot
      options$item_size_legend_ancova_plot <- input$item_size_legend_ancova_plot
      options$orientation_legend_ancova_plot <- input$orientation_legend_ancova_plot
    }
    {
      #Cores
      options$colors_mesh_3d <- input$colors_mesh_3d
      options$bgcolor_mesh_3d <- input$bgcolor_mesh_3d
      options$personal_bgcolor_mesh_3d <- input$personal_bgcolor_mesh_3d

      #Eixos
      options$axis_x_mesh_3d <- input$axis_x_mesh_3d
      options$axis_y_mesh_3d <- input$axis_y_mesh_3d
      options$axis_z_mesh_3d <- input$axis_z_mesh_3d

      #Legenda
      options$legend_mesh_3d <- input$legend_mesh_3d
      options$border_legend_mesh_3d <- input$border_legend_mesh_3d
      options$title_legend_mesh_3d <- input$title_legend_mesh_3d
      options$bold_title_legend_mesh_3d <- input$bold_title_legend_mesh_3d
      options$item_size_legend_mesh_3d <- input$item_size_legend_mesh_3d
      options$orientation_legend_mesh_3d <- input$orientation_legend_mesh_3d
    }
    })

#----------------------------SUMMARY----------------------------
  output$title_name_summary  <- renderUI(h1(strong(values$usr_title)))
  output$summary_data_table <- renderDT(values$data_info)
  output$summary_text <- renderDT(summaryDataTable(values, options))

  {
    #----------------------------BASIC PLOTS----------------------------
    output$plotly_linear_histogram <- renderPlotly(renderHistogramLinear(values, options))

    output$plot_histogram <- renderPlot(renderHistogramRidges(values, options))

    output$plotly_box_plot <- renderPlotly(renderBoxPlot(values, options))

    output$plotly_violin <- renderPlotly(renderViolinPlot(values, options))

    output$plotly_dot_plot <- renderPlotly(renderDotPlot(values, options))

    output$plotly_beeswarm_dot_plot <- renderPlotly(renderBeeSwarm(values, options))

    output$plotly_density_plot <- renderPlotly(renderDensityPlot(values, options))

    output$plotly_error_bar <- renderPlotly(renderErrorBar(values, options))

    output$plotly_histogram3d <- renderPlotly(renderHistogram3d(values, options))

    output$plotly_density3d <- renderPlotly(renderDensityPlot3d(values, options))

    output$plotly_scatter3d <- renderPlotly(renderScatterPlot3d(values, options))

    output$plotly_beeswarm3d <- renderPlotly(renderBeeSwarm3d(values, options))

    output$plotly_bar3d <- renderPlotly(barHistogram3d(values, options))

    output$transform_bi_table <- renderUI(
      tagList(
        selectInput(
          inputId = 'transform_bi_variable',
          label = 'Escolha a variável para ser plotada: ',
          choices = names(values$bidimensional_data),
          selected = ''
        ),
        actionButton("load_transform_bi",
                     strong('Carregue!'),
                     style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                     width = "80%",
                     class = "btn-info"
        )
      )
    )

    output$checkbox_mesh_ui <- renderUI(
      checkboxGroupButtons(
        inputId = "checkbox_mesh",
        label = '',
        choices = seq(options$num_sheets_imported_tri),
        checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                         no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
      )
    )

    output$plotly_mesh3d <- renderPlotly(renderMesh3D(values, options)) }
}