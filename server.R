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
  output$homogenity_results <- renderUI(tagList(h2(strong('Escolha os seus dados na barra de controle à esquerda:'),align = 'center'), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()))
  output$transform_norm_results <- renderUI(tagList(h2(strong('Escolha os seus dados na barra de controle à esquerda:'),align = 'center'), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()))
  output$table_transform_bi_data_output <- renderUI(tagList(h2(strong('Escolha a variável na barra de controle à esquerda:'),align = 'center'), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()))
  output$title_name_insert_bi <- renderUI(h2(strong('Digite os dados:')))
  frase_erro <- 'Dados inseridos incorretamente, verificar o manual para mais informações.'

  #Iniciar as planilhas
  output$user_data <- renderRHandsontable({ rhandsontable(data = data.frame(matrix('', 1000, 1000))) })
  output$user_data_bi <- renderRHandsontable({ rhandsontable(data = data.frame(matrix('', 1000, 1000))) })

  #Esconder todos os paineis
  hideTab(inputId = "tabs", target = "Gráficos 2D")
  hideTab(inputId = "tabs", target = "Gráficos 3D")
  hideTab(inputId = "tabs", target = "Avaliando os dados")
  hideTab(inputId = "tabs", target = "Informações gerais")

  hideTab(inputId = "tabs", target = "Transformar seus dados")
  hideTab(inputId = "tabs", target = "Comparando duas médias")
  hideTab(inputId = "tabs", target = "Comparando multiplas médias")

  hideTab(inputId = "tabs", target = 'Gráfico em Mesh')

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
      #Duas Médias
      if(input$examp_select_bi == 'gas') {
        dt <- data.frame(read.xlsx('Data/exemplo_two_means.xlsx'))
        type <- 'two_col'
      }
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
        dt <- data.frame(score = s2[3], time = s2[2], id = as.character(s2[[1]]))
        type <- 'anova_2groups'
      }
      if(input$examp_select_bi == 'gas3') {
        dt <- data.frame(read.xlsx('Data/exemplo1ANCOVA.xlsx'))
        type <- 'ancova'
      }
      if(input$examp_select_bi == 'anxiety') {
        data("anxiety", package = "datarium")
        dt <- as.data.frame(anxiety[, 2:4])
        dt$Group <- dt$group
        dt <- dt[,-1]
        names(dt) <- c('T1', 'T2', 'Group')
        type <- 'ancova'
      }
      if(input$examp_select_bi == 'iris_manova'){
        dt <- iris
        type <- 'manova'
      }
    }
    else if(input$file_selector_bi == 'import') {
      dt <- data.frame(read.xlsx(input$file_imported_bi$datapath))
      type <- input$imported_bi_type
    }
    if ((type %in% c('two_col', 'anova','anova_2groups', 'ancova', 'manova')) & checandoDados(dt, type)){
      showTab(inputId = "tabs", target = "Comparando duas médias")
      showTab(inputId = "tabs", target = "Avaliando os dados")
      showTab(inputId = "tabs", target = "Comparando multiplas médias")
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
    dt <- data.frame(hot_to_r(input$user_data_bi))
    empty_columns <- colSums(dt == "") == nrow(dt)
    dt <- dt[, !empty_columns]

    if(ncol(dt) != 0) {
      empty_rows <- rowSums(dt == "") == ncol(dt)
      dt <- dt[!empty_rows,]
    } else dt <- NULL

    if(!is.null(dt)) {
      names(dt) <- dt[1,]
      # names(dt) <- gsub('\\.', ' ', names(dt))
      dt <- dt[-1,]

      dt <- as.data.frame(dt)
    } else output$rest_of_sidebar <- renderMenu(NULL)

    type <- output$inserted_bi_type
    if ((type %in% c('two_col', 'anova','anova_2groups', 'ancova', 'manova')) & checandoDados(dt, type)){
      values$usr_title <- paste0(input$title_id_insert_bi)
      output$title_name_insert_bi <- renderUI(
      h2(strong(values$usr_title)))
      showTab(inputId = "tabs", target = "Comparando duas médias")
      showTab(inputId = "tabs", target = "Avaliando os dados")
      showTab(inputId = "tabs", target = "Comparando multiplas médias")

      values$bidimensional_data_type <- type
      values$bidimensional_data <- dt
    }
    else{
      hideTab(inputId = "tabs", target = "Comparando duas médias")
      hideTab(inputId = "tabs", target = "Avaliando os dados")
      hideTab(inputId = "tabs", target = "Comparando multiplas médias")
    }
  })

  #-------------------Avaliando os dados-------------------#
  observe(if(!is.null(values$bidimensional_data)){
    if(ncol(values$bidimensional_data) == 2 & is.numeric(values$bidimensional_data[,1]) & is.numeric(values$bidimensional_data[,2])) {
    #-------------------Homogenity of Variance-------------------#
    {
        output$homogenity_results <- renderUI(tagList(
          uiOutput('homogenity_method_name'),
          DTOutput('homogenity_table'),
          uiOutput('homogenity_method_results')
        ))
        choosen <- input$homogenity_tests
        ci <- input$homogenity_ci
        data <- contingency_data(values$bidimensional_data)

        if (choosen == 'f_test') {
          first <- names(values$bidimensional_data)[1]
          sec <- names(values$bidimensional_data)[2]
          output$homogenity_method_name <- renderUI(h3('Teste F para comparação de duas variáveis'))

          data <- data.frame(data[which(data$`Classificação` == first | data$`Classificação` == sec),]$Dados, data[which(data$Classificação == first | data$Classificação == sec),]$`Classificação`)
          names(data) <- c('Dados', 'Classificacao')

          res <- var.test(Dados ~ Classificacao, data = data, conf.level = ci)
          dt <- signif(data.frame(F = res$statistic, Num_df = res$parameter[1], Denom_df = res$parameter[2], p = res$p.value), 4)

          output$homogenity_table <- renderDT(dt)
          output$homogenity_method_results <- renderUI(
            tagList(
              h4('Com um intervalo de confiança de ', ci * 100, '%:'),
              h4(signif(res$conf.int[1], 4), signif(res$conf.int[2], 4)), br(),
              if (signif(res$p.value, 4) == 0) h4('O valor de p é ', strong('aproximadadente 0'), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                                                  '. Assim sugere que ', strong('há diferênças significantes'), ' entre as duas variâncias')
              else if (1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('maior do que o nivel de significância', 1 - ci),
                                                '. Assim sugere que ', strong('não há diferênças significantes'), ' entre as duas variâncias')
              else h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                      '. Assim sugere que ', strong('há diferênças significantes'), ' entre as duas variâncias')
            )
          )
        }
        else if (choosen == 'bartlett_test') {
          res <- bartlett.test(Dados ~ Classificação, data = data)
          output$homogenity_method_name <- renderUI(h3('Teste de Bartlett para comparação múltiplas variáveis'))
          output$homogenity_table <- renderDT(signif(data.frame(F = res$statistic, df = res$parameter, p = res$p.value)), 4)
          output$homogenity_method_results <- renderUI(
            tagList(
              if (signif(res$p.value, 4) == 0) h4('O valor de p é ', strong('aproximadadente 0'), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                                                  '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
              else if (1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('maior do que o nivel de significância', 1 - ci),
                                                '. Assim sugere que ', strong('não há diferênças significantes'), ' entre as variâncias das variáveis')
              else h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                      '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
            )
          )

        }
        else if (choosen == 'levene_test') {
          output$homogenity_method_name <- renderUI(h3('Teste de Levene para comparação múltiplas variáveis'))
          res <- leveneTest(Dados ~ Classificação, data = data)
          output$homogenity_table <- renderDT(signif(data.frame(df1 = res$Df[1], df2 = res$Df[2], F = res$`F value`, Sig = res$`Pr(>F)`), 4))
          output$homogenity_method_results <- renderUI(
            tagList(
              if (signif(res$`Pr(>F)`[1], 4) == 0) h4('O valor de p é ', strong('aproximadadente 0'), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                                                      '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
              else if (1 - ci < res$`Pr(>F)`[1]) h4('O valor de p = ', strong(signif(res$`Pr(>F)`[1], 4)), ', o que é ', strong('maior do que o nivel de significância', 1 - ci),
                                                    '. Assim sugere que ', strong('não há diferênças significantes'), ' entre as variâncias das variáveis')
              else h4('O valor de p = ', strong(signif(res$`Pr(>F)`[1], 4)), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                      '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
            )
          )
        }
        else if (choosen == 'fk_test') {
          res <- fligner.test(Dados ~ Classificação, data = data)
          output$homogenity_method_name <- renderUI(h3('Teste de Fligner-Killeen para comparação múltiplas variáveis'))
          output$homogenity_table <- renderDT(signif(data.frame(Chi_Quadrado = res$statistic, df = res$parameter, p = res$p.value), 4))
          output$homogenity_method_results <- renderUI(
            tagList(
              if (signif(res$p.value, 4) == 0) h4('O valor de p é ', strong('aproximadadente 0'), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                                                  '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
              else if (1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('maior do que o nivel de significância', 1 - ci),
                                                '. Assim sugere que ', strong('não há diferênças significantes'), ' entre as variâncias das variáveis')
              else h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('menor ou igual ao nivel de significância', 1 - ci),
                      '. Assim sugere que ', strong('há diferênças significantes'), ' entre, pelo menos 2 variâncias das variáveis')
            )
          )
        } }
    #-------------------Assessing Normality-------------------#
    {
    output$plotly_norm_density <- renderUI(plotlyOutput('plotly_norm_density2'))
    output$plotly_norm_qq <- renderUI(plotlyOutput('plotly_norm_qq2'))
    output$check_norm_table <- renderUI(DTOutput('check_norm_table2'))

    output$plotly_norm_qq2 <- renderPlotly(renderAssessingNormQQ(values, options))
    output$plotly_norm_density2 <- renderPlotly(renderAssessingNormDensity(values, options))
    output$check_norm_table2 <- renderDT(
    datatable(
      renderCheckNormTable(values, options),rownames = FALSE,
      container = withTags(table(
        class = 'display',
        thead(tr(
          th(colspan = 3, 'Shapiro-Wilk'),
          th(colspan = 3, 'Kolmogorov-Smirnov')
        ), tr(
          lapply(rep(c('Dados', 'p', 'Decisão'), 2), th)
        ))
      )),
      options = list(initComplete = JS(
        "function(settings, json) {",
        "var headerBorder = [0,1];",
        "var header = $(this.api().table().header()).find('tr:first > th').filter(function(index) {return $.inArray(index,headerBorder) > -1 ;}).addClass('cell-border-right');",
        "}"),columnDefs=list(list(className="dt-right cell-border-right",targets=2))
      ))
    )
  }
    #-------------------Transform to Normality-------------------#
    {
      output$transform_norm_results <- renderUI(tagList(
        h3('Sem nenhuma transformação'),
        plotlyOutput('transform_norm_results_original'),
        uiOutput('transform_norm_results_new'),
        uiOutput('transform_norm_results_method_statistics'),
        uiOutput('transform_norm_download')
      ))
      output$transform_norm_results_original <- renderPlotly(ggplotly(ggdensity(data = contingency_data(values$bidimensional_data), x = "Dados", color = 'Classificação', fill = 'Classificação', alpha = 0.7)))
      output$transform_norm_results_method_statistics <- renderUI(h4('O coeficiente de distorção é : ', signif(skewness(contingency_data(values$bidimensional_data)$Dados, na.rm = TRUE), 4)))
      observeEvent(input$load_transform_norm, {

      data <- contingency_data(values$bidimensional_data)
      if(length(table(is.na(data$Dados))) != 1 | names(table(is.na(data$Dados))) != 'FALSE')
        data <- data[-which(is.na(data$Dados)),]
      df <- data
        logy <- if(input$transform_norm_distributions_logy == 0) 1 else input$transform_norm_distributions_logy
      df$Dados <- switch(
        input$transform_norm_distributions,
        'none' = data$Dados,
        'sqrt' = if(input$transform_norm_distributions_skewed) sqrt(data$Dados) else sqrt(max(data$Dados + 1) - data$Dados),
        'log10' = if(input$transform_norm_distributions_skewed) log10(data$Dados) else log10(max(data$Dados + 1) - data$Dados),
        'logy' = if(input$transform_norm_distributions_skewed) data$Dados * logy else (max(data$Dados + 1) - data$Dados) * logy,
        '1/x' = if(input$transform_norm_distributions_skewed) 1/(data$Dados) else 1/(max(data$Dados + 1) - data$Dados)
      )

      if (input$transform_norm_distributions != 'none'){
         output$transform_norm_results_new <- renderUI(tagList(
            uiOutput('transform_norm_results_new_name'),
            plotlyOutput('transform_norm_results_new_plot'),
         ))
        fig <- ggplotly(
          ggdensity(data = df, x = "Dados", color = 'Classificação', fill = 'Classificação', alpha = 0.7)
          # + stat_overlay_normal_density(linetype = "dashed")
        )
        transformation <- input$transform_norm_distributions
        output$transform_norm_results_new_name <- renderUI(h3('Com a transformação: ',transformation))
        output$transform_norm_results_new_plot <- renderPlotly(fig)
        output$transform_norm_results_method_statistics <- renderUI(tagList(
           h4('O coeficiente de distorção é : ', signif(skewness(data$Dados, na.rm = TRUE), 4)),
           h4('O novo coeficiente de distorção é : ', signif(skewness(df$Dados, na.rm = TRUE), 4))
        ))

        # Download the new df
        dfDownload <- data
        output$transform_norm_download <- renderUI(downloadButton('transform_norm_download2','Baixe a nova tabela!'))
        output$transform_norm_download2 <- downloadHandler(
          filename = function() { "transformed_df.xlsx"},
          content = function(file) {write_xlsx(dfDownload, path = file)}
        )
      }
      else{
        output$transform_norm_results_method_statistics <- renderUI(tagList(h4('O coeficiente de distorção é : ', signif(skewness(data$Dados, na.rm = TRUE), 4))))
        output$transform_norm_results_new <- renderUI(p(''))
        output$transform_norm_download <- renderUI(p(''))
      }
    })
  }
    }
    else {
      output$homogenity_results <- renderUI(h3(frase_erro, align = 'center'))

      output$plotly_norm_density <- renderUI(h3(frase_erro, align = 'center'))
      output$plotly_norm_qq <- renderUI(h3(frase_erro, align = 'center'))
      output$check_norm_table <- renderUI(h3(frase_erro, align = 'center'))

      output$transform_norm_results <- renderUI(h3(frase_erro, align = 'center'))
    }
    #-------------------Assumption of Sphericity-------------------#
    if (ncol(values$bidimensional_data) == 3 & length(table(values$bidimensional_data[,ncol(values$bidimensional_data)])) == 3 & !is.numeric(values$bidimensional_data[,ncol(values$bidimensional_data)]))
      {
      output$sphericity_results <- renderUI(
        tagList(
          h3(strong('ANOVA')),
          DTOutput('sphericity_anova_test'),
          h3(strong('Teste de Esfericidade de Mauchly')),
          DTOutput('mauchly_test'),
          h3(strong('Correções de esfericidade')),
          fluidRow(
            column(6, h4('Correção Greenhouse-Geisser', align = 'center'), DTOutput('sphericity_corrections_gg')),
            column(6, h4('Correção Huynh-Feldt', align = 'center'), DTOutput('sphericity_corrections_hf'))
          ),
          h3(strong('Resultados:')),
          uiOutput('sphericity_statistics')
        )
      )
      # print()
      dt <- data.frame(values$bidimensional_data[,1], values$bidimensional_data[,ncol(values$bidimensional_data)])
      colnames(dt) <- c('Dados', 'Classificação')
      k <- lapply(names(table(dt$Classificação)), function (x) seq(length(which(dt$Classificação == x))))
      k2 <- NULL
      for (i in k)
        k2 <- append(k2, i)
      dt$id <- k2

      res <- anova_test(data = dt, dv = Dados,wid = id, within = Classificação)
      correction <- input$sphericity_correc_anova_2
      output$sphericity_anova_test <- renderDT(get_anova_table(res, correction = correction))

      mauchly <- res$`Mauchly's Test for Sphericity`[2:3]
      mauchly$Significância <- if(mauchly$p <= 1 - input$esfericity_ci) 'Significante' else 'Não Significante'
      output$mauchly_test <- renderDT(mauchly)

      output$sphericity_corrections_gg <- renderDT(res$`Sphericity Corrections`[2:5])
      output$sphericity_corrections_hf <- renderDT(res$`Sphericity Corrections`[6:9])
      output$sphericity_statistics <- renderUI(h4(
        'Pelo teste de esfericidade de mauchly, ', strong('p =', mauchly$p), '.', if(mauchly$p <= 1 - input$esfericity_ci)
           h4('As variâncias das diferenças entre os grupo ',strong('não são iguais'),' assim não podemos assumir a esfericidade.')
        else h4('As variâncias das diferenças entre os grupo',strong('são iguais'),' conforme o intervalo de confiança, assim podemos assumir
        a esfericidade.'), br()))
  }
    else
      output$sphericity_results <- renderUI(h3(frase_erro, align = 'center'))
  })

  #-------------------Comparando duas médias-------------------#
  observe(if(!is.null(values$bidimensional_data)){
      if (values$bidimensional_data_type == 'two_col'){
        #-------------------T Test-------------------#
        {
          if(input$test_t_options == 'one') {
            df <- values$bidimensional_data
            output$t_test_predict <- renderUI(tagList(
              h3(strong('Testando Normalidade', align = 'center')),
              column(6,
                     h4(names(df)[1]),
                     plotlyOutput('t_test_normality_1'),
                     uiOutput('t_test_normality_results_1'), align = 'center'
              ),
              column(6,
                     h4(names(df)[2]),
                     plotlyOutput('t_test_normality_2'),
                     uiOutput('t_test_normality_results_2'), align = 'center'
                ),
              column(12,
                     br(),
                     h3(strong('Verificando Outliers', align = 'center'))
              ),
              column(6,
                     plotlyOutput('t_test_boxplot_1'),
                     uiOutput('t_test_outliers_1'), align = 'center'
              ),
              column(6,
                     plotlyOutput('t_test_boxplot_2'),
                     uiOutput('t_test_outliers_2'), align = 'center'
              ),
              column(12,
                     h3(strong('Resultados', align = 'center')),
                     column(6,
                            h4(strong('Teste T - ',names(values$bidimensional_data)[1]), align = 'center'),
                            DTOutput('t_test_dt_1'),
                            uiOutput('t_test_effect_size1')
                     ),
                     column(6,
                            h4(strong('Teste T - ',names(values$bidimensional_data)[2]), align = 'center'),
                            DTOutput('t_test_dt_2'),
                            uiOutput('t_test_effect_size2')
                     )
              )
            ))
            output$t_test_normality_1 <- renderPlotly(ggplotly(ggqqplot(df[,1], color = '#F8766D')))
            output$t_test_normality_results_1 <- renderUI(p('O valor de p utilizando o teste de Shapiro Wilk é de: ', signif(shapiro.test(df[,1])$p.value, 4)))
            output$t_test_normality_2 <- renderPlotly(ggplotly(ggqqplot(df[,2], color = '#28B3B6')))
            output$t_test_normality_results_2 <- renderUI(p('O valor de p utilizando o teste de Shapiro Wilk é de: ', signif(shapiro.test(df[,2])$p.value, 4)))
            output$t_test_boxplot_1 <- renderPlotly(plot_ly(data.frame(), y = df[,1], type = 'box', boxpoints = "all", fillcolor = '#FEE4E2', name = names(df)[1], marker = list(color = '#F8766D', outliercolor = 'gray'), line = list(color = '#F8766D')))
            output$t_test_outliers_1 <- renderUI(if(nrow(identify_outliers(df[1])) == 0) p('Não exstem outliers') else p('Existem ',nrow(identify_outliers(df[1])), ' outliers.'))
            output$t_test_boxplot_2 <- renderPlotly(plot_ly(data.frame(), y = df[,2], type = 'box', boxpoints = "all", fillcolor = '#D4F0F0', name = names(df)[2], marker = list(color = '#28B3B6', outliercolor = 'gray'), line = list(color = '#28B3B6')))
            output$t_test_outliers_2 <- renderUI(if(nrow(identify_outliers(df[2])) == 0) p('Não exstem outliers') else p('Existem ',nrow(identify_outliers(df[2])), ' outliers.'))

            mu <- input$test_t_mu

            output$t_test_dt_1 <- renderDT(test_t_uni(df[1], mu))
            output$t_test_dt_2 <- renderDT(test_t_uni(df[2], mu))

            output$t_test_effect_size1 <- renderUI(p('A área de efeito da variável ', (strong(names(df)[1])), ', com mu = ', strong(mu), ' é de: ', strong(signif(abs(mean(df[,1]) - mu) / sd(df[,1]), 4))))
            output$t_test_effect_size2 <- renderUI(p('A área de efeito da variável ', (strong(names(df)[2])), ', com mu = ', strong(mu), ' é de: ', strong(signif(abs(mean(df[,2]) - mu) / sd(df[,2]), 4))))
          }
          if(input$test_t_options == 'two' | input$test_t_options == 'paired'){
            output$t_test_predict <- renderUI(tagList(
              column(6, h3(strong('Testando Normalidade', align = 'center'))),
              column(6, h3(strong('Verificando Outliers', align = 'center'))),

              column(12, plotlyOutput('t_test_plotly')),
              column(6, uiOutput('t_test_normality_results')),
              column(12, DTOutput('t_test_outliers')),

              column(12,
                     h3(strong('Teste de homocedasticidade')),
                     DTOutput('t_test_homostacity'),
                     uiOutput('t_test_homostacity_results'),
                     h3(strong('Resultados: ')),
                     DTOutput('t_test_dt'),
                     uiOutput('t_test_effect_size')
                ,align = 'center'
              )
            ))
            dt <- contingency_data(values$bidimensional_data)

            fig1 <- renderAssessingNormQQ(values)
            shap <- dt %>% group_by(Classificação) %>% shapiro_test(Dados) %>% as.data.frame()
            shap <- shap[-c(2,3)]

            output$t_test_normality_results <- renderUI(p('Os valores de p utilizando o teste de Shapiro Wilk de',
                                                          strong(shap[1,1],' é de: ', signif(shap[1,2], 4)), ifelse(signif(shap[1,2], 4) > 0.05, '(Estatísticamente normal)', '(Estatísticamente não normal)'), ' e ',
                                                          strong(shap[2,1],' é de: ', signif(shap[2,2], 4)), ifelse(signif(shap[2,2], 4) > 0.05, '(Estatísticamente Normal).', '(Estatísticamente não normal).'))
            )
            # output$t_test_boxplot <- renderPlotly(plot_ly(data = dt, y =~ Dados, x =~ Classificação, color =~ Classificação, type = 'box'))
            fig2 <- plot_ly(data = dt, y =~ Dados, x =~ Classificação, color =~ Classificação, type = 'box')
            outliers_dt <- dt %>% group_by(Classificação) %>% identify_outliers(Dados) %>% data.frame()
            output$t_test_outliers <- if(nrow(outliers_dt) != 0) renderDT(outliers_dt)

            output$t_test_plotly <- renderPlotly(subplot(fig1, fig2, margin = 0.1))
            ftest <- var.test(Dados ~ Classificação, dt)
            ftest_dt <- data.frame('Estimativa' = signif(ftest$estimate), 'p' = signif(ftest$p.value), 'Estatística' = signif(ftest$statistic))
            output$t_test_homostacity <- renderDT(ftest_dt)
            output$t_test_homostacity_results <- renderUI(p('O valor de p é: ',strong(signif(ftest_dt$p, 4)), 'ou seja,', ifelse(ftest_dt$p > 0.05, 'a variância entre os grupos é estatísticamente igual.', 'a variância entre os grupos é estatísticamente diferente.')))

            #Remove outliers
            # if(input$test_t_options != 'paired')
            #   dt <- removeOutliers(dt)

            test_w <- dt %>% t_test(Dados ~ Classificação, paired = input$test_t_options == 'paired', var.equal = ftest_dt$p > 0.05 | input$test_t_options == 'paired')
            # test_w <- dt %>% t_test(Dados ~ Classificação, paired = input$test_t_options == 'paired', var.equal = F)
            test_w_df <- data.frame(p = signif(test_w$p, 4), estatística = signif(test_w$statistic, 4), df = signif(test_w$df, 4))
            rownames(test_w_df) <- if(input$test_t_options == 'two') paste0('Teste T') else if(input$test_t_options == 'paired') paste0('Teste T - Pareado')
            output$t_test_dt <- renderDT(test_w_df)
            cohensD <- (dt %>% cohens_d(Dados ~ Classificação, paired = input$test_t_options == 'paired'))$effsize
            output$t_test_effect_size <-renderUI(tagList(p('O valor p do teste T é: ',strong(test_w_df$p) , ' ou seja, ', ifelse(test_w_df$p > 0.05, 'as variâncias de ambos os grupos são estatísticamente iguais', 'os dados médios de ambos os grupos são estatísticamente diferentes'),
                                                   br(),
                                                   'A área de efeito entre as variáveis ', strong(names(dt)[1]),', e ',strong(names(dt)[2]), ' é de: ', strong(signif(cohensD, 4)),br()),
                                                   if(ftest_dt$p <= 0.05 & input$test_t_options != 'paired') p('O algoritmo para calcular a área de efeito foi o ', strong('o algoritmo de Welch,'), ' como as variâncias foram diferentes.')
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
                 plotlyOutput('wilcoxon_test_symmetry_1'),align = 'center'
          ),
          column(6,
                 h4(names(df)[2]),
                 plotlyOutput('wilcoxon_test_symmetry_2'),align = 'center'
            ),
          column(12,
                 br(),
                 h3(strong('Verificando Outliers', align = 'center'))
          ),
          column(6,
                 plotlyOutput('wilcoxon_test_boxplot_1'),
                 uiOutput('wilcoxon_test_outliers_1'), align = 'center'
          ),
          column(6,
                 plotlyOutput('wilcoxon_test_boxplot_2'),
                 uiOutput('wilcoxon_test_outliers_2'), align = 'center'
          ),
          column(12,
                 h3(strong('Resultados', align = 'center')),
                 column(6,
                        h4(strong('Teste de Wilcoxon - ',names(values$bidimensional_data)[1]), align = 'center'),
                        DTOutput('wilcoxon_test_dt_1'),
                        uiOutput('wilcoxon_test_effect_size1')
                 ),
                 column(6,
                        h4(strong('Teste de Wilcoxon - ',names(values$bidimensional_data)[2]), align = 'center'),
                        DTOutput('wilcoxon_test_dt_2'),
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

        mu <- input$wilcoxon_t_mu

        w_test1 <- rstatix::wilcox_test(data = data.frame(data = df[[1]]),data ~ 1, mu = mu)
        w_test_df1 <- data.frame(p = signif(w_test1$p[[1]], 4), estatística = signif(w_test1$statistic[[1]], 4))
        rownames(w_test_df1) <- paste0('Test de Wilcoxon - ', names(df)[1])
        output$wilcoxon_test_dt_1 <- renderDT(w_test_df1)
        w_effectsize1 <- wilcox_effsize(data.frame(data = df[[1]]), data ~ 1, mu = mu)
        output$wilcoxon_test_effect_size1 <- renderUI(p('A área de efeito da variável ', (strong(names(df)[1])), ', com mu = ', mu, ' é de: ', strong(signif(w_effectsize1$effsize[[1]], 4))))

        w_test2 <- rstatix::wilcox_test(data = data.frame(data = df[[2]]),data ~ 1, mu = mu)
        w_test_df2 <- data.frame(p = signif(w_test2$p[[1]], 4), estatística = signif(w_test2$statistic[[1]], 4))
        rownames(w_test_df2) <- paste0('Test de Wilcoxon - ', names(df)[2])
        output$wilcoxon_test_dt_2 <- renderDT(w_test_df2)
        w_effectsize2 <- wilcox_effsize(data.frame(data = df[[2]]), data ~ 1, mu = mu)
        output$wilcoxon_test_effect_size2 <- renderUI(p('A área de efeito da variável ', (strong(names(df)[2])), ', com mu = ', mu, ' é de: ', strong(signif(w_effectsize2$effsize[[1]], 4))))
      }
      else if(input$wilcoxon_test_options == 'rank_sum' | input$wilcoxon_test_options == 'paired'){
        output$wilcoxon_test_predict <- renderUI(tagList(
          column(12, h3(strong('Verificando Outliers', align = 'center'))),
          column(12, plotlyOutput('wilcoxon_test_plotly')),
          column(12, DTOutput('wilcoxon_test_outliers')),

          column(12,
                 h3(strong('Resultados: ')),
                 DTOutput('wilcoxon_test_dt'),
                 uiOutput('wilcoxon_test_effect_size'),
                 align = 'center'
          )
        ))
        dt <- contingency_data(values$bidimensional_data)

        outliers_dt <- dt %>% group_by(Classificação) %>% identify_outliers(Dados) %>% data.frame()
        output$wilcoxon_test_outliers <- if(nrow(outliers_dt) != 0) renderDT(outliers_dt)

        output$wilcoxon_test_plotly <- renderPlotly(plot_ly(data = dt, y =~ Dados, x =~ Classificação, color =~ Classificação, type = 'box'))

        #Remove Outliers
        # if(input$wilcoxon_test_options != 'paired')
        #   dt <- removeOutliers(dt)

        test_w <- rstatix::wilcox_test(dt, Dados ~ Classificação, paired = input$wilcoxon_test_options == 'paired')
        test_w_df <- data.frame(p = signif(test_w$p, 4), estatística = signif(test_w$statistic[[1]], 4))
        rownames(test_w_df) <- if(input$wilcoxon_test_options == 'two') paste0('Teste de Wilcoxon') else if(input$wilcoxon_test_options == 'paired') paste0('Teste de Wilcoxon - Pareado')
        output$wilcoxon_test_dt <- renderDT(test_w_df)
        w_effectsize <- rstatix::wilcox_effsize(dt, Dados ~ Classificação, paired = input$wilcoxon_test_options == 'paired')$effsize[[1]]
        output$wilcoxon_test_effect_size <- renderUI(p('O valor p do teste de Wilxoxon é: ', test_w_df$p, ' ou seja, ', ifelse(test_w_df$p > 0.05, 'os dados médios de ambos os grupos são estatísticamente iguais', 'os dados médios de ambos os grupos são estatísticamente diferentes'),
                                                   br(),
          'A área de efeito entre as variáveis ', (strong(names(values$bidimensional_data)[1])),', e ',(strong(names(values$bidimensional_data)[2])),  ', é de: ', strong(signif(w_effectsize, 4))))
      }
    }
        #-------------------Sign Test-------------------#
        {
          output$sign_test_results <- renderUI(tagList(
            plotlyOutput('sign_test_outliers'),
            h3(strong('Estatísticas'), align = 'center'),
            DTOutput('sign_test_dt'),
            uiOutput('sign_test_p')
          ))
          dt <- values$bidimensional_data
          colnames(dt) <- c('var1', 'var2')
          dt <- contingency_data(dt)
          output$sign_test_outliers <- renderPlotly(plot_ly(data = dt, y =~ Dados, x =~ Classificação, color =~ Classificação, type = 'box'))

          sign_test <- dt %>% rstatix::sign_test(Dados ~ Classificação)
          sign_test_df <- data.frame(p = signif(sign_test$p[[1]], 4), estatística = signif(sign_test$statistic[[1]], 4), df = signif(sign_test$df[[1]], 4))
          rownames(sign_test_df) <- paste0('Test do Sinal')
          output$sign_test_dt <- renderDT(sign_test_df)
          output$sign_test_p <- renderUI(p('O valor p do teste do Sinal é é: ', sign_test_df$p, ' ou seja, ', ifelse(sign_test_df$p > 0.05, 'os dados médios de ambos os grupos são estatísticamente iguais', 'os dados médios de ambos os grupos são estatísticamente diferentes')),
          )
        }
      }
      else{
        output$t_test_predict <- renderUI(h3(frase_erro, align = 'center'))
        output$wilcoxon_test_predict <- renderUI(h3(frase_erro, align = 'center'))
        output$sign_test_results <- renderUI(h3(frase_erro, align = 'center'))
      }
    })

  #-------------------ANOVA's-------------------#
  observe(if (!is.null(values$bidimensional_data) ) {
     if(values$bidimensional_data_type == 'anova' | values$bidimensional_data_type == 'anova_2groups'){
       #-------------------ANOVA-------------------#
       if(values$bidimensional_data_type == 'anova')
       {
         output$anova_statistics <- renderUI(
           tagList(
           column(6,
                  h3(strong('Testando Normalidade', align = 'center')),
                  plotlyOutput('anova_qq_plot'),
                  uiOutput('anova_shapiro')

           ),
          column(6,
                 h3(strong('Verificando Outliers', align = 'center')),
                 plotlyOutput('anova_box_plot'),
                 DTOutput('anova_outliers')
          ),br(),
           column(12,
                  h3(strong('Verificando da Homogeneidade de Variância', align = 'center')),
                  DTOutput('anova_levene_dt'),
                  uiOutput('anova_levene_results'),br(),
                  h3(strong('Resultado do teste de ANOVA', align = 'center')),
                  DTOutput('anova_dt'),
                  uiOutput('anova_p'),
                  h3(strong('Tabela Post Hoc', align = 'center')),
                  DTOutput('anova_posthoc')
           ))
         )
       df <- values$bidimensional_data
       names <- names(df)
       names(df) <- c('vd', 'vi')
       model <- lm(df$vd ~ df$vi)

       #Normalidade
       output$anova_qq_plot <- renderPlotly(ggplotly(ggqqplot(residuals(model), color = "#E7B800")))
       shap <- signif(rstatix::shapiro_test(residuals(model))$p.value, 4)
       output$anova_shapiro <- renderUI(p('O valor p do teste de Shapiro-Wilk para estest dados são: ', shap, align = 'center'))

       #Outliers
       output$anova_box_plot <- renderPlotly(plot_ly(df, x = df[,2], y = df[,1], type = 'box', color = df[,2]))
       if (nrow(df %>% group_by(vi) %>% identify_outliers(vd)) > 0)
         output$anova_outliers <- renderDT(as.data.frame(df %>% group_by(vi) %>% identify_outliers(vd)))

       #Teste de Levene
       levene <- signif((df %>% rstatix::levene_test(vd ~ vi))$p, 4)
       output$anova_levene_dt <- renderDT(signif(data.frame(df %>% rstatix::levene_test(vd ~ vi)), 4))
       output$anova_levene_results <- renderUI(p('O valor de p do teste de Levene é: ', levene, align = 'center'))

       #ANOVA
       anova_dt <- df %>% anova_test(vd ~ vi)
       output$anova_dt <- renderDT(anova_dt[,4:7])
       output$anova_p <- renderUI(p('O valor de p do anova é: ', anova_dt$p, align = 'center'))
       posthoc <- df %>% tukey_hsd(vd ~ vi)
       posthoc$conf.high <- signif(posthoc$conf.high, 4)
       posthoc$conf.low <- signif(posthoc$conf.low, 4)
       posthoc$estimate <- signif(posthoc$estimate, 4)
       posthoc$p.adj <- signif(posthoc$p.adj, 4)
       output$anova_posthoc <- renderDT(posthoc[-c(1, 9)])
     }
       else
         output$anova_statistics <- renderUI(h3(frase_erro))

       if(values$bidimensional_data_type == 'anova_2groups'){
         #-------------------ANOVA - Repeted Measures-------------------#
          {
       output$anova_rep_statistics <- renderUI(tagList(
         column(6,
                  h3(strong('Testando Normalidade', align = 'center')),
                  plotlyOutput('anova_rep_qq_plot'),
                  uiOutput('anova_rep_shapiro')

           ),
          column(6,
                 h3(strong('Verificando Outliers', align = 'center')),
                 plotlyOutput('anova_rep_box_plot'),
                 DTOutput('anova_rep_outliers')
          ),br(),
           column(12,
                  h3(strong('', align = 'center')),
                  h3(strong('Esfericidade de Mauchly', align = 'center')),
                  DTOutput('anova_rep_mauchly_dt'),
                  uiOutput('anova_rep_mauchly_results'),br(),
                  h3(strong('Resultado do teste de ANOVA', align = 'center')),
                  DTOutput('anova_rep_dt'),
                  uiOutput('anova_rep_p'),
                  h3(strong('Tabela Post Hoc', align = 'center')),
                  DTOutput('anova_rep_posthoc')
           )))
       df <- values$bidimensional_data
       names <- names(df)
       names(df) <- c('vd', 'vi', 'wid')
       model <- lm(df$vd ~ df$vi)

       #Normalidade
       output$anova_rep_qq_plot <- renderPlotly(ggplotly(ggqqplot(residuals(model), color = "#E7B800")))
       shap <- signif(rstatix::shapiro_test(residuals(model))$p.value, 4)
       output$anova_rep_shapiro <- renderUI(p('O valor p do teste de Shapiro-Wilk para estest dados são: ', shap, align = 'center'))

       #Outliers
       output$anova_rep_box_plot <- renderPlotly(plot_ly(df, x = df[,2], y = df[,1], type = 'box', color = df[,2]))
       if (nrow(df %>% group_by(vi) %>% identify_outliers(vd)) > 0)
         output$anova_rep_outliers <- renderDT(as.data.frame(df %>% group_by(vi) %>% identify_outliers(vd)))

       #Teste de Esfericidade de Mauchly
       anova_dt <- df %>% anova_test(dv = vd, within = vi, wid = wid)
       mauchly <- anova_dt$`Mauchly's Test for Sphericity`
       output$anova_rep_mauchly_dt <- renderDT(mauchly[-4])
       output$anova_rep_mauchly_results <- renderUI(p('O valor de p do teste de mauchly é: ', mauchly$p, align = 'center'))

       #ANOVA
       output$anova_rep_dt <- renderDT(get_anova_table(anova_dt))
       output$anova_rep_p <- renderUI(p('O valor de p do anova é: ', anova_dt$p, align = 'center'))
       posthoc <- data.frame(df %>% pairwise_t_test(vd ~ vi, paired = TRUE, p.adjust.method = "bonferroni"))
       output$anova_rep_posthoc <- renderDT(posthoc[-c(1, 3, 4, 10)])
     }
         #-------------------ANOVA - Mixed Measures-------------------#
          {
       output$anova_mix_statistics <- renderUI(tagList(
         column(12,
         column(6,
                  h3(strong('Testando Normalidade', align = 'center')),
                  plotlyOutput('anova_mix_qq_plot'),
                  uiOutput('anova_mix_shapiro')

           ),
          column(6,
                 h3(strong('Verificando Outliers', align = 'center')),
                 plotlyOutput('anova_mix_box_plot'),
                 DTOutput('anova_mix_outliers')
          ))
         ,br(),
           column(12,
                  h3(strong('Esfericidade de Mauchly', align = 'center')),
                  DTOutput('anova_mix_mauchly_dt'),
                  uiOutput('anova_mix_mauchly_results'),br(),

                  h3(strong('Verificando Homogeniedade de Covariância', align = 'center')),
                  DTOutput('anova_mix_boxm'),

                  h3(strong('Resultado do teste de ANOVA', align = 'center')),
                  DTOutput('anova_mix_dt'),
                  uiOutput('anova_mix_statistics'),
                  h3(strong('Tabela Post Hoc', align = 'center')),
                  DTOutput('anova_mix_posthoc')
           )))
       df <- values$bidimensional_data
       names <- names(df)
       names(df) <- c('vd', 'vi', 'wid')
       model <- lm(df$vd ~ df$vi)

       #Normalidade
       output$anova_mix_qq_plot <- renderPlotly(ggplotly(ggqqplot(residuals(model), color = "#E7B800")))
       shap <- signif(rstatix::shapiro_test(residuals(model))$p.value, 4)
       output$anova_mix_shapiro <- renderUI(p('O valor p do teste de Shapiro-Wilk para estest dados são: ', shap, align = 'center'))

       #Outliers
       output$anova_mix_box_plot <- renderPlotly(plot_ly(df, x = df[,2], y = df[,1], type = 'box', color = df[,2]))
       if (nrow(df %>% group_by(vi) %>% identify_outliers(vd)) > 0)
         output$anova_mix_outliers <- renderDT(as.data.frame(df %>% group_by(vi) %>% identify_outliers(vd)))

       #Teste de Esfericidade de Mauchly
       anova_dt <- df %>% anova_test(dv = vd, within = vi, wid = wid)
       mauchly <- anova_dt$`Mauchly's Test for Sphericity`
       output$anova_mix_mauchly_dt <- renderDT(mauchly[-4])
       output$anova_mix_mauchly_results <- renderUI(p('O valor de p do teste de mauchly é: ', mauchly$p, align = 'center'))

        #Teste de Homogeniedade das Covariâncias
        box_m <- box_m(df[, "vd", drop = FALSE], df$vi)
        output$anova_mix_boxm <- renderDT(box_m)

       #ANOVA
       output$anova_mix_dt <- renderDT(get_anova_table(anova_dt))
       output$anova_mix_p <- renderUI(p('O valor de p do anova é: ', anova_dt$p, align = 'center'))
       posthoc <- data.frame(df %>% pairwise_t_test(vd ~ vi, paired = TRUE, p.adjust.method = "bonferroni"))
       output$anova_rep_posthoc <- renderDT(posthoc[-c(1:5, 10)])
     }
       }
       else{
         output$anova_rep_statistics <- renderUI(h3(frase_erro))
         output$anova_mix_statistics <- renderUI(h3(frase_erro))
       }
     }
    else{
       output$anova_statistics <- renderUI(h3(frase_erro))
       output$anova_rep_statistics <- renderUI(h3(frase_erro))
       output$anova_mix_statistics <- renderUI(h3(frase_erro))
     }
  })

  #-------------------ANCOVA-------------------#
  observe({ if(!is.null(values$bidimensional_data)){
    if(values$bidimensional_data_type == 'ancova'){
      output$ancova_statistics <- renderUI(tagList(
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

      ))

      df <- values$bidimensional_data
      names(df) <- c('vd', 'cov', 'vi')
      # output$ancova_linearity <- renderPlotly(renderANCOVA(values, options))
      output$ancova_linearity <- renderPlot(
        ggscatter(df, x = "cov", y = "vd", color = "vi", add = "reg.line")+
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = vi))
      )
      regression <- as.data.frame(anova_test(df, vd ~ vi*cov))
      output$ancova_test <- renderDT(regression)
      output$ancova_regression_results <- renderUI(p('O valor de F(',regression$DFn[3], ', ',regression$DFd[3],') = ', regression$F[3], ' e o valor de P é: ', regression$p[3]))

      #Teste de levene
      levene <- ancova_levene_test(df)
      output$ancova_levene_test <- renderDT(levene)
      #Teste de shapiro wilk
      shapiro <- ancova_shapiro_test(df)
      output$ancova_shapiro_test <- renderDT(shapiro)

      #Resultados/interpretações
      output$ancova_results <- renderUI(tagList(
        if(as.double(shapiro$p) > options$ancova_ci) h4('O teste de Shapiro-Wilk não foi significante (p > ',options$ancova_ci,'), assim podemos
        assumir a normalidade dos residuos')
        else h4('O teste de Shapiro-Wilk foi significante (p <= ',options$ancova_ci,'), não podemos
        assumir a normalidade dos resíduos.'),

        if(as.double(levene$p) > options$ancova_ci) h4('O teste de Levene não foi significante (p > ',options$ancova_ci,'), assim podemos
        assumir a igualdade da variância dos resíduos para todos os grupos.')
        else h4('O teste de Levene foi significante (p <= ',options$ancova_ci,'), não podemos
        assumir a igualdade da variância dos resíduos.')
      ))
      #Posthoc
      output$ancova_posthoc <- renderDT(posthoc_ancova_table(df))
    }
      else
      output$ancova_statistics <- renderUI(tagList(br(),br(),h3(frase_erro, align = 'center')))
    }
    else
      output$ancova_statistics <- renderUI(tagList(br(),br(),h3(frase_erro, align = 'center')))
  })

  #-------------------MANOVA-------------------#
  observe({if(!is.null(values$bidimensional_data)) {
    if (values$bidimensional_data_type == 'manova'){
        output$manova_statistics <- renderUI(
               tagList(
                 column(12,
                        h3(strong('Dados')),
                        plotlyOutput('manova_boxplot'),
                        h3(strong('Detectando Outliers Multivariados')),
                        DTOutput('manova_outliers_multi'),
                        h3(strong('Checando Normalidade Multivariada')),
                        DTOutput('manova_normality_multi'),
                        align = 'center'
                 ),
                 br(),
                 column(6,
                        h3(strong('Checando Tamanho da Amostra')),
                        h3(strong('Checando Homogeniedade das Covariâncias')),
                        DTOutput('manova_covariancia'),
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
                      DTOutput('manova_dt'),
                      uiOutput('manova_res'),
                      h3(strong('Tabela Post Hoc')),
                      DTOutput('manova_posthoc'),
                      align = 'center'
               ))
        )
      df <- values$bidimensional_data
      df_ncol <- ncol(df)
      df <- df %>% df_select(vars = names(df)[seq_len(ncol(df))]) %>% add_column(id = seq_len(nrow(df)), .before = 1)
      output$manova_boxplot <- renderPlotly(ggboxplot(df, x = names(df)[df_ncol], y = names(df)[2:(df_ncol - 1)], merge = TRUE, palette = "jco") %>% ggplotly())
      output$manova_outliers_multi <- df %>% group_by(var = names(df)[df_ncol]) %>% mahalanobis_distance(-id) %>% filter(is.outlier == TRUE) %>% as.data.frame() %>% renderDT()
      output$manova_normality_multi <- df %>% df_select(vars = names(df)[2:(df_ncol - 1)]) %>% mshapiro_test() %>% as.data.frame() %>% renderDT()
      dt_multicollinearity <- df %>% cor_test(vars = names(df)[2:(df_ncol - 1)]) %>% as.data.frame()
      dt_multicollinearity <- dt_multicollinearity[-c(3, 6, 7, 8)]
      dt_multicollinearity[3:4] <- signif(dt_multicollinearity[3:4], 4)
      output$manova_multicollinearity <- dt_multicollinearity %>% renderDT()
      dt_cov <- box_m(df[, names(df)[2:(df_ncol - 1)]], df[,df_ncol]) %>% as.data.frame()
      dt_cov2 <- data.frame(`Estatística` = dt_cov$statistic, p = dt_cov$p.value, df = dt_cov$parameter, `Método` = 'Box\'s M-test')
      dt_cov2[c(1, 2)] <- signif(dt_cov2[c(1, 2)], 4)
      output$manova_covariancia <- dt_cov2 %>% renderDT()
      output$manova_variance <- df %>% gather(key = "variable", value = "value", names(df)[2:(df_ncol - 1)]) %>% group_by(variable) %>% levene_test(value ~ df[,df_ncol]) %>% as.data.frame() %>% renderDT()
      # model <- lm(cbind(Sepal.Length, Petal.Length) ~ Species, df)
      # output$manova_dt <- Manova(model, test.statistic = "Pillai") %>% as.data.frame() %>% renderDT()


      output$manova_unidimensional_assumptions <- renderUI(tagList(
        column(12,
               h3(strong('Checando Normalidade')),
               plotlyOutput('manova_normality_uni'),
               DTOutput('manova_shapiro_uni'),
               h3(strong('Checando Linearidade')),
               plotOutput('manova_linearity_plot1'),
               plotOutput('manova_linearity_plot2'),
               plotOutput('manova_linearity_plot3'),
               align = 'center'
        )
      ))
      #Testes de Normalidade
      fig1 <- ggplotly(ggqqplot(df, names(df)[2], color = names(df)[df_ncol], ggtheme = theme_bw()))
      fig2 <- ggplotly(ggqqplot(df, names(df)[3], color = names(df)[df_ncol], ggtheme = theme_bw()))
      output$manova_normality_uni <- renderPlotly(subplot(fig1, fig2, margin = 0.1))
      #Shapiro Wilk
      dt_shapiro_uni <- df %>% group_by(var = names(df)[df_ncol]) %>% shapiro_test(vars = names(df)[2:(df_ncol - 1)]) %>% arrange(variable) %>% as.data.frame()
      dt_shapiro_uni[3:4] <- signif(dt_shapiro_uni[3:4], 4)
      output$manova_shapiro_uni <- dt_shapiro_uni %>% renderDT()

      #Testes de Linearidade
      results <- df %>% df_select(vars = names(df)[2:(df_ncol - 1)]) %>% group_by(var = names(df)[df_ncol]) %>% doo(~ggpairs(.) + theme_bw(), result = "plots")
      output$manova_linearity_plot1 <- renderPlot(results$plots[[1]])
      output$manova_linearity_plot2 <- renderPlot(results$plots[[2]])
      output$manova_linearity_plot3 <- renderPlot(results$plots[[3]])

      }
    else
        output$manova_statistics <- renderUI(tagList(br(),br(),h3(frase_erro, align = 'center')))
  }
  else
    output$manova_statistics <- renderUI(tagList(br(),br(),h3(frase_erro, align = 'center')))
  })

  #-------------------ANOVA testes não parametricos-------------------#
  observe({if(!is.null(values$bidimensional_data)){
    #-------------------Kruskal-Wallis-------------------#
    if(values$bidimensional_data_type == 'anova'){
      output$kruskal_test_statistics <- renderUI(tagList(
        column(
          12,
          h3(strong('Detectando Outliers')),
          plotlyOutput('kruskal_boxplot'),
          br(),
          h3(strong('Calculo do Teste de Kruskal Wallis')),
          DTOutput('kruskal_dt'),
          uiOutput('kruskal_interpretation'),
          h3(strong('Área de Efeito')),
          DTOutput('kruskal_effectArea'),
          uiOutput('kruskal_effectArea_interpretation'),
          br(),
          h3(strong('Múltiplas comparações entre pares')),
          column(6,
                 h3(strong('Teste de Dunn')),
                 DTOutput('kruskal_dunn_test')
          ),
          column(6,
                 h3(strong('Teste de Wilcoxon')),
                 DTOutput('kruskal_wilcoxon_test')
          )
          , align = 'center'
        )
      ))
      df <- values$bidimensional_data
      #Boxplot
      output$kruskal_boxplot <- renderPlotly(plot_ly(df, y = df[[1]], color = df[[2]], type = 'box'))
      #Cálculo do Teste
      dfkruskal_dt <- df %>% rstatix::kruskal_test(df[[1]] ~ df[[2]])
      dfkruskal_dt <- dfkruskal_dt[3:6]
      dfkruskal_dt[1] <- signif(dfkruskal_dt[1], 4)
      dfkruskal_dt[3] <- signif(dfkruskal_dt[3], 4)
      output$kruskal_dt <- renderDT(dfkruskal_dt)

      #Area de Efeito
      dfkruskal_effectArea <- df %>% rstatix::kruskal_effsize(df[[1]] ~ df[[2]])
      dfkruskal_effectArea <- dfkruskal_effectArea[3:5]
      dfkruskal_effectArea[1] <- signif(dfkruskal_effectArea[1], 4)
      output$kruskal_effectArea <- renderDT(dfkruskal_effectArea)
      names(df) <- c('Dados', 'Grupos')

      #Dumm's test
      df_dumm_test <- df %>% rstatix::dunn_test(Dados ~ Grupos, p.adjust.method = "bonferroni")
      df_dumm_test <- df_dumm_test[c(2, 3, 6, 7)]
      df_dumm_test[3] <- signif(df_dumm_test[3], 4)
      df_dumm_test[4] <- signif(df_dumm_test[4], 4)
      output$kruskal_dunn_test <- renderDT(df_dumm_test)

      #Wilcoxon's test
      df_wilcoxon_test <- df %>% rstatix::wilcox_test(Dados ~ Grupos, p.adjust.method = "bonferroni")
      df_wilcoxon_test <- df_wilcoxon_test[c(2, 3, 6, 7)]
      df_wilcoxon_test[3] <- signif(df_wilcoxon_test[3], 4)
      df_wilcoxon_test[4] <- signif(df_wilcoxon_test[4], 4)
      output$kruskal_wilcoxon_test <- renderDT(df_wilcoxon_test)
    }
    else
      output$kruskal_test_statistics <- renderUI(tagList(br(),br(),h3(frase_erro, align = 'center')))

    #-------------------Friedman Test-------------------#
    if(values$bidimensional_data_type == 'anova_2groups') {
      output$friedman_test_statistics <- renderUI(tagList(
        column(
          12,
          h3(strong('Detectando Outliers')),
          plotlyOutput('friedman_boxplot'),
          br(),
          h3(strong('Calculo do Teste de friedman Wallis')),
          DTOutput('friedman_dt'),
          uiOutput('friedman_interpretation'),
          h3(strong('Área de Efeito')),
          DTOutput('friedman_effectArea'),
          uiOutput('friedman_effectArea_interpretation'),
          br(),
          h3(strong('Múltiplas comparações entre pares')),
          column(6,
                 h3(strong('Teste de Wilcoxon')),
                 DTOutput('friedman_wilcoxon_test')
          ),
          column(6,
                 h3(strong('Teste do Sinal')),
                 DTOutput('friedman_sign_test')
          )
          , align = 'center'
        )
      ))
      df <- values$bidimensional_data
      #Boxplot
      output$friedman_boxplot <- renderPlotly(plot_ly(df, y = df[[1]], color = df[[2]], type = 'box'))
      #Cálculo do Teste
      names(df) <- c('Dados', 'Grupo', 'id')
      df_friedman_dt <- (df %>% rstatix::friedman_test(Dados ~ Grupo | id) %>% data.frame())[3:6]
      df_friedman_dt[3] <- signif(df_friedman_dt[3], 4)
      output$friedman_dt <- renderDT(df_friedman_dt)

      #Area de Efeito
      df_friedman_effectArea <- (df %>% rstatix::friedman_effsize(Dados ~ Grupo | id) %>% data.frame())[-(1:2)]
      df_friedman_effectArea[1] <- signif(df_friedman_effectArea[1], 4)
      output$friedman_effectArea <- renderDT(df_friedman_effectArea)

      #Sign's test
      df_friedman_sign_test <- df %>% rstatix::sign_test(Dados ~ Grupo, p.adjust.method = "bonferroni")
      df_friedman_sign_test <- df_friedman_sign_test[c(2, 3, 6, 7, 8)]
      df_friedman_sign_test[3] <- signif(df_friedman_sign_test[3], 5)
      df_friedman_sign_test[5] <- signif(df_friedman_sign_test[5], 5)
      output$friedman_sign_test <- renderDT(df_friedman_sign_test)

      #Wilcoxon's test
      df_friedman_wilcoxon_test <- df %>% rstatix::wilcox_test(Dados ~ Grupo, p.adjust.method = "bonferroni", paired = TRUE)
      df_friedman_wilcoxon_test <- df_friedman_wilcoxon_test[c(2, 3, 6, 7)]
      df_friedman_wilcoxon_test[3] <- signif(df_friedman_wilcoxon_test[3], 4)
      df_friedman_wilcoxon_test[4] <- signif(df_friedman_wilcoxon_test[4], 4)
      output$friedman_wilcoxon_test <- renderDT(df_friedman_wilcoxon_test)
    }
    else
      output$friedman_test_statistics <- renderUI(tagList(br(),br(),h3(frase_erro, align = 'center')))
  }
    else{
    output$kruskal_test_statistics <- renderUI(tagList(br(),br(),h3(frase_erro, align = 'center')))
    output$friedman_test_statistics <- renderUI(tagList(br(),br(),h3(frase_erro, align = 'center')))
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
    options$ancova_ci <- input$ancova_ci
    options$ancova_sumsq <- input$ancova_sumsq
  }

    #Homogenity
    # options$homogenity_ci <- input$homogenity_ci

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