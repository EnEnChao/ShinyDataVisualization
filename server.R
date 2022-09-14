source('RFunctions/table.R')
source('RFunctions/PlotFunct.R')

server <- function (input, output, session){

  #----------- VARIÁVEIS -----------
  values <- reactiveValues()
  options <- reactiveValues()

  values$usr_title <- NULL
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

  #Iniciar as planilhas
  output$user_data <- renderRHandsontable({ rhandsontable(data = data.frame(matrix('', 1000, 1000))) })
  output$user_data_bi <- renderRHandsontable({ rhandsontable(data = data.frame(matrix('', 1000, 1000))) })

  #Esconder todos os paineis
  hideTab(inputId = "tabs", target = "Gráficos 2D")
  hideTab(inputId = "tabs", target = "Gráficos 3D")
  hideTab(inputId = "tabs", target = "Checando os dados")
  hideTab(inputId = "tabs", target = "Informações gerais")

  hideTab(inputId = "tabs", target = "Transformar seus dados")
  hideTab(inputId = "tabs", target = "Comparando duas médias")
  hideTab(inputId = "tabs", target = "Comparando multiplas médias")

  hideTab(inputId = "tabs", target = 'Gráfico em Mesh')

  #----------- BOTÕES DE CARREGAMENTO -----------

  observeEvent(input$tutorial_button,{
      showModal(modalDialog(
        title = "Artigo",
        tags$iframe(style="height:600px; width:100%", src="Shiny Data Visualization - Tutorial.pdf"),
        easyClose = TRUE,
        footer = NULL
      ))
    })

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

  observeEvent(input$load_bidimensional,{
    output$plotly_ancova <- renderUI(tagList(br(),br(),h3(strong('Escolha as variáveis na aba de opções.'), align = 'center')))
    output$ancova_statistics <- renderUI(p(''))
    showTab(inputId = "tabs", target = "Comparando duas médias")
    showTab(inputId = "tabs", target = "Checando os dados")
    showTab(inputId = "tabs", target = "Comparando multiplas médias")

    if (input$file_selector_bi == 'example'){
      if(input$examp_select_bi == 'gas')
      dt <- data.frame(read.xlsx('Data/exemplo1ANCOVA.xlsx'))
      if(input$examp_select_bi == 'anxiety') {
        data("anxiety", package = "datarium")
        dt <- data.frame(anxiety)
        names(dt) <- c('Id', 'Grupo', 'T1', 'T2', 'T3')
      }
      if(input$examp_select_bi == 'escolaridade'){
        dt <- data.frame(read.xlsx('Data/Escolaridade.xlsx'))
      }
    }
    else if(input$file_selector_bi == 'import') {
      dt <- data.frame(read.xlsx(input$file_imported_bi$datapath))
      if(!input$imported_bi_as.factor)
        dt <- consolidated_data(dt)
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

    values$usr_title <- paste0(input$title_id_import_bi)
      output$title_name_import_bi <- renderUI(
         h2(strong(values$usr_title))
      )
    values$bidimensional_data <- dt
  })

  observeEvent(input$load_spreadsheet_bi,{
    output$plotly_ancova <- renderUI(tagList(br(),br(),h3(strong('Escolha as variáveis na aba de opções.'), align = 'center')))
    output$ancova_statistics <- renderUI(p(''))
    showTab(inputId = "tabs", target = "Comparando duas médias")
    showTab(inputId = "tabs", target = "Checando os dados")
    showTab(inputId = "tabs", target = "Comparando multiplas médias")

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
      if(!input$inserted_bi_as.factor)
        dt <- consolidated_data(dt)
    } else output$rest_of_sidebar <- renderMenu(NULL)

    values$usr_title <- paste0(input$title_id_insert_bi)
    output$title_name_insert_bi <- renderUI(
      h2(strong(values$usr_title)))

    values$bidimensional_data <- dt
  })

    observeEvent(input$load_homogenity,{

   output$homogenity_results <- renderUI(tagList(
      uiOutput('homogenity_method_name'),
      DTOutput('homogenity_table'),
      uiOutput('homogenity_method_results')
    ))

    choosen <- input$homogenity_tests
    ci <- input$homogenity_ci
    data <- data.frame(values$bidimensional_data[input$homogenity_var_vi], values$bidimensional_data[input$homogenity_var_vd])
    colnames(data) <- c('Classificação', 'Dados')

    if(choosen == 'f_test'){
      first <- input$first_var_f_test
      sec <- input$sec_var_f_test

      if(first == sec)
        output$homogenity_results <- renderUI(h4('Erro: Foi selecionada variáveis repetidas'))
      else{
        output$homogenity_method_name <- renderUI(h3('Teste F para comparação de duas variáveis'))

        data <- data.frame(data[which(data$`Classificação` == first | data$`Classificação` == sec),]$Dados, data[which(data$Classificação == first | data$Classificação == sec),]$`Classificação`)
        names(data) <- c('Dados', 'Classificacao')

        res <- var.test(Dados ~ Classificacao, data = data, conf.level = ci)
        dt <- signif(data.frame(F = res$statistic,Num_df = res$parameter[1], Denom_df = res$parameter[2] ,p = res$p.value), 4)

        output$homogenity_table <- renderDT(dt)
        output$homogenity_method_results <- renderUI(
          tagList(
            h4('Com um intervalo de confiança de ', ci*100,'%:'),
            h4(signif(res$conf.int[1], 4), signif(res$conf.int[2], 4)), br(),
            if(signif(res$p.value, 4) == 0) h4('O valor de p é ',strong('aproximadadente 0'),', o que é ', strong('menor ou igual ao nivel de significância',1 - ci),
               '. Assim sugere que ',strong('há diferênças significantes'),' entre as duas variâncias')
            else if(1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ',strong('maior do que o nivel de significância', 1 - ci),
               '. Assim sugere que ',strong('não há diferênças significantes'),' entre as duas variâncias')
            else h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('menor ou igual ao nivel de significância',1 - ci),
               '. Assim sugere que ',strong('há diferênças significantes'),' entre as duas variâncias')
          )
        )
      }
    }
    else if(choosen == 'bartlett_test'){
      res <- bartlett.test(Dados ~ Classificação, data = data)
      output$homogenity_method_name <- renderUI(h3('Teste de Bartlett para comparação múltiplas variáveis'))
      output$homogenity_table <- renderDT(signif(data.frame(F = res$statistic, df = res$parameter, p = res$p.value)), 4)
      output$homogenity_method_results <- renderUI(
        tagList(
           if(signif(res$p.value, 4) == 0) h4('O valor de p é ',strong('aproximadadente 0'),', o que é ', strong('menor ou igual ao nivel de significância',1 - ci),
                                             '. Assim sugere que ',strong('há diferênças significantes'),' entre, pelo menos 2 variâncias das variáveis')
           else if(1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ',strong('maior do que o nivel de significância',1 - ci),
                                       '. Assim sugere que ',strong('não há diferênças significantes'),' entre as variâncias das variáveis')
           else h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('menor ou igual ao nivel de significância',1 - ci),
                   '. Assim sugere que ',strong('há diferênças significantes'),' entre, pelo menos 2 variâncias das variáveis')
        )
      )

    }
    else if(choosen == 'levene_test'){
      output$homogenity_method_name <- renderUI(h3('Teste de Levene para comparação múltiplas variáveis'))
      res <- leveneTest(Dados ~ Classificação, data = data)
      output$homogenity_table <- renderDT(signif(data.frame(df1 = res$Df[1], df2 = res$Df[2], F = res$`F value`, Sig = res$`Pr(>F)`), 4))
      output$homogenity_method_results <- renderUI(
        tagList(
           if(signif(res$`Pr(>F)`[1], 4) == 0) h4('O valor de p é ',strong('aproximadadente 0'),', o que é ', strong('menor ou igual ao nivel de significância',1 - ci),
                                             '. Assim sugere que ',strong('há diferênças significantes'),' entre, pelo menos 2 variâncias das variáveis')
           else if(1 - ci < res$`Pr(>F)`[1]) h4('O valor de p = ', strong(signif(res$`Pr(>F)`[1], 4)), ', o que é ',strong('maior do que o nivel de significância',1 - ci),
                                       '. Assim sugere que ',strong('não há diferênças significantes'),' entre as variâncias das variáveis')
           else h4('O valor de p = ', strong(signif(res$`Pr(>F)`[1], 4)), ', o que é ', strong('menor ou igual ao nivel de significância',1 - ci),
                   '. Assim sugere que ',strong('há diferênças significantes'),' entre, pelo menos 2 variâncias das variáveis')
        )
      )
    }
    else if(choosen == 'fk_test'){
      res <- fligner.test(Dados ~ Classificação, data = data)
      output$homogenity_method_name <- renderUI(h3('Teste de Fligner-Killeen para comparação múltiplas variáveis'))
      output$homogenity_table <- renderDT(signif(data.frame(Chi_Quadrado = res$statistic, df = res$parameter, p = res$p.value), 4))
      output$homogenity_method_results <- renderUI(
        tagList(
           if(signif(res$p.value, 4) == 0) h4('O valor de p é ',strong('aproximadadente 0'),', o que é ', strong('menor ou igual ao nivel de significância',1 - ci),
                                             '. Assim sugere que ',strong('há diferênças significantes'),' entre, pelo menos 2 variâncias das variáveis')
           else if(1 - ci < res$p.value) h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ',strong('maior do que o nivel de significância',1 - ci),
                                       '. Assim sugere que ',strong('não há diferênças significantes'),' entre as variâncias das variáveis')
           else h4('O valor de p = ', strong(signif(res$p.value, 4)), ', o que é ', strong('menor ou igual ao nivel de significância',1 - ci),
                   '. Assim sugere que ',strong('há diferênças significantes'),' entre, pelo menos 2 variâncias das variáveis')
        )
      )
    }
  })
  observeEvent(input$load_assessing_norm,{
    options$assessing_norm_vi <- input$assessing_norm_vi
    options$assessing_norm_vd <- input$assessing_norm_vd
    output$plotly_norm_density <- renderPlotly(renderAssessingNormDensity(values, options))
    output$plotly_norm_qq <- renderPlotly(renderAssessingNormQQ(values, options))

    output$check_norm_table_names <- renderDT(data.frame(Dados = names(table(values$bidimensional_data[input$assessing_norm_vi])) ))
    output$check_norm_table <- renderDT(
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
  })
  observeEvent(input$load_sphericity, {

    output$sphericity_results <- renderUI(
      tagList(
        conditionalPanel(condition = 'input.sphericity_picker == ""',
                         h3('Escolha uma ou mais tabelas para serem mostradas no controle de opções a direita')
        ),
        conditionalPanel(condition = 'input.sphericity_picker.includes("anova")',
                         h3(strong('ANOVA')),
                         DTOutput('sphericity_anova_test')
        ),
        conditionalPanel(condition = 'input.sphericity_picker.includes("mauchly")',
                         h3(strong('Teste de Esfericidade de Mauchly')),
                         DTOutput('maunchly_test')
        ),
        conditionalPanel(condition = 'input.sphericity_picker.includes("corrections")',
                         h3(strong('Correções de esfericidade')),
                         fluidRow(
                           column(6,
                                  h4('Correção Greenhouse-Geisser', align = 'center'),
                                  DTOutput('sphericity_corrections_gg')
                           ),
                           column(6,
                                  h4('Correção Huynh-Feldt', align = 'center'),
                                  DTOutput('sphericity_corrections_hf')
                           )
                         )
        ),
        conditionalPanel(condition = 'input.sphericity_picker.includes("results")',
                         h3(strong('Resultados:')),
                         uiOutput('sphericity_statistics')
        )
      )
    )
    dt <- data.frame(values$bidimensional_data[input$sphericity_vi], values$bidimensional_data[input$sphericity_vd])
    colnames(dt) <- c('Classificação', 'Dados')

    k <- lapply(names(table(dt$Classificação)), function (x) seq(length(which(dt$Classificação == x))))
    k2 <- NULL
    for (i in k)
      k2 <- append(k2, i)
    dt$id <- k2


    res <- anova_test(data = dt, dv = Dados,wid = id, within = Classificação)
    if('anova' %in% input$sphericity_picker){
        output$sphericity_correc_anova <- renderUI(selectInput(
          'sphericity_correc_anova_2',
          label = 'Escolha a correção para a tabela ANOVA: ',
          choices = c('auto', 'GG', 'HF', 'none'),
          selected = 'auto'
        ))
      correction <- input$sphericity_correc_anova_2
      output$sphericity_anova_test <- renderDT(get_anova_table(res, correction = correction))
    }

    maunchly <- res$`Mauchly's Test for Sphericity`[2:3]
    maunchly$Significância <- if(maunchly$p <= 1 - input$esfericity_ci) 'Significante' else 'Não Significante'
    if('mauchly' %in% input$sphericity_picker)
      output$maunchly_test <- renderDT(maunchly)

    if('corrections' %in% input$sphericity_picker) {
      output$sphericity_corrections_gg <- renderDT(res$`Sphericity Corrections`[2:5])
      output$sphericity_corrections_hf <- renderDT(res$`Sphericity Corrections`[6:9])
    }
    if('results' %in% input$sphericity_picker) {
      output$sphericity_statistics <- renderUI(h4(
        'Pelo teste de esfericidade de Maunchly, ', strong('p =', maunchly$p), '.', if(maunchly$p <= 1 - input$esfericity_ci)
          h4('As variâncias das diferenças entre os grupo ',strong('não são iguais'),' assim não podemos assumir a esfericidade.')
        else h4('As variâncias das diferenças entre os grupo',strong('são iguais'),' conforme o intervalo de confiança, assim podemos assumir
        a esfericidade.'), br())
      )
    }
  })
  observeEvent(input$load_transform_norm, {
    output$transform_norm_results <- renderUI(tagList(
      uiOutput('transform_norm_results_method_name'),
      plotlyOutput('transform_norm_results_plotly'),
      uiOutput('transform_norm_results_method_statistics'),
      downloadButton('transform_norm_download','Baixe a nova tabela!')
    ))

    data <- data.frame(values$bidimensional_data[input$transform_norm_vi], values$bidimensional_data[input$transform_norm_vd])
    colnames(data) <- c('Classificação', 'Dados')

    df <- data
    logy <- if(input$transform_norm_distributions_logy == 1 || input$transform_norm_distributions_logy == -1) 10 else input$transform_norm_distributions_logy
    df$Dados <- switch(
      input$transform_norm_distributions,
      'none' = data$Dados,
      'sqrt' = if(input$transform_norm_distributions_skewed) sqrt(data$Dados) else sqrt(max(data$Dados + 1) - data$Dados),
      'log10' = if(input$transform_norm_distributions_skewed) log10(data$Dados) else log10(max(data$Dados + 1) - data$Dados),
      'logy' = if(input$transform_norm_distributions_skewed) log(data$Dados, logy) else log(max(data$Dados + 1) - data$Dados, logy),
      '1/x' = if(input$transform_norm_distributions_skewed) 1/(data$Dados) else 1/(max(data$Dados + 1) - data$Dados)
    )

    fig <- ggplotly(
      ggdensity(data = df, x = "Dados", color = 'Classificação', fill = 'Classificação', alpha = 0.7)
        # + stat_overlay_normal_density(linetype = "dashed")
    )
    output$transform_norm_results_plotly <- renderPlotly(fig)
    if (input$transform_norm_distributions == 'none')
      output$transform_norm_results_method_statistics <- renderUI(
         h4('O coeficiente de distorção é : ', signif(skewness(data$Dados, na.rm = TRUE), 4))
      )
    else
      output$transform_norm_results_method_statistics <- renderUI(tagList(
         h4('O coeficiente de distorção é : ', signif(skewness(data$Dados, na.rm = TRUE), 4)),
         h4('O novo coeficiente de distorção é : ', signif(skewness(df$Dados, na.rm = TRUE), 4))
      ))

    # Download the new df
    dfDownload <- data

    output$transform_norm_download <- downloadHandler(
      filename = function() { "transformed_df.xlsx"},
      content = function(file) {write_xlsx(dfDownload, path = file)}
    )
  })

  # observeEvent(input$load_transform_bi,{
  #   showTab(inputId = "tabs", target = "Gráficos 2D")
  #   showTab(inputId = "tabs", target = "Gráficos 3D")
  #   showTab(inputId = "tabs", target = "Checando os dados")
  #   showTab(inputId = "tabs", target = "Informações gerais")
  #
  #   dt <- data.frame(values$bidimensional_data[input$transform_bi_variable], values$bidimensional_data[input$transform_bi_variable])
  #
  #   output$table_transform_bi_data_output <- renderUI(
  #     shinycssloaders::withSpinner(
  #       DTOutput("table_transform_bi_data_output2"),
  #       type = spinnerType,
  #       color = spinnerColor,
  #       size = spinnerSize
  #     )
  #   )
  #   output$table_transform_bi_data_output2 <- renderDT(dt)
  #
  #   setUniValues(values, dt)
  #   output$title_name_transform_bi <- renderUI(h2(strong(values$usr_title)))
  # })

  observeEvent(input$load_t_test, {
      output$t_test_results <- renderUI(tagList(
        h3(strong('Estatísticas'), align = 'center'),
        DTOutput('t_test_dt'),
        uiOutput('t_test_effect_size')
      ))

      var1 <- input$test_t_variable_ui

      dt <- values$bidimensional_data
      if(input$test_t_options == 'one') {
        if(!is.numeric(dt[var1][, 1]))
          output$t_test_results <- renderUI('Escolha as variáveis na aba de opções. (Dados inválidos)')
        else{
          test_t <- t.test(dt[var1], mu = input$test_t_mu)
          test_t_df <- data.frame(p = signif(test_t$p.value, 4), estatística = signif(test_t$estimate, 4), df = signif(test_t$parameter, 4), estimativa = signif(test_t$estimate, 4))
          rownames(test_t_df) <- paste0('Test T - ', var1)
          output$t_test_dt <- renderDT(test_t_df)

          l <- dt[var1][,1]
          cohensD <- abs(mean(l) - input$test_t_mu) / sd(l)
          output$t_test_effect_size <- renderUI(p('A área de efeito da variável ', (strong(var1)), ', com mu = ', strong(input$test_t_mu), ' é de: ', strong(signif(cohensD, 4))))
        }
      }
      else if(input$test_t_options == 'two'){
        var2 <- input$test_t_variable_ui2
        if(var1 == var2 | !is.numeric(dt[var1][, 1]) | !is.numeric(dt[var2][, 1]))
          output$t_test_results <- renderUI('Escolha as variáveis na aba de opções. (Dados inválidos)')
        else {
          test_t <- t.test(x = dt[var1], y = dt[var2])
          test_t_df <- data.frame(p = signif(test_t$p.value, 4), estatística = signif(test_t$estimate, 4), df = signif(test_t$parameter, 4), estimativa = signif(test_t$estimate, 4))
          rownames(test_t_df) <- c(paste0('Test T - ', var1), paste0('Test T - ', var2))
          output$t_test_dt <- renderDT(test_t_df)

          l1 <- dt[var1][, 1]
          l2 <- dt[var2][, 1]

          s1 <- sd(l1)
          s2 <- sd(l2)
          n1 <- length(l1)
          n2 <- length(l2)
          pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n1 - 2))
          cohensD <- abs(mean(l1) - mean(l2)) / pooled
          output$t_test_effect_size <-renderUI(p('A área de efeito entre as variáveis ', strong(var1),', e ',strong(var2), ' é de: ', strong(signif(cohensD, 4))))
        }
      }

  })

  observeEvent(input$load_wilcoxon_test, {
    output$wilcoxon_test_results <- renderUI(tagList(
        h3(strong('Estatísticas'), align = 'center'),
        DTOutput('wilcoxon_test_dt'),
        uiOutput('wilcoxon_test_effect_size')
      ))
    var1 <- input$wilcoxon_test_variable_ui

    if(input$wilcoxon_test_options == 'one') {
      dt <- data.frame(values$bidimensional_data[input$wilcoxon_test_variable_ui])
      colnames(dt) <- 'var1'
      mu <- input$wilcoxon_t_mu

      w_test <- dt %>% rstatix::wilcox_test(var1 ~ 1, mu = mu)
      w_test_df <- data.frame(p = signif(w_test$p[[1]], 4), estatística = signif(w_test$statistic[[1]], 4))
      rownames(w_test_df) <- paste0('Test de Wilcoxon - ', var1)
      output$wilcoxon_test_dt <- renderDT(w_test_df)

      w_effectsize <- dt %>% wilcox_effsize(var1 ~ 1, mu = 0)
      output$wilcoxon_test_effect_size <- renderUI(p('A área de efeito da variável ', (strong(var1)), ', com mu = ', mu, ' é de: ', strong(signif(w_effectsize$effsize[[1]], 4))))
    }
    else if(input$wilcoxon_test_options == 'two'){
      dt <- data.frame(values$bidimensional_data[input$wilcoxon_test_variable_ui], values$bidimensional_data[input$wilcoxon_test_variable_ui2])
      colnames(dt) <- c('var1', 'var2')
      var2 <- input$wilcoxon_test_variable_ui2

      if(var1 == var2 | !is.numeric(dt$var1) | !is.numeric(dt$var2))
        output$wilcoxon_test_results <- renderUI('Escolha as variáveis na aba de opções. (Dados inválidos)')
      else{
        dt <- consolidated_data(dt)
        w_test <- dt %>% rstatix::wilcox_test(Dados ~ Classificação)
        w_test_df <- data.frame(p = signif(w_test$p[[1]], 4), estatística = signif(w_test$statistic[[1]], 4))
        rownames(w_test_df) <- paste0('Test de Wilcoxon')
        output$wilcoxon_test_dt <- renderDT(w_test_df)

        w_effectsize <- dt %>% wilcox_effsize(Dados ~ Classificação)
        output$wilcoxon_test_effect_size <- renderUI(p('A área de efeito entre as variáveis ', (strong(var1)),', e ',(strong(var2)),  ', é de: ', strong(signif(w_effectsize$effsize[[1]], 4))))
      }
    }
  })

  observeEvent(input$load_ancova, {
    options$ancova_variable <- input$ancova_variable
    options$ancova_covariable <- input$ancova_covariable
    options$ancova_group_variable <- input$ancova_group_variable
    if(
      options$ancova_variable != options$ancova_covariable & options$ancova_variable != options$ancova_group_variable & options$ancova_covariable != options$ancova_group_variable &
      is.numeric(values$bidimensional_data[options$ancova_variable][[1]]) & is.numeric(values$bidimensional_data[options$ancova_covariable][[1]])
    ){

      output$ancova_statistics <- renderUI(tagList(
        h3(strong('ANCOVA'), align = 'center'),
        DTOutput('ancova_test'),
        h3(strong('Verificação de suposição'), align = 'center'),
        br(),
        column(6,
               h4(strong('Teste de Levene'), align = 'center'),
               DTOutput('ancova_levene_test')
        ),
        column(6,
               h4(strong('Teste de Shapiro-Wilk'), align = 'center'),
               DTOutput('ancova_shapiro_test')
        ),
        h3(strong('Tabela Posthoc:'), align = 'center'),
        DTOutput('ancova_posthoc'),
        h3(strong('Resultados:'), align = 'center'),
        br(),
        uiOutput('ancova_results')
      ))

      output$plotly_ancova <- renderUI(tagList(
        shinycssloaders::withSpinner(
                        plotlyOutput('plotly_ancova2'),
                        type = spinnerType,
                        color = spinnerColor,
                        size = spinnerSize
                      ),
      ))
      output$plotly_ancova2 <- renderPlotly(renderANCOVA(values, options))
      setAncovaValues(values, options)
      output$ancova_test <- renderDT(ancova_table(values, options))
      levene <- levene_table(values, options)
      output$ancova_levene_test <- renderDT(levene)
      shapiro <- shapiro_table(values, options)
      output$ancova_shapiro_test <- renderDT(shapiro)

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
      output$ancova_posthoc <- renderDT(posthoc_table(values, options))
    }
    else{
      output$plotly_ancova <- renderUI(tagList(br(),br(),h3(strong('Escolha as variáveis na aba de opções. (Dados inválidos)'), align = 'center')))
      output$ancova_statistics <- renderUI(p())
    }
  })

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

  #----------- VARIÁVEIS DOS GRÁFICOS -----------
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

  #----------- LAYOUT DOS GRÁFICOS -----------
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

  #Summary
  output$title_name_summary  <- renderUI(h1(strong(values$usr_title)))
  output$summary_data_table <- renderDT(values$data_info)
  output$summary_text <- renderDT(summaryDataTable(values, options))

    #----------- GRÁFICOS -----------
  {
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

    output$var_f_test <- renderUI(
      tagList(
        hr(),
        selectInput(
          inputId = 'first_var_f_test',
          label = 'Escolha o primeiro grupo: ',
          choices = names(table(values$bidimensional_data[input$homogenity_var_vi])),
          selected = ''
        ),
        selectInput(
          inputId = 'sec_var_f_test',
          label = 'Escolha o segundo grupo: ',
          choices = names(table(values$bidimensional_data[input$homogenity_var_vi])),
          selected = ''
        ),
        hr()
      )
    )

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
    output$anova_variables <- renderUI(
      tagList(
        selectInput(
          inputId = 'ancova_variable',
          label = 'Escolha a variável dependente: ',
          choices = names(values$bidimensional_data),
          selected = ''
        ),
        selectInput(
          inputId = 'ancova_group_variable',
          label = 'Escolha a variável independente: ',
          choices = names((values$bidimensional_data)),
          selected = ''
        ),
        actionButton("load_anova",
                     strong('Carregue!'),
                     style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                     width = "80%",
                     class = "btn-info"
        )
      ),
    )
  { output$assessing_norm_variables <- renderUI(tagList(
    selectInput(
      inputId = 'assessing_norm_vi',
      label = 'Escolha a variável independente: ',
      choices = names(values$bidimensional_data),
      selected = ''
    ),
    selectInput(
      inputId = 'assessing_norm_vd',
      label = 'Escolha a variável dependente: ',
      choices = names(values$bidimensional_data),
      selected = ''
    )
  )) }
  { output$homogenity_var_variables <- renderUI(tagList(
    selectInput(
      inputId = 'homogenity_var_vi',
      label = 'Escolha a variável independente: ',
      choices = names(values$bidimensional_data),
      selected = ''
    ),
    selectInput(
      inputId = 'homogenity_var_vd',
      label = 'Escolha a variável dependente: ',
      choices = names(values$bidimensional_data),
      selected = ''
    )
  )) }
  { output$sphericity_variables <- renderUI(tagList(
    selectInput(
      inputId = 'sphericity_vi',
      label = 'Escolha a variável independente: ',
      choices = names(values$bidimensional_data),
      selected = ''
    ),
    selectInput(
      inputId = 'sphericity_vd',
      label = 'Escolha a variável dependente: ',
      choices = names(values$bidimensional_data),
      selected = ''
    )
  )) }
  { output$transform_norm_variables <- renderUI(tagList(
    selectInput(
      inputId = 'transform_norm_vi',
      label = 'Escolha a variável independente: ',
      choices = names(values$bidimensional_data),
      selected = ''
    ),
    selectInput(
      inputId = 'transform_norm_vd',
      label = 'Escolha a variável dependente: ',
      choices = names(values$bidimensional_data),
      selected = ''
    )
  )) }
    output$test_t_variable <- renderUI(tagList(
      selectInput(
        inputId = 'test_t_variable_ui',
        label = 'Escolha a primeira variável: ',
        choices = names(values$bidimensional_data),
        selected = ''
      ),
      uiOutput('test_t_variable_ui2_aux')
    ))
    observeEvent(input$test_t_options, {
      if(input$test_t_options == 'two') {
       output$test_t_variable_ui2_aux <- renderUI(
         selectInput(
           inputId = 'test_t_variable_ui2',
           label = 'Escolha a segunda variável: ',
           choices = names(values$bidimensional_data),
           selected = ''
         ))}
      else output$test_t_variable_ui2_aux <- renderUI(p())
    })

    output$wilcoxon_test_variable <- renderUI(tagList(
      selectInput(
        inputId = 'wilcoxon_test_variable_ui',
        label = 'Escolha a primeira variável: ',
        choices = names(values$bidimensional_data),
        selected = ''
      ),
      uiOutput('wilcoxon_test_variable_ui2_aux')
    ))
    observeEvent(input$wilcoxon_test_options, {
      if(input$wilcoxon_test_options == 'two') {
       output$wilcoxon_test_variable_ui2_aux <- renderUI(
         selectInput(
           inputId = 'wilcoxon_test_variable_ui2',
           label = 'Escolha a segunda variável: ',
           choices = names(values$bidimensional_data),
           selected = ''
         ))}
      else output$test_t_variable_ui2_aux <- renderUI(p())
    })

    output$ancova_variables <- renderUI(
      tagList(
        selectInput(
          inputId = 'ancova_variable',
          label = 'Escolha a variável dependente: ',
          choices = names(values$bidimensional_data),
          selected = ''
        ),
        selectInput(
          inputId = 'ancova_covariable',
          label = 'Escolha a covariável: ',
          choices = names((values$bidimensional_data)),
          selected = ''
        ),
        selectInput(
          inputId = 'ancova_group_variable',
          label = 'Escolha a variável independente: ',
          choices = names((values$bidimensional_data)),
          selected = ''
        ),
        actionButton("load_ancova",
                     strong('Carregue!'),
                     style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                     width = "80%",
                     class = "btn-info"
        )
      ),
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
