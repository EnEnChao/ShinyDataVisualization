source('RFunctions/table.R')
source('RFunctions/PlotFunct.R')

server <- function (input, output, session){

  #----------- VARIÁVEIS -----------
  values <- reactiveValues()
  options <- reactiveValues()

  values$usr_title <- NULL
  values$data_info <- data.frame()
  values$condensed_data_info <- data.frame()

  #Iniciar alguns textos dinâmicos
  output$table_import_data_output <- renderUI(tagList(h2(strong('Importe os seus dados na barra de controle à esquerda:'),align = 'center'), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()))
  output$title_name_insert <- renderUI(h2(strong('Digite os dados:')))

  output$table_import_bi_data_output <- renderUI(tagList(h2(strong('Importe os seus dados na barra de controle à esquerda:'),align = 'center'), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()))
  output$title_name_insert_bi <- renderUI(h2(strong('Digite os dados:')))

  #Iniciar as planilhas
  output$user_data <- renderRHandsontable({ rhandsontable(data = data.frame(matrix('', 1000, 1000))) })
  output$user_data_bi <- renderRHandsontable({ rhandsontable(data = data.frame(matrix('', 1000, 1000))) })

  #Esconder todos os paineis
  hideTab(inputId = "tabs", target = "Gráficos 2D")
  hideTab(inputId = "tabs", target = "Gráficos 3D")
  hideTab(inputId = "tabs", target = "Checando os dados")
  hideTab(inputId = "tabs", target = "Informações gerais")

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
    showTab(inputId = "tabs", target = "Checando os dados")
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
    showTab(inputId = "tabs", target = "Checando os dados")
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
    output$plotly_ancova <- renderUI(tagList(br(),br(),h3(strong('Escolha as variaveis na aba de opções.'), align = 'center')))
    output$ancova_statistics <- renderUI(p(''))
    showTab(inputId = "tabs", target = "Comparando duas médias")
    showTab(inputId = "tabs", target = "Comparando multiplas médias")

    if (input$file_selector_bi == 'example'){
      if(input$examp_select_bi == 'gas')
      dt <- data.frame(read.xlsx('Data/exemplo1ANCOVA.xlsx'))
      if(input$examp_select_bi == 'anxiety') {
        data("anxiety", package = "datarium")
        dt <- anxiety %>%
          select(id, group, t1, t3) %>%
          rename(dependente = t1, covariavel = t3)

        dt[14, "posttest"] <- 19
        set.seed(123)
        dt %>% sample_n_by(group, size = 1)

      }
    }
    else if(input$file_selector_bi == 'import')
      dt <- data.frame(read.xlsx(input$file_imported_bi$datapath))
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
      output$title_name_import_bi <- renderUI(h2(strong(values$usr_title)))
    values$bidimensional_data <- dt
  })

  observeEvent(input$load_spreadsheet_bi,{
    output$plotly_ancova <- renderUI(tagList(br(),br(),h3(strong('Escolha as variaveis na aba de opções.'), align = 'center')))
    output$ancova_statistics <- renderUI(p(''))
    showTab(inputId = "tabs", target = "Comparando duas médias")
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
    } else output$rest_of_sidebar <- renderMenu(NULL)

    values$usr_title <- paste0(input$title_id_insert_bi)
    output$title_name_insert_bi <- renderUI(
      h2(strong(values$usr_title)))

    values$bidimensional_data <- dt
  })

  observeEvent(input$load_ancova, {
    options$ancova_variable <- input$ancova_variable
    options$ancova_covariable <- input$ancova_covariable
    options$ancova_group_variable <- input$ancova_group_variable

    if(!(options$ancova_variable == options$ancova_covariable || options$ancova_variable == options$ancova_group_variable || options$ancova_covariable == options$ancova_group_variable)){

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
    }
    else{
      output$plotly_ancova <- renderUI(tagList(br(),br(),h3(strong('Escolha as variaveis na aba de opções. (Dados inválidos)'), align = 'center')))
    }
  })

  observeEvent(input$load_tridimensional, {
    showTab(inputId = "tabs", target = "Gráfico em Mesh")
    output$mesh_insert_result <- renderUI(h3(strong('Arquivo carregado: \"',input$examp_select_mesh,'\"')))
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
    options$ancova_ci <- input$ancova_ci }

    #Gráfico em Mesh
    { options$examp_select_mesh <- input$examp_select_mesh
    options$checkbox_mesh <- input$checkbox_mesh }
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
    })

  #Summary
  output$title_name_summary  <- renderUI(h1(strong(values$usr_title)))
  output$summary_data_table <- renderDT(values$data_info)
  output$summary_text <- renderDT(summaryDataTable(values, options))

    #----------- GRÁFICOS -----------
  { output$plotly_linear_histogram <- renderPlotly(renderHistogramLinear(values, options))

  output$plot_histogram <- renderPlot(renderHistogramRidges(values, options))

  output$plotly_box_plot <- renderPlotly(renderBoxPlot(values, options))

  output$plotly_violin <- renderPlotly(renderViolinPlot(values, options))

  output$plotly_dot_plot <- renderPlotly(renderDotPlot(values, options))

  output$plotly_beeswarm_dot_plot <- renderPlotly(renderBeeSwarm(values, options))

  output$plotly_density_plot <- renderPlotly(renderDensityPlot(values, options))

  output$plotly_error_bar <- renderPlotly(renderErrorBar(values, options))

  output$plotly_norm_density <- renderPlotly(renderCheckNormDensity(values, options))
  output$plotly_norm_qq <- renderPlotly(renderCheckNormQQ(values, options))
  output$check_norm_table <- renderDT(renderCheckNormTable(values, options))

  output$plotly_histogram3d <- renderPlotly(renderHistogram3d(values, options))

  output$plotly_density3d <- renderPlotly(renderDensityPlot3d(values, options))

  output$plotly_scatter3d <- renderPlotly(renderScatterPlot3d(values, options))

  output$plotly_beeswarm3d <- renderPlotly(renderBeeSwarm3d(values, options))

  output$plotly_bar3d <- renderPlotly(barHistogram3d(values, options))

  output$ancova_variables <- renderUI(
    tagList(
      selectInput(
        inputId = 'ancova_variable',
        label = 'Escolha a variavel dependente: ',
        choices = names(values$bidimensional_data),
        selected = options$ancova_variable
      ),
      selectInput(
        inputId = 'ancova_covariable',
        label = 'Escolha a covariavel: ',
        choices = names((values$bidimensional_data)),
        selected = options$ancova_covariable
      ),
      selectInput(
        inputId = 'ancova_group_variable',
        label = 'Escolha a variavel de grupo: ',
        choices = names((values$bidimensional_data)),
        selected = options$ancova_group_variable
      ),
      actionButton("load_ancova",
                   strong('Carregue!'),
                   style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                   width = "80%",
                   class = "btn-info"
      )
    ),
  )

  output$plotly_mesh3d <- renderPlotly(renderMesh3D(values, options)) }
}
