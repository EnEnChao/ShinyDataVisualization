#----------- SUMMARY DATA TABLE -----------
summaryDataTable <- function(values, options) {
  data <- values$data_info
  df <- st(data, out = 'return', add.median = TRUE)
  df <- df[3:9]
  df$Var <- sapply(df$`Std. Dev.`, function (x) as.double(x)^2 )

  colnames(df) <- c('Média', 'Desvio Padrão', 'Mínimo', '1º Quadrante','Mediana', '3º Quadrante', 'Máximo', 'Variância')
  rownames(df) <- colnames(data)
  if(options$transpose_table)
    df <- t(df)

  return(df)
}

#----------- ADD LAYOUT -----------
addLayout <- function(fig, title, layoutConfig = NULL) {
  fig <- fig %>% layout(
    title = list(text = paste0('<b> ', title, ' </b>'), font = fontTit),
    xaxis = list(title = list(text = layoutConfig$xTitle, font = fontAxis)),
    yaxis = list(title = list(text = layoutConfig$yTitle, font = fontAxis)),
    font = genericFont,
    margin = margin,
    plot_bgcolor = if(is.null(layoutConfig$bgcolor)) 'white' else { if (layoutConfig$bgcolor == 'personal') layoutConfig$pBgcolor else layoutConfig$bgcolor },
    showlegend = layoutConfig$legend,
    legend = if(layoutConfig$legend) list(
      title = if(layoutConfig$legend_bold) list(text = paste0('<b>',layoutConfig$legend_title , '<b>')) else list(text = layoutConfig$legend_title),
      itemsize = layoutConfig$legend_size,
      orientation = layoutConfig$legend_orientation,
      bgcolor = 'transparent',
      bordercolor = 'black',
      borderwidth = if(layoutConfig$legend_border) 2 else 0
    )
  )

  return(fig)
}

#----------- HISTOGRAM -----------
renderHistogramLinear <- function(values, options) {
  fig <- plot_ly(data = values$c_data_info, x = ~Dados, color = ~Classificação,
                 alpha = options$opacity_histogram,
                 type = 'histogram',
                 bingroup = 1,
                 legendgroup =~ Classificação,
                 nbinsx = options$bandwidth_histogram
  )
  if(options$show_density_histogram)
    for (i in seq(values$ncol)){
      data_density <- density(values$data_info[,i])
      fig <- fig %>% add_trace(x = data_density$x, y = data_density$y* ( max(values$data_info[,i]) * options$density_histogram_scale),
                               color = values$names[i],
                               type = 'scatter',
                               mode = 'lines',
                               fill = options$density_histogram_area,
                               opacity = options$density_histogram_line_opacity,
                               alpha = if (options$density_histogram_area ==  'none') options$density_histogram_line_opacity else options$density_histogram_area_opacity,
                               showlegend = FALSE,
                               legendgroup = values$names[i]
      )
    }

  layoutConfig <- list(
    bgcolor = if(is.null(options$colors_linear_histogram)) options$bgColorPlotly else{
         if(options$colors_linear_histogram) options$bgColorPlotly else{
           if(options$bgcolor_linear_histogram == 'personal') options$personal_bgcolor_linear_histogram
           else options$bgcolor_linear_histogram
    }},
    xTitle = if(!is.null(options$axis_x_linear_histogram)) options$axis_x_linear_histogram else 'Dados',
    yTitle = if(!is.null(options$axis_y_linear_histogram)) options$axis_y_linear_histogram else 'Frequência',

    legend = if(is.null(options$legend_linear_histogram)) TRUE else options$legend_linear_histogram,
    legend_title = if(is.null(options$legend_linear_histogram) | if(is.null(options$legend_linear_histogram)) TRUE else !options$legend_linear_histogram) '' else options$title_legend_linear_histogram,
    legend_bold = if(is.null(options$legend_linear_histogram) | if(is.null(options$legend_linear_histogram)) TRUE else !options$legend_linear_histogram) FALSE else options$bold_title_legend_linear_histogram,
    legend_size = if(is.null(options$legend_linear_histogram) | if(is.null(options$legend_linear_histogram)) TRUE else !options$legend_linear_histogram) 'trace' else options$item_size_legend_linear_histogram,
    legend_orientation = if(is.null(options$legend_linear_histogram) | if(is.null(options$legend_linear_histogram)) TRUE else !options$legend_linear_histogram) 'v' else options$orientation_legend_linear_histogram,
    legend_border = if(is.null(options$legend_linear_histogram) | if(is.null(options$legend_linear_histogram)) TRUE else !options$legend_linear_histogram) FALSE else options$border_legend_linear_histogram
  )
  fig <- fig %>% addLayout(values$usr_title, layoutConfig = layoutConfig)

  if (options$stack_histogram)
    fig <- fig %>% layout(barmode = "stack")
  else fig <- fig %>% layout(barmode = "overlay")

  if (options$bargap_histogram)
    fig <- fig %>% layout(bargap = options$bargap_histogram_level)

  return(fig)

}

#----------- HISTOGRAM - RIDGES -----------
renderHistogramRidges <- function(values, options) {
  img <- ggplot(values$c_data_info, aes(x = Dados, y = Classificação, fill = stat(x))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                        bandwidth = options$bandwidth_ridges_histogram,
                        quantile_lines = options$show_quartis_ridges_histogram,
                        quantiles = options$n_quartis_ridges_histogram,
                        show.legend = TRUE,
                        jittered_points = options$points_ridges_histogram,
                        position = options$points_position_ridges_histogram,
                        point_shape = options$points_shape_ridges_histogram,
                        point_size = options$points_size_ridges_histogram,
                        point_alpha = options$points_opacity_ridges_histogram,
                        alpha = 0.7,
                        scale = options$scale_ridges_histogram
    ) +
    scale_fill_viridis_c(name = 'Legenda de valores',
                         direction = options$reverse_ridges_histogram,
                         option = options$ridges_color
                         # ,alpha = options$opacity_ridges_histogram
    ) +
    ggtitle(values$usr_title) +
    # scale_x_continuous(name = 'Dados') +
    # scale_y_continuous(name = 'Classificação') +
    theme(
      axis.title.y = element_text(size = 15, color = 'red', angle = 0, face = "bold"),
      axis.title.x = element_text(size = 15, color = 'orange', angle = 0, face = "bold"),
      plot.title = element_text(size = 18, face = "italic", hjust = 0.5)
    ) +
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)
  return(img)
}

#----------- BOX PLOT -----------
renderBoxPlot <- function(values, options) {

  #Plot
  fig <- plot_ly(data = values$c_data_info, y = ~Dados, x = ~Classificação, color = ~Classificação,
                 type = 'box',
                 quartilemethod = options$box_algorithm,
                 boxpoints = options$point_box,
                 legendgroup = ~Classificação,
                 jitter = options$jitter_box,
                 pointpos = options$jitter_pointpos,
                 marker = list(
                   opacity = options$points_opacity_box,
                   line = if(options$point_box == 'all') list(
                     width = options$points_width_box,
                     color = 'black'
                   ) else list(autocolorscale = TRUE),
                   size = options$points_size_box,
                   symbol = options$dot_box_shape
                 ),
                 boxmean = options$meanline_box
  )
  layoutConfig <- list(bgcolor = if(is.null(options$colors_box_plots)) options$bgColorPlotly else{
         if(options$colors_box_plots) options$bgColorPlotly else{
           if(options$bgcolor_box_plots == 'personal') options$personal_bgcolor_box_plots
           else options$bgcolor_box_plots
             }},
                       pBgcolor = options$personal_bgcolor_box_plots,
                       xTitle = if(!is.null(options$axis_x_box_plots)) options$axis_x_box_plots else 'Classificação',
                       yTitle = if(!is.null(options$axis_y_box_plots)) options$axis_y_box_plots else 'Dados',
    legend = if(is.null(options$legend_box_plots)) TRUE else options$legend_box_plots,
    legend_title = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) '' else options$title_legend_box_plots,
    legend_bold = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) FALSE else options$bold_title_legend_box_plots,
    legend_size = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) 'trace' else options$item_size_legend_box_plots,
    legend_orientation = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) 'v' else options$orientation_legend_box_plots,
    legend_border = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) FALSE else options$border_legend_box_plots
  )

  fig <- fig %>% addLayout(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}

#----------- VIOLIN -----------
renderViolinPlot <- function(values, options) {
  #Plot
  fig <- plot_ly(data = values$c_data_info, y = ~Dados, x = ~Classificação, color = ~Classificação,
                 split = ~Classificação,
                 type = 'violin',
                 legendgroup = ~Classificação,
                 hoveron = "points+kde",
                 points = options$point_violin,
                 pointpos = options$jitter_pointpos,
                 scalemode = 'count',
                 jitter = options$jitter_violin,
                 marker = list(
                   opacity = options$points_opacity_violin,
                   line = list(
                     width = options$points_width_violin,
                     color = 'black'
                   ),
                   size = options$points_size_violin,
                   symbol = options$dot_violin_shape
                 ),
                 bandwidth = options$bandwidth_violin,
                 meanline = list(
                   visible = options$meanline_violin
                 )
  )
  fig <- fig %>% add_trace(legendgroup = ~Classificação,
                           showlegend = FALSE,
                           type = 'violin',
                           quartilemethod = options$violin_algorithm,
                           width = 0.1
  )
  layoutConfig <- list(bgcolor = if(is.null(options$colors_box_plots)) options$bgColorPlotly else{
         if(options$colors_box_plots) options$bgColorPlotly else{
           if(options$bgcolor_box_plots == 'personal') options$personal_bgcolor_box_plots
           else options$bgcolor_box_plots
             }},
                       pBgcolor = options$personal_bgcolor_box_plots,
                       xTitle = if(!is.null(options$axis_x_box_plots)) options$axis_x_box_plots else 'Classificação',
                       yTitle = if(!is.null(options$axis_y_box_plots)) options$axis_y_box_plots else 'Dados',
    legend = if(is.null(options$legend_box_plots)) TRUE else options$legend_box_plots,
    legend_title = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) '' else options$title_legend_box_plots,
    legend_bold = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) FALSE else options$bold_title_legend_box_plots,
    legend_size = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) 'trace' else options$item_size_legend_box_plots,
    legend_orientation = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) 'v' else options$orientation_legend_box_plots,
    legend_border = if(is.null(options$legend_box_plots) | if(is.null(options$legend_box_plots)) TRUE else !options$legend_box_plots) FALSE else options$border_legend_box_plots
  )

  fig <- fig %>% addLayout(values$usr_title, layoutConfig = layoutConfig)
  return(fig)
}

#----------- DOT PLOT -----------
renderDotPlot <- function(values, options) {

  data <- get_bin_freq(data_info = values$data_info,bins = options$bins_dot_plot, min = values$abs_min, max = values$abs_max)

  if (is.null(options$shape_markers_dot_plot))
    options$shape_markers_dot_plot <- 'circle'
  #Plot
  fig <- plot_ly(data = data, x = ~Dados, y = ~Freq)
  fig <- fig %>% add_trace(
    type = 'scatter',
    mode = 'markers',
    color = ~Classificação,
    legendgroup = ~Classificação,
    symbol = if(options$shape_markers_dot_plot == 'shapes') ~Classificação else 'circle',
    symbols = if(options$shape_markers_dot_plot == 'shapes') '' else options$shape_markers_dot_plot,
    name = ~Classificação,
    marker = list(
      size = options$size_markers_dot_plot,
      opacity = ~options$opacity_markers_dot_plot,
      line = if (options$line_markers_dot_plot) { list(color = 'black', width = 1) } else { list(width = 0) }
    )
  )
  if (options$line_dot_plot)
    fig <- fig %>% add_lines(type = 'scatter', mode = 'lines',
                             alpha = options$opacity_line_dot_plot,
                             showlegend = FALSE,
                             color = ~Classificação,
                             legendgroup = ~Classificação)

  #Calculo da ellipse
  if (options$ellipse_dot_plot){
    ellipse <- ellipse_data(data, values$names, options$ci_ellipse)

    fig <- fig %>% add_trace(x = ellipse$x, y = ellipse$y,
                             type = 'scatter', mode = 'lines',
                             line = list( dash = options$ellipse_line_format),
                             legendgroup = ellipse$Classificação,
                             name = ellipse$Classificação,
                             color = ellipse$Classificação,
                             showlegend = FALSE,
                             fill = 'toself',
                             # dash = 'dash',
                             alpha = if (!options$ellipse_area_dot_plot) 0 else options$area_opacity_ellipse
    )
  }

  layoutConfig <- list(bgcolor = if(is.null(options$colors_simple_dot_plot)) options$bgColorPlotly else{
         if(options$colors_simple_dot_plot) options$bgColorPlotly else{
           if(options$bgcolor_simple_dot_plot == 'personal') options$personal_bgcolor_simple_dot_plot
           else options$bgcolor_simple_dot_plot
             }},
                       pBgcolor = options$personal_bgcolor_simple_dot_plot,
                       xTitle = if(!is.null(options$axis_x_simple_dot_plot)) options$axis_x_simple_dot_plot else 'Classificação',
                       yTitle = if(!is.null(options$axis_y_simple_dot_plot)) options$axis_y_simple_dot_plot else 'Dados',
    legend = if(is.null(options$legend_simple_dot_plot)) TRUE else options$legend_simple_dot_plot,
    legend_title = if(is.null(options$legend_simple_dot_plot) | if(is.null(options$legend_simple_dot_plot)) TRUE else !options$legend_simple_dot_plot) '' else options$title_legend_simple_dot_plot,
    legend_bold = if(is.null(options$legend_simple_dot_plot) | if(is.null(options$legend_simple_dot_plot)) TRUE else !options$legend_simple_dot_plot) FALSE else options$bold_title_legend_simple_dot_plot,
    legend_size = if(is.null(options$legend_simple_dot_plot) | if(is.null(options$legend_simple_dot_plot)) TRUE else !options$legend_simple_dot_plot) 'trace' else options$item_size_legend_simple_dot_plot,
    legend_orientation = if(is.null(options$legend_simple_dot_plot) | if(is.null(options$legend_simple_dot_plot)) TRUE else !options$legend_simple_dot_plot) 'v' else options$orientation_legend_simple_dot_plot,
    legend_border = if(is.null(options$legend_simple_dot_plot) | if(is.null(options$legend_simple_dot_plot)) TRUE else !options$legend_simple_dot_plot) FALSE else options$border_legend_simple_dot_plot
  )

  fig <- fig %>% layout(xaxis = list(categoryorder = 'category ascending'))
  fig <- fig %>% addLayout(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}

#----------- BEE SWARM -----------
renderBeeSwarm <- function (values, options){
  bee_swarm <- sapply(values$data_info, function (x) beeswarm(x,
                                                              method = options$beeswarm_method,
                                                              do.plot = FALSE,
                                                              priority = if(options$beeswarm_method == 'swarm' || options$beeswarm_method == 'compactswarm') options$priority_beeswarm else c("ascending", "descending", "density", "random", "none"),
                                                              side = if(options$side_beeswarm) 1  else 0L,
                                                              cex = options$size_beeswarm,
                                                              spacing = options$spacing_markers_beeswarm
  ))
  data <- data.frame(Dados = NULL, Classificação = NULL, y = NULL)
  for (i in seq(values$ncol))
    data <- rbind(data, data.frame(Dados = bee_swarm[, i]$x + (i - 1) * options$width_beeswarm, y = bee_swarm[, i]$y, Classificação = colnames(bee_swarm)[i]))
  fig <- plot_ly(data = data, x = ~Dados, y = ~y,
                 type = 'scatter',
                 mode = 'markers',
                 color = ~Classificação,
                 legendgroup = ~Classificação,
                 symbol = if (options$shape_markers_beeswarm == 'shapes') ~Classificação else 'circle',
                 symbols = if (options$shape_markers_beeswarm == 'shapes') '' else options$shape_markers_beeswarm,
                 name = ~Classificação,
                 marker = list(
                   size = options$size_markers_beeswarm,
                   opacity = options$opacity_markers_beeswarm,
                   line = if (options$line_markers_beeswarm) { list(color = 'black', width = 1) } else { list(width = 0) }
                 )
  )

  layoutConfig <- list(bgcolor = if(is.null(options$colors_beeswarm_dot_plot)) options$bgColorPlotly else{
         if(options$colors_beeswarm_dot_plot) options$bgColorPlotly else{
           if(options$bgcolor_beeswarm_dot_plot == 'personal') options$personal_bgcolor_beeswarm_dot_plot
           else options$bgcolor_beeswarm_dot_plot
             }},
                     pBgcolor = options$personal_bgcolor_beeswarm_dot_plot,
                     xTitle = if(!is.null(options$axis_x_beeswarm_dot_plot)) options$axis_x_beeswarm_dot_plot else 'Frequência',
                     yTitle = if(!is.null(options$axis_y_beeswarm_dot_plot)) options$axis_y_beeswarm_dot_plot else 'Dados',
  legend = if(is.null(options$legend_beeswarm_dot_plot)) TRUE else options$legend_beeswarm_dot_plot,
  legend_title = if(is.null(options$legend_beeswarm_dot_plot) | if(is.null(options$legend_beeswarm_dot_plot)) TRUE else !options$legend_beeswarm_dot_plot) '' else options$title_legend_beeswarm_dot_plot,
  legend_bold = if(is.null(options$legend_beeswarm_dot_plot) | if(is.null(options$legend_beeswarm_dot_plot)) TRUE else !options$legend_beeswarm_dot_plot) FALSE else options$bold_title_legend_beeswarm_dot_plot,
  legend_size = if(is.null(options$legend_beeswarm_dot_plot) | if(is.null(options$legend_beeswarm_dot_plot)) TRUE else !options$legend_beeswarm_dot_plot) 'trace' else options$item_size_legend_beeswarm_dot_plot,
  legend_orientation = if(is.null(options$legend_beeswarm_dot_plot) | if(is.null(options$legend_beeswarm_dot_plot)) TRUE else !options$legend_beeswarm_dot_plot) 'v' else options$orientation_legend_beeswarm_dot_plot,
  legend_border = if(is.null(options$legend_beeswarm_dot_plot) | if(is.null(options$legend_beeswarm_dot_plot)) TRUE else !options$legend_beeswarm_dot_plot) FALSE else options$border_legend_beeswarm_dot_plot
  )

  fig <- fig %>% addLayout(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}

#----------- DENSITY PLOT -----------
renderDensityPlot <- function(values, options) {
  data <- values$c_data_info$Dados
  fig <- plot_ly(x = ~density(data[seq_len(values$nrow)], kernel = options$algorithm_density_plot)$x,
                 y = ~density(data[seq_len(values$nrow)], kernel = options$algorithm_density_plot)$y,
                 type = 'scatter', mode = options$line_density, fill = options$area_density,
                 name = values$names[1]
  )

  for (i in seq(values$ncol - 1)) {
    s <- i * values$nrow + 1
    e <- (i + 1) * values$nrow
    data_aux <- data.frame(x_aux = density(data[s:e], kernel = options$algorithm_density_plot)$x, y_aux = density(data[s:e], kernel = options$algorithm_density_plot)$y)
    fig <- fig %>% add_trace(
      x = ~x_aux,
      y = ~y_aux,
      data = data_aux,
      fill = options$area_density,
      name = values$names[i + 1]
    )
  }
  layoutConfig <- list(bgcolor = if(is.null(options$colors_density_plot)) options$bgColorPlotly else{
         if(options$colors_density_plot) options$bgColorPlotly else{
           if(options$bgcolor_density_plot == 'personal') options$personal_bgcolor_density_plot
           else options$bgcolor_density_plot
             }},
                       pBgcolor = options$personal_bgcolor_density_plot,
                       xTitle = if(!is.null(options$axis_x_density_plot)) options$axis_x_density_plot else 'Dados',
                       yTitle = if(!is.null(options$axis_y_density_plot)) options$axis_y_density_plot else 'Densidade',
    legend = if(is.null(options$legend_density_plot)) TRUE else options$legend_density_plot,
    legend_title = if(is.null(options$legend_density_plot) | if(is.null(options$legend_density_plot)) TRUE else !options$legend_density_plot) '' else options$title_legend_density_plot,
    legend_bold = if(is.null(options$legend_density_plot) | if(is.null(options$legend_density_plot)) TRUE else !options$legend_density_plot) FALSE else options$bold_title_legend_density_plot,
    legend_size = if(is.null(options$legend_density_plot) | if(is.null(options$legend_density_plot)) TRUE else !options$legend_density_plot) 'trace' else options$item_size_legend_density_plot,
    legend_orientation = if(is.null(options$legend_density_plot) | if(is.null(options$legend_density_plot)) TRUE else !options$legend_density_plot) 'v' else options$orientation_legend_density_plot,
    legend_border = if(is.null(options$legend_density_plot) | if(is.null(options$legend_density_plot)) TRUE else !options$legend_density_plot) FALSE else options$border_legend_density_plot
  )

  fig <- fig %>% addLayout(values$usr_title, layoutConfig = layoutConfig)
  return(fig)
}

#----------- ERROR BAR -----------
renderErrorBar <- function(values, options) {
  error <- switch(
    options$error_algorithm,
    'sd' = as.double(lapply(values$data_info, function(x) sd(x))),
    'se' = as.double(lapply(values$data_info, function(x) { sd(x) / sqrt(length(x)) })),
    'ci' = as.double(lapply(values$data_info, function(x) { qt((options$alpha_ci) / 2 + .5, length(x) - 1) * (sd(x) / sqrt(length(x))) }))
  )
  data <- data.frame(
    names = values$names,
    mean = as.double(lapply(values$data_info, function(x) mean(x))),
    error = error
  )

  if (is.null(options$markers_shape_error_bar))
    options$markers_shape_error_bar <- 'circle'

  fig <- plot_ly(data = data)
  fig <- fig %>% add_trace(x = ~names, y = ~mean,
                 type = 'scatter', mode = 'markers',
                 opacity = ~options$opacity_markers_error_bar,
                 marker = list(
                   size = options$size_markers_error_bar,
                   line = if (options$line_markers_error_bar) { list(color = 'black', width = 1) } else { list(width = 0) }
                 ),
                 legendgroup = ~names,
                 symbol = if(options$markers_shape_error_bar == 'shapes') ~names else 'circle',
                 symbols = if(options$markers_shape_error_bar == 'shapes') '' else ~options$markers_shape_error_bar,
                 name = ~names
  )

  if (options$error_bar)
    fig <- fig %>% add_trace(x = ~names, y = ~mean,
                             type = 'bar',
                             alpha = 0.8,
                             legendgroup = ~names,
                             color = ~names,
                             showlegend = FALSE,
                             opacity = options$opacity_error_bar
    )
  if (options$error_line)
    fig <- fig %>% add_trace(x = ~names, y = ~mean,
                             error_y = list(array = error, color = '#000000'),
                             type = 'scatter',
                             mode = 'markers',
                             opacity = options$opacity_error_line,
                             alpha = 0,
                             # color = ~names,
                             legendgroup = ~names,
                             showlegend = FALSE
    )

  layoutConfig <- list(bgcolor = if(is.null(options$colors_error_bar)) options$bgColorPlotly else{
         if(options$colors_error_bar) options$bgColorPlotly else{
           if(options$bgcolor_error_bar == 'personal') options$personal_bgcolor_error_bar
           else options$bgcolor_error_bar
             }},
                       pBgcolor = options$personal_bgcolor_error_bar,
                       xTitle = if(!is.null(options$axis_x_error_bar)) options$axis_x_error_bar else 'Dados',
                       yTitle = if(!is.null(options$axis_y_error_bar)) options$axis_y_error_bar else 'Densidade',
    legend = if(is.null(options$legend_error_bar)) TRUE else options$legend_error_bar,
    legend_title = if(is.null(options$legend_error_bar) | if(is.null(options$legend_error_bar)) TRUE else !options$legend_error_bar) '' else options$title_legend_error_bar,
    legend_bold = if(is.null(options$legend_error_bar) | if(is.null(options$legend_error_bar)) TRUE else !options$legend_error_bar) FALSE else options$bold_title_legend_error_bar,
    legend_size = if(is.null(options$legend_error_bar) | if(is.null(options$legend_error_bar)) TRUE else !options$legend_error_bar) 'trace' else options$item_size_legend_error_bar,
    legend_orientation = if(is.null(options$legend_error_bar) | if(is.null(options$legend_error_bar)) TRUE else !options$legend_error_bar) 'v' else options$orientation_legend_error_bar,
    legend_border = if(is.null(options$legend_error_bar) | if(is.null(options$legend_error_bar)) TRUE else !options$legend_error_bar) FALSE else options$border_legend_error_bar
  )

  fig <- fig %>% addLayout(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}

renderCustomPlot <- function (values, options){
  fig <- subplot(renderErrorBar(values, options), renderDotPlot(values, options) ,renderBoxPlot(values, options), renderViolinPlot(values, options), nrows = 2)

  return(fig)
}

renderCheckNormDensity <- function (values, options){
  data <- values$c_data_info
    fig <- ggplotly(
    ggdensity(data = data, x = "Dados", color = 'Classificação', ggtheme = theme_minimal())
  )
  fig
}
renderCheckNormQQ <- function (values, options){
  data <- values$c_data_info
  fig <- ggplotly(
    ggqqplot(data = data, x = "Dados", color = 'Classificação', ggtheme = theme_minimal())
  )
  fig
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

renderANCOVA <- function (values, options){

  dt <- values$bidimensional_data
  groups <- names(table(dt[,3]))

  fig <- plot_ly()

  fig <- fig %>% add_trace(
    type = 'scatter', mode = 'lines+markers',
    y = c(
      min(dt[,1]) * lm(dt[,2]~dt[,1] + dt[,3])$coefficients[2] + lm(dt[,2]~dt[,1] + dt[,3])$coefficients[1],
      max(dt[,1]) * lm(dt[,2]~dt[,1] + dt[,3])$coefficients[2] + lm(dt[,2]~dt[,1] + dt[,3])$coefficients[1]),
    x = c(min(dt[,1]), max(dt[,1])
    ),
    line = list(
      width = options$ancova_line_width,
      dash = 'longdash',
      color = 'black',
      shape = 'spline'
    ),
    marker = list(
      symbol = 'line-ns-open',
      color = 'black',
      size = 12
      # ,width = 5
    )
    # ,showlegend = FALSE
  )
  for (i in groups){
    dt_aux <- dt[which(dt[,3] == i),]
    fig <- fig%>% add_trace(
          type = 'scatter', mode = 'lines+markers', color = i,legendgroup = i,
          y = c(
            min(dt_aux[,1]) * lm(dt_aux[,2]~dt_aux[,1])$coefficients[2] + lm(dt_aux[,2]~dt_aux[,1])$coefficients[1],
            max(dt_aux[,1]) * lm(dt_aux[,2]~dt_aux[,1])$coefficients[2] + lm(dt_aux[,2]~dt_aux[,1])$coefficients[1]
          ),
          x = c(min(dt_aux[,1]), max(dt_aux[,1])),
          line = list(
            width = options$ancova_line_width
          ),
          marker = list(
            symbol = 'line-ns-open',
            size = 12
            # ,width = 5
          )
    )
  }
    fig <- fig %>% add_trace(type = 'scatter', mode = 'markers',
                             x = dt[,1],
                             y = dt[,2],
                             color = dt[,3],
                             legendgroup = dt[,3],
                             showlegend = FALSE,
                             marker = list(
                               opacity = options$ancova_marker_opacity,
                               size = options$ancova_marker_size
                               # ,line = list(
                               #   width = 1,
                               #   color = 'black'
                               # )
                             )
  )

  fig
}