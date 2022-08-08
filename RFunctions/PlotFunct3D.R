#----------- LAYOUT 3D -----------
addLayout3d <- function (fig, title, layoutConfig = NULL){
  fig <- fig %>% layout(
    title = list(text = paste0('<b> ', title, ' </b>'), font = fontTit),
    scene = list(
      xaxis = list(title = list(text = layoutConfig$xTitle, font = fontAxis),
                   showbackground=TRUE,
                   backgroundcolor = if(is.null(layoutConfig$bgcolor)) 'white' else { if (layoutConfig$bgcolor == 'personal') layoutConfig$pBgcolor else layoutConfig$bgcolor }),
      yaxis = list(title = list(text = layoutConfig$yTitle, font = fontAxis),
                   showbackground=TRUE,
                   backgroundcolor = if(is.null(layoutConfig$bgcolor)) 'white' else { if (layoutConfig$bgcolor == 'personal') layoutConfig$pBgcolor else layoutConfig$bgcolor }),
      zaxis = list(title = list(text = layoutConfig$zTitle, font = fontAxis),
                   showbackground=TRUE,
                   backgroundcolor = if(is.null(layoutConfig$bgcolor)) 'white' else { if (layoutConfig$bgcolor == 'personal') layoutConfig$pBgcolor else layoutConfig$bgcolor })
    ),
    font = genericFont,
    margin = margin,
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

#----------- HISTOGRAM 3D -----------
renderHistogram3d <- function (values, options){
  data <- get_bin_freq(values$data_info, bins = options$bins_histogram3d, min = values$abs_min, max = values$abs_max)

  fig <- plot_ly(data, x = ~Dados, z = ~Freq, y = ~Classificação,
                 color = ~Classificação,
                 type = 'scatter3d',
                 mode = 'lines',
                 line = list(
                   width = options$width_line_histogram3d,
                   dash = 'solid'
                 ),
                 alpha = options$opacity_histogram3d,
                 showlegend = TRUE,
                 legendgroup = ~Classificação,
                 surfaceaxis=0
  )
  if(options$markers_histogram3d)
    fig <- fig %>% add_trace(
      type = 'scatter3d',
      mode = 'markers',
      color = ~Classificação,
      symbol = options$shape_markers_histogram3d,
      symbols = options$shape_markers_histogram3d,
      name = ~Classificação,
      marker = list(
        size = options$size_markers_histogram3d
      ),
      legendgroup = ~Classificação,
      showlegend = FALSE
    )

  layoutConfig <- list(
    bgcolor = if(is.null(options$colors_histogram_3d)) options$bgColorPlotly else{
         if(options$colors_histogram_3d) options$bgColorPlotly else{
           if(options$bgcolor_histogram_3d == 'personal') options$personal_bgcolor_histogram_3d
           else options$bgcolor_histogram_3d
    }},
    xTitle = if(!is.null(options$axis_x_histogram_3d)) options$axis_x_histogram_3d else 'Dados',
    yTitle = if(!is.null(options$axis_y_histogram_3d)) options$axis_y_histogram_3d else 'Frequência',
    zTitle = if(!is.null(options$axis_z_histogram_3d)) options$axis_z_histogram_3d else 'Classificação',

    legend = if(is.null(options$legend_histogram_3d)) TRUE else options$legend_histogram_3d,
    legend_title = if(is.null(options$legend_histogram_3d) | if(is.null(options$legend_histogram_3d)) TRUE else !options$legend_histogram_3d) '' else options$title_legend_histogram_3d,
    legend_bold = if(is.null(options$legend_histogram_3d) | if(is.null(options$legend_histogram_3d)) TRUE else !options$legend_histogram_3d) FALSE else options$bold_title_legend_histogram_3d,
    legend_size = if(is.null(options$legend_histogram_3d) | if(is.null(options$legend_histogram_3d)) TRUE else !options$legend_histogram_3d) 'trace' else options$item_size_legend_histogram_3d,
    legend_orientation = if(is.null(options$legend_histogram_3d) | if(is.null(options$legend_histogram_3d)) TRUE else !options$legend_histogram_3d) 'v' else options$orientation_legend_histogram_3d,
    legend_border = if(is.null(options$legend_histogram_3d) | if(is.null(options$legend_histogram_3d)) TRUE else !options$legend_histogram_3d) FALSE else options$border_legend_histogram_3d  
  )

  fig <- fig %>% addLayout3d(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}

#----------- SURFACE 3D -----------
renderMeshPlot3d <- function (values, options){
  fig <- plot_ly()
  brks <- hist(values$c_data_info$Dados, plot = FALSE)$breaks
  for (i in seq(values$ncol)){
    data_density <- hist(values$data_info[,i], plot = FALSE, breaks = brks)
    dt <- data.frame(Dados = data_density$breaks[1:length(data_density$counts)], Frequência = data_density$counts, Classificação = values$names[i])
    fig <- fig %>% add_trace(data = dt, x=~ Dados, z=~ Classificação, y =~ Frequência,
                             color = ~Classificação,
                             type = 'mesh3d',
                             legendgroup =~ Classificação,
                             opacity = options$opacity_mesh3d,
                             showlegend = TRUE
    )
    if(options$markers_mesh3d)
      fig <- fig %>% add_trace(data = dt, x=~ Dados, z=~ Classificação, y =~ Frequência,
                               type = 'scatter3d',
                               mode = 'markers',
                               color = ~Classificação,
                               legendgroup =~ Classificação,
                               marker = list(
                                 size = options$size_markers_mesh3d,
                                 line = if (options$line_markers_mesh3d) { list(color = 'black', width = 1) } else { list(width = 0) }
                               ),
                               showlegend = FALSE
      )
    if(options$lines_mesh3d)
      fig <- fig %>% add_trace(data = dt, x=~ Dados, z=~ Classificação, y =~ Frequência,
                               type = 'scatter3d',
                               mode = 'lines',
                               color = ~Classificação,
                               legendgroup =~ Classificação,
                               line = list(
                                 width = options$width_markers_mesh3d
                               ),
                               showlegend = FALSE
      )
  }
  # fig <- fig %>% addLayout3d(values$usr_title, 'Dados', 'Classificação', 'Frequência')

  return(fig)
}

#----------- DENSITY PLOT 3D -----------
renderDensityPlot3d <- function (values, options){

  data_density <- data.frame(Dados = NULL, Frequência = NULL, Classificação = NULL)
  for (i in seq(values$ncol)){
    density <- density(values$data_info[,i], kernel = options$algorithm_density_plot3d)
    data_aux <- data.frame(Dados = density$x, Frequência = density$y, Classificação = values$names[i])
    data_density <- rbind(data_density, data_aux)
  }

  fig <- plot_ly(data = data_density, x =~ Dados, y =~ Classificação, z =~ Frequência,
                           color =~ Classificação,
                           type = 'scatter3d',
                           mode = 'lines',
                           fill = 'tozeroy',
                           line = list(
                             width = options$width_markers_density3d
                           ),
                           # opacity = options$density_histogram_line_opacity,
                           # alpha = if (options$density_histogram_area ==  'none') options$density_histogram_line_opacity else options$density_histogram_area_opacity,
                           showlegend = TRUE,
                           legendgroup =~ Classificação
  )

  layoutConfig <- list(
    bgcolor = if(is.null(options$colors_density_plot_3d)) options$bgColorPlotly else{
         if(options$colors_density_plot_3d) options$bgColorPlotly else{
           if(options$bgcolor_density_plot_3d == 'personal') options$personal_bgcolor_density_plot_3d
           else options$bgcolor_density_plot_3d
    }},
    xTitle = if(!is.null(options$axis_x_density_plot_3d)) options$axis_x_density_plot_3d else 'Dados',
    yTitle = if(!is.null(options$axis_y_density_plot_3d)) options$axis_y_density_plot_3d else 'Frequência',
    zTitle = if(!is.null(options$axis_z_density_plot_3d)) options$axis_z_density_plot_3d else 'Classificação',

    legend = if(is.null(options$legend_density_plot_3d)) TRUE else options$legend_density_plot_3d,
    legend_title = if(is.null(options$legend_density_plot_3d) | if(is.null(options$legend_density_plot_3d)) TRUE else !options$legend_density_plot_3d) '' else options$title_legend_density_plot_3d,
    legend_bold = if(is.null(options$legend_density_plot_3d) | if(is.null(options$legend_density_plot_3d)) TRUE else !options$legend_density_plot_3d) FALSE else options$bold_title_legend_density_plot_3d,
    legend_size = if(is.null(options$legend_density_plot_3d) | if(is.null(options$legend_density_plot_3d)) TRUE else !options$legend_density_plot_3d) 'trace' else options$item_size_legend_density_plot_3d,
    legend_orientation = if(is.null(options$legend_density_plot_3d) | if(is.null(options$legend_density_plot_3d)) TRUE else !options$legend_density_plot_3d) 'v' else options$orientation_legend_density_plot_3d,
    legend_border = if(is.null(options$legend_density_plot_3d) | if(is.null(options$legend_density_plot_3d)) TRUE else !options$legend_density_plot_3d) FALSE else options$border_legend_density_plot_3d  
  )
  
  fig <- fig %>% addLayout3d(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}

#----------- DOT PLOT 3D -----------
renderScatterPlot3d <- function (values, options){
  data <- get_bin_freq(values$data_info, bins = options$bins_scatter3d, min = values$abs_min, max = values$abs_max)

  fig <- plot_ly(data, x = ~Dados, y = ~Freq, z = ~Classificação,
                 type = 'scatter3d',
                 mode = 'markers',
                 color = ~Classificação,
                 legendgroup = ~Classificação,
                 showlegend = TRUE,
                 symbols = options$shape_markers_histogram3d,
                 name = ~Classificação,
                 marker = list(
                    size = options$size_markers_scatter3d,
                    opacity = options$opacity_markers_scatter3d,
                    line = if(options$line_markers_scatter3d) list(color = 'black', width = 1) else list(width = 0)
                  )
  )

  #Adicionando elipse
  if(options$ellipse_scatter3d) {
    ellipse <- ellipse_data(data, values$names, options$ci_ellipse3d)
    ellipse$z <- 0
    if (options$area_ellipse3d) {
      fig <- fig %>% add_trace(x = ellipse$x, y = ellipse$y, z = ellipse$Classificação,
                               type = 'mesh3d',
                               legendgroup = ellipse$Classificação,
                               name = ellipse$Classificação,
                               color = ellipse$Classificação,
                               colorscale='Viridis',
                               showlegend = FALSE,
                               opacity = options$opacity_area_ellipse3d
      ) }
    if(options$line_ellipse3d) {
      fig <- fig %>% add_trace(x = ellipse$x, y = ellipse$y, z = ellipse$Classificação,
                               type = 'scatter3d', mode = 'lines+markers',
                               legendgroup = ellipse$Classificação,
                               name = ellipse$Classificação,
                               showlegend = FALSE,
                               opacity = options$opacity_line_ellipse3d,
                               color = ellipse$Classificação,
                               line = list(
                                 color = 'black',
                                 width = options$width_line_ellipse3d,
                                 dash = options$ellipse3d_line_format
                               ),
                               marker = list(size = 0.1, color = 'transparent')
      ) }
  }

    layoutConfig <- list(
    bgcolor = if(is.null(options$colors_dot_plot_3d)) options$bgColorPlotly else{
         if(options$colors_dot_plot_3d) options$bgColorPlotly else{
           if(options$bgcolor_dot_plot_3d == 'personal') options$personal_bgcolor_dot_plot_3d
           else options$bgcolor_dot_plot_3d
    }},
    xTitle = if(!is.null(options$axis_x_dot_plot_3d)) options$axis_x_dot_plot_3d else 'Dados',
    yTitle = if(!is.null(options$axis_y_dot_plot_3d)) options$axis_y_dot_plot_3d else 'Classificação',
    zTitle = if(!is.null(options$axis_z_dot_plot_3d)) options$axis_z_dot_plot_3d else 'Frequência',

    legend = if(is.null(options$legend_dot_plot_3d)) TRUE else options$legend_dot_plot_3d,
    legend_title = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) '' else options$title_legend_dot_plot_3d,
    legend_bold = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) FALSE else options$bold_title_legend_dot_plot_3d,
    legend_size = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) 'trace' else options$item_size_legend_dot_plot_3d,
    legend_orientation = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) 'v' else options$orientation_legend_dot_plot_3d,
    legend_border = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) FALSE else options$border_legend_dot_plot_3d  
  )
  
  fig <- fig %>% addLayout3d(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}

#----------- BEE SWARM 3D -----------
renderBeeSwarm3d <- function (values, options){
  bee_swarm <- sapply(values$data_info, function (x) beeswarm(x
                                                              ,method = options$beeswarm3d_method,
                                                              do.plot = FALSE,
                                                              # priority = if(options$beeswarm3d_method == 'swarm' || options$beeswarm3d_method == 'compactswarm') options$priority_beeswarm3d else c("ascending", "descending", "density", "random", "none"),
                                                              side = if(options$side_beeswarm3d) 1 else 0L,
                                                              spacing = options$spacing_markers_beeswarm3d
  ))

  data <- data.frame(Dados = NULL, Classificação = NULL, y = NULL)
  for (i in seq(values$ncol))
    data <- rbind(data, data.frame(Dados = bee_swarm[, i]$x, y = bee_swarm[, i]$y, Classificação = colnames(bee_swarm)[i]))

  fig <- plot_ly(data = data, x = ~Dados, z = ~y, y =~ Classificação,
                 type = 'scatter3d',
                 mode = 'markers',
                 color = ~Classificação,
                 legendgroup = ~Classificação,
                 symbol = if (options$shape_markers_beeswarm3d == 'shapes') ~Classificação else 'circle',
                 symbols = if (options$shape_markers_beeswarm3d == 'shapes') '' else options$shape_markers_beeswarm3d,
                 name = ~Classificação,
                 marker = list(
                   size = options$size_markers_beeswarm3d,
                   opacity = ~options$opacity_markers_beeswarm3d,
                   line = if (options$line_markers_beeswarm3d) { list(color = 'black', width = 1) } else { list(width = 0) }
                 )
  )
  layoutConfig <- list(
    bgcolor = if(is.null(options$colors_dot_plot_3d)) options$bgColorPlotly else{
         if(options$colors_dot_plot_3d) options$bgColorPlotly else{
           if(options$bgcolor_dot_plot_3d == 'personal') options$personal_bgcolor_dot_plot_3d
           else options$bgcolor_dot_plot_3d
    }},
    xTitle = if(!is.null(options$axis_x_dot_plot_3d)) options$axis_x_dot_plot_3d else 'Dados',
    yTitle = if(!is.null(options$axis_y_dot_plot_3d)) options$axis_y_dot_plot_3d else 'Classificação',
    zTitle = if(!is.null(options$axis_z_dot_plot_3d)) options$axis_z_dot_plot_3d else 'Frequência',

    legend = if(is.null(options$legend_dot_plot_3d)) TRUE else options$legend_dot_plot_3d,
    legend_title = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) '' else options$title_legend_dot_plot_3d,
    legend_bold = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) FALSE else options$bold_title_legend_dot_plot_3d,
    legend_size = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) 'trace' else options$item_size_legend_dot_plot_3d,
    legend_orientation = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) 'v' else options$orientation_legend_dot_plot_3d,
    legend_border = if(is.null(options$legend_dot_plot_3d) | if(is.null(options$legend_dot_plot_3d)) TRUE else !options$legend_dot_plot_3d) FALSE else options$border_legend_dot_plot_3d  
  )
  
  fig <- fig %>% addLayout3d(values$usr_title, layoutConfig = layoutConfig)
  return(fig)
}

constructHistogram3d <- function (fig, xstart, xfinesh, espacamento,  height, i, j, name, ncol = 10, opacity = 1){
  fig <- fig %>% add_trace(x = c(xstart + espacamento, xstart + espacamento, xfinesh - espacamento, xfinesh - espacamento, xstart + espacamento, xstart + espacamento, xfinesh - espacamento, xfinesh - espacamento),
                           y = c(0, height, height, 0, 0, height, height, 0),
                           z = c(j, j, j, j, j + 0.5, j + 0.5, j + 0.5, j + 0.5),
                           facecolor = rep(rainbow(ncol)[j], each = 2),
                           name = name,
                           opacity = opacity,
                           showlegend = if(i == 1) TRUE else FALSE,
                           # intensity = c(j, j, j, j, j + 0.1, j + 0.1, j + 0.1, j + 0.1),
                           # colors = colorRamp(c("blue", "lightblue", "chartreuse3", "yellow", "red")),
                           legendgroup = name, type = 'mesh3d')
  return(fig)
}

barHistogram3d <- function (values, options){
  fig <- plot_ly()

  dt <- values$data_info
  brks <- hist(values$c_data_info$Dados, plot = FALSE, breaks = options$algorithm_bar3d)$breaks
  histogram <- sapply(dt, function (x) hist(x, breaks = brks, plot = FALSE ))

  for (j in seq(ncol(histogram)))
    for (i in seq(length(histogram[,1]$counts)))
      fig <- fig %>% constructHistogram3d(histogram[,j]$breaks[i], histogram[,j]$breaks[i + 1], histogram[,j]$counts[i], espacamento = options$spacing_bar_bar3d, i, j, colnames(histogram)[j], ncol = values$ncol, opacity = options$opacity_bar_bar3d)

  fig  <- fig %>% layout(legend = list(itemsizing = 'constant', itemwidth = 30, x = 1, y = 1))
  layoutConfig <- list(
    bgcolor = if(is.null(options$colors_bar_plot_3d)) options$bgColorPlotly else{
         if(options$colors_bar_plot_3d) options$bgColorPlotly else{
           if(options$bgcolor_bar_plot_3d == 'personal') options$personal_bgcolor_bar_plot_3d
           else options$bgcolor_bar_plot_3d
    }},
    xTitle = if(!is.null(options$axis_x_bar_plot_3d)) options$axis_x_bar_plot_3d else 'Dados',
    yTitle = if(!is.null(options$axis_y_bar_plot_3d)) options$axis_y_bar_plot_3d else 'Frequência',
    zTitle = if(!is.null(options$axis_z_bar_plot_3d)) options$axis_z_bar_plot_3d else 'Classificação',

    legend = if(is.null(options$legend_bar_plot_3d)) TRUE else options$legend_bar_plot_3d,
    legend_title = if(is.null(options$legend_bar_plot_3d) | if(is.null(options$legend_bar_plot_3d)) TRUE else !options$legend_bar_plot_3d) '' else options$title_legend_bar_plot_3d,
    legend_bold = if(is.null(options$legend_bar_plot_3d) | if(is.null(options$legend_bar_plot_3d)) TRUE else !options$legend_bar_plot_3d) FALSE else options$bold_title_legend_bar_plot_3d,
    legend_size = if(is.null(options$legend_bar_plot_3d) | if(is.null(options$legend_bar_plot_3d)) TRUE else !options$legend_bar_plot_3d) 'trace' else options$item_size_legend_bar_plot_3d,
    legend_orientation = if(is.null(options$legend_bar_plot_3d) | if(is.null(options$legend_bar_plot_3d)) TRUE else !options$legend_bar_plot_3d) 'v' else options$orientation_legend_bar_plot_3d,
    legend_border = if(is.null(options$legend_bar_plot_3d) | if(is.null(options$legend_bar_plot_3d)) TRUE else !options$legend_bar_plot_3d) FALSE else options$border_legend_bar_plot_3d  
  )
  
  fig <- fig %>% addLayout3d(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}

renderMesh3D <- function (values, options){
  # individual plots
  if(is.null(options$checkbox_mesh))
    return(plot_ly(type = 'scatter', mode = 'markers') %>% layout(xaxis = list(showgrid = FALSE), yaxis = list(showgrid = FALSE)))

  plots <- list()
  for (i in seq(length(options$checkbox_mesh))){
    file <- paste0('Data/Machine Learning/',options$examp_select_mesh,'/',options$examp_select_mesh,'_',options$checkbox_mesh[i],'.txt')
    plots[[i]] <- plot_ly(z = as.matrix(read.table(file))*100000000, type = 'mesh3d', scene = paste0('scene',i)) %>% add_surface()
  }
  # subplot and define scene
  fig <- subplot(plots)

    layoutConfig <- list(
    bgcolor = if(is.null(options$colors_mesh_3d)) options$bgColorPlotly else{
         if(options$colors_mesh_3d) options$bgColorPlotly else{
           if(options$bgcolor_mesh_3d == 'personal') options$personal_bgcolor_mesh_3d
           else options$bgcolor_mesh_3d
    }},
    xTitle = if(!is.null(options$axis_x_mesh_3d)) options$axis_x_mesh_3d else 'Dados',
    yTitle = if(!is.null(options$axis_y_mesh_3d)) options$axis_y_mesh_3d else 'Frequência',
    zTitle = if(!is.null(options$axis_z_mesh_3d)) options$axis_z_mesh_3d else 'Classificação',

    legend = if(is.null(options$legend_mesh_3d)) TRUE else options$legend_mesh_3d,
    legend_title = if(is.null(options$legend_mesh_3d) | if(is.null(options$legend_mesh_3d)) TRUE else !options$legend_mesh_3d) '' else options$title_legend_mesh_3d,
    legend_bold = if(is.null(options$legend_mesh_3d) | if(is.null(options$legend_mesh_3d)) TRUE else !options$legend_mesh_3d) FALSE else options$bold_title_legend_mesh_3d,
    legend_size = if(is.null(options$legend_mesh_3d) | if(is.null(options$legend_mesh_3d)) TRUE else !options$legend_mesh_3d) 'trace' else options$item_size_legend_mesh_3d,
    legend_orientation = if(is.null(options$legend_mesh_3d) | if(is.null(options$legend_mesh_3d)) TRUE else !options$legend_mesh_3d) 'v' else options$orientation_legend_mesh_3d,
    legend_border = if(is.null(options$legend_mesh_3d) | if(is.null(options$legend_mesh_3d)) TRUE else !options$legend_mesh_3d) FALSE else options$border_legend_mesh_3d
  )

  fig <- fig %>% addLayout3d(values$usr_title, layoutConfig = layoutConfig)

  return(fig)
}