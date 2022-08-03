mesh_page <- function (){
  tabPanel(
    'Gráfico em Mesh',
    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           h4('Escolha os gráficos a serem mostrados: ', align = 'center'),
           checkboxGroupButtons(
             inputId = "checkbox_mesh",
             label = '',
             choices = seq(20),
             checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                              no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
           )
    ),
    column(9,
           fluidPage(fluidRow(column(
             h2("Gráfico em Mesh 3D", style="text-align:center; font-size:50px;"),
                         tabPanel(title = 'Gráfico',
                                  shinycssloaders::withSpinner(
                                    plotlyOutput('plotly_mesh3d'),
                                    type = spinnerType,
                                    color = spinnerColor,
                                    size = spinnerSize
                                  )
             ),
             width = 9
      )))
    ),
    column(12,hr())
  )
}