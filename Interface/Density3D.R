density3D_page <- function (){
  tabPanel(
    'Gráfico de Densidade 3D',
        column(3,
           h3(strong("Controle de opções:"), align = 'center'),
               column(12,
                      accordion(
                        id = 'accordion_density_plot',
                        accordionItem(
                          title = "Configurações do gráfico de densidade",
                          status = accordionStatus,
                          collapsed = TRUE,
                          materialSwitch(
                            label = 'Mostrar Área:',
                            inputId = "area_density_plot",
                            status = switchStatus,
                            value = TRUE
                          ),
                          materialSwitch(
                            label = 'Mostrar linhas:',
                            inputId = "line_density_plot",
                            status = switchStatus,
                            value = TRUE
                          )
                        ),
                        accordionItem(
                          title = "Estimativa de densidade kernel",
                          status = accordionStatus,
                          collapsed = TRUE,
                          selectInput(
                            inputId = 'algorithm_density_plot',
                            label = 'Escolha a estimativa de densidade kernel: ',
                            choices = c(
                              'Gaussiana' = 'gaussian',
                              'Retangular' = 'rectangular',
                              'Triangular' = 'triangular',
                              'Epanechnikov' = 'epanechnikov',
                              'Biweight' = 'biweight',
                              'Cosseno' = 'cosine',
                              'Optcosseno' = 'optcosine'
                            ),
                            selected = 'gaussian'
                          )
                        )
                      )
               )
        ),
    column(9,
           fluidPage(fluidRow(column(
             h3("Density Plot 3D", style="text-align:center; font-size:50px;"),
             tabPanel(title = 'Gráfico',
                      shinycssloaders::withSpinner(
                        plotlyOutput('plotly_density3d'),
                        type = spinnerType,
                        color = spinnerColor,
                        size = spinnerSize
                      )
        ),
        width = 9
      ))),
    ),
    column(12, hr())
  )
}