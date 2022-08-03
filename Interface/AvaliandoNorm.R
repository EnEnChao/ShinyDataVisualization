check_norm_page <- function (){
  tabPanel(
    'Distribuição de dados Normais',
    column(12,
           fluidPage(fluidRow(column(
             tabsetPanel(id = 'check_norm_tabs', type = 'tabs',
               tabPanel(id = 'check_norm_d', value = 'check_norm_d',title = 'Gráfico de densidade',
                        h3("Gráfico de densidade", style="text-align:center; font-size:50px;"),
                        shinycssloaders::withSpinner(
                          plotlyOutput('plotly_norm_density'),
                          type = spinnerType,
                          color = spinnerColor,
                          size = spinnerSize
                        )
               ),
               tabPanel(id = 'check_norm_qq', value = 'check_norm_qq',title = 'Gráfico quantile-quantile',
                        h3("Gráfico quantile-quantile", style="text-align:center; font-size:50px;"),
                        shinycssloaders::withSpinner(
                          plotlyOutput('plotly_norm_qq'),
                          type = spinnerType,
                          color = spinnerColor,
                          size = spinnerSize
                        )
               ),
               tabPanel(title = 'Tabela de verificação de normalidade',
                        h3("Tabela de verificação de normalidade", style="text-align:center; font-size:50px;"),
                      shinycssloaders::withSpinner(
                        DTOutput('check_norm_table'),
                        type = spinnerType,
                        color = spinnerColor,
                        size = spinnerSize
                      )
               )
             ),
             width = 12
           )))
           ),
    column(12,hr())
  )
}