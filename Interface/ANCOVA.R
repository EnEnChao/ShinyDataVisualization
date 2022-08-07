ancova_page <- function (){
  tabPanel(
    'ANCOVA',
    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           column(12,
                  accordion(
                    id = 'accordion_ancova',
                    accordionItem(
                      title = 'Configurações do gráfico',
                      status = accordionStatus,
                      collapsed = FALSE,
                      uiOutput('ancova_variables'),
                      numericInput(
                        inputId = 'ancova_ci',
                        label = 'Intervalo de confiança',
                        min = 0, max = 1, value = 0.05
                      )
                    ),
                    accordionItem(
                      title = "Configurações das linhas",
                      status = accordionStatus,
                      collapsed = TRUE,
                      numericInput(
                        'ancova_line_width',
                        'Largura das linhas',
                        min = 0, value = 3
                      )
                    ),
                    accordionItem(
                      title = "Configurações dos pontos",
                      status = accordionStatus,
                      collapsed = TRUE,
                      sliderInput(
                        label = 'Determine a opacidade:',
                        inputId = "ancova_marker_opacity",
                        min = 0.01, max = 1, value = 0.8, step = 0.01
                      ),
                      numericInput(
                        label = 'Determine o tamanho:',
                        inputId = "ancova_marker_size",
                        min = 0, value = 4
                      )
                    )
                  )
           )
    ),
    column(9,
           fluidPage(fluidRow(column(
             h3("ANCOVA - Análise de Covariância", style="text-align:center; font-size:50px;"),
             tabPanel(title = 'Gráfico',
                      shinycssloaders::withSpinner(
                        uiOutput('plotly_ancova'),
                        type = spinnerType,
                        color = spinnerColor,
                        size = spinnerSize
                      ),
                      uiOutput('ancova_statistics')
        ),
        width = 9
      )))
    ),
    column(12, hr())
  )
}