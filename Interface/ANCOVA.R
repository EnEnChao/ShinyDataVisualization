ancova_page <- function (){
  tabPanel(
    'ANCOVA',
    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           column(12,
                  accordion(
                    id = 'accordion_ancova',
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
                    ),
                    accordionItem(
                      title = 'Selecione os testes estatísticos',
                      status = accordionStatus,
                      collapsed = TRUE,
                      checkboxGroupButtons(
                         inputId = "Id060",
                         label = "Label",
                         choices =
                           c("Tabela de dados descritivos",
                             "Tabela com covariância",
                             "Tabela de correlação",
                             "Homogeneidade das variâncias",
                             "Pontos fora da curva",
                             "Teste post-hoc"),
                         checkIcon = list(
                            yes = tags$i(class = "fa fa-check-square",
                          style = "color: steelblue"),
                         no = tags$i(class = "fa fa-square-o",
                          style = "color: steelblue"))
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
                        plotlyOutput('plotly_ancova'),
                        type = spinnerType,
                        color = spinnerColor,
                        size = spinnerSize
                      ),
                      uiOutput('ancova_anova_test'),
                      DTOutput('ancova_levene_test'),
                      DTOutput('ancova_levene_test_log'),
        ),
        width = 9
      ))),

    )
  )
}