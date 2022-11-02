manova_page <- function (){
  tabPanel(
    'MANOVA',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
             id = 'accordion_manova',
             accordionItem(
               title = 'Configurações do gráfico',
               status = accordionStatus,
               collapsed = TRUE,
               numericInput(
                 inputId = 'manova_ci',
                 label = 'Intervalo de confiança',
                 min = 0, max = 1, value = 0.05
               ),
               selectInput(
                 inputId = 'manova_sumsq',
                 label = 'Algoritmo para soma dos quadrados',
                 choices = c(2, 3),
                 selected = 2
               )
             )
           )}
           )
    ),
        column(9,
           fluidPage(fluidRow(column(
             h3("MANOVA - Análise de Variância Multivariada", style="text-align:center; font-size:50px;"),
             tabPanel(title = 'Gráfico',
                      uiOutput('manova_statistics')
        ),
        width = 9
      )))
    ),
    column(12, hr())
  )
}