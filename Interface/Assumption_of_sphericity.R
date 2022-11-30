sphericity_page <- function (){
  tabPanel(
    'Avaliando a esfericidade',
    column(12,
           h3("Avaliando a esfericidade", style="text-align:center; font-size:50px;"),
           h3(strong('Teste de Esfericidade de Mauchly')),
           shinycssloaders::withSpinner(
             DTOutput('mauchly_test'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
            ),
           h3(strong('Resultados:')),
           uiOutput('sphericity_statistics'),
           br(),
           h3(strong('Correções de esfericidade')),
           fluidRow(
             column(6, h4('Correção Greenhouse-Geisser', align = 'center'),
                    shinycssloaders::withSpinner(
                      DTOutput('sphericity_corrections_gg'),
                      type = spinnerType,
                      color = spinnerColor,
                      size = spinnerSize
                    )
             ),
             column(6, h4('Correção Huynh-Feldt', align = 'center'),
                    shinycssloaders::withSpinner(
                      DTOutput('sphericity_corrections_hf'),
                      type = spinnerType,
                      color = spinnerColor,
                      size = spinnerSize
                    )
             )
           )
           , align = 'center'
    ),
    column(12,hr())
  )
}