transform_norm_page <- function (){
  tabPanel(
    'Transformando os dados para normalidade',
    column(3,
           h3(strong("Controle do layout:"), align = 'center'),
                      { accordion(
             id = 'accordion_transform_norm',
             accordionItem(
               title = 'Testes estatísticos',
               status = accordionStatus,
               collapsed = FALSE,
               selectInput(
                 'transform_norm_distributions',
                 'Escolha a distribuição: ',
                 choices = c(
                   'Nenhuma' = 'none',
                   'sqrt(x)' = 'sqrt',
                   'log10(x)' = 'log10',
                   # 'Numeric' = 'logy',
                   '1/x' = '1/x'
                 ),
                 selected = 'none'
               ),
               conditionalPanel(condition = 'input.transform_norm_distributions == "logy"',
                                numericInput('transform_norm_distributions_logy', 'Escolha um número para multiplicar o dataframe: ',
                                             value = 10, step = 1
                                )

               ),
               conditionalPanel(condition = 'input.transform_norm_distributions != "none"',
                                selectInput(
                                  'transform_norm_distributions_skewed',
                                  'Tendência das medidas: ',
                                  choices = c(
                                    'Dados positivamente distorcidos' = TRUE,
                                    'Dados negativamente distorcidos' = FALSE
                                  ),
                                  selected = TRUE
                                )
               ),
               actionButton("load_transform_norm",
                     strong('Carregue!'),
                     style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                     width = "80%",
                     class = "btn-info"
               )
             )
           )}
    ),
    column(9,
           h3("Transformando em Normalidade", style="text-align:center; font-size:50px;"),
           br(),
           h3('Sem nenhuma transformação'),
           shinycssloaders::withSpinner(
             plotlyOutput('transform_norm_results_original'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
           ),
           uiOutput('transform_norm_results_new'),
           uiOutput('transform_norm_results_method_statistics'),
           uiOutput('transform_norm_download'),
           align = 'center'
    ),
    column(12, hr())
  )
}