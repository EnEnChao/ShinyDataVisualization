homogenity_var_page <- function (){
  tabPanel(
    title = 'Homogeneidade das variâncias',
    column(3,
           h3(strong("Controle de opções:"), align = 'center'),
           { accordion(
             id = 'accordion_homogenity',
             accordionItem(
               title = 'Testes estatísticos',
               status = accordionStatus,
               collapsed = FALSE,
               selectInput(
                 'homogenity_tests',
                 'Escolha o teste: ',
                 choices = c(
                   'Teste de Levene' = 'levene_test',
                   'Teste F' = 'f_test',
                   'Teste de Bartlett' = 'bartlett_test',
                   'Teste de Fligner-Killeen' = 'fk_test'
                 ),
                 selected = 'levene_test'
               )
             )
           )}
    ),
    column(9,
           h3("Homogeneidade das Variâncias", style="text-align:center; font-size:50px;"),
           br(),
           uiOutput('homogenity_method_name'),
           shinycssloaders::withSpinner(
             DTOutput('homogenity_table'),
             type = spinnerType,
             color = spinnerColor,
             size = spinnerSize
           ),
           uiOutput('homogenity_method_results'),
           align = 'center'
    ),
    column(12, hr())
  )
}