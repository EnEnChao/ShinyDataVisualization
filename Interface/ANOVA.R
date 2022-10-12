anova_page <- function (){
  tabPanel(
    'ANOVA',
    column(3,
           column(12,
             h3(strong("Controle de opções:"), align = 'center'),
                  { accordion(
                    id = 'accordion_anova',
                    accordionItem(
                      title = 'Variáveis',
                      status = accordionStatus,
                      collapsed = FALSE,
                      uiOutput('anova_variables')
                    )
                  ) }
           )
    ),
    column(9,
           tabsetPanel(

           )
    ),
    column(12, hr())
  )
}