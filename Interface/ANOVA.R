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
             tabPanel('ANOVA One-Way', h3("ANOVA One-Way", style="text-align:center; font-size:50px;")),
             tabPanel('ANOVA Two-Ways', h3("ANOVA Two-Ways", style="text-align:center; font-size:50px;")),
             tabPanel('ANOVA Three-Ways', h3("ANOVA Three-Ways", style="text-align:center; font-size:50px;"))
           )
    ),
    column(12, hr())
  )
}