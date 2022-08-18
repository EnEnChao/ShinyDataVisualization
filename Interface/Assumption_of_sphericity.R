sphericity_page <- function (){
  tabPanel(
    'Avaliando a esfericidade',
    column(3,
      h3(strong("Controle de opções:"), align = 'center'),
    { accordion(
      id = 'accordion_sphericity',
      accordionItem(
        title = 'Testes estatísticos',
        status = accordionStatus,
        collapsed = FALSE
      )
    )}
    ),
    column(9,
           fluidPage(fluidRow(column(9,
                                     h3("Avaliando a esfericidade", style="text-align:center; font-size:50px;"),
           )))
    ),
    column(12,hr())
  )
}