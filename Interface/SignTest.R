sign_test_page <- function (){
    tabPanel('Teste do Sinal',
         column(3,
                h3(strong("Controle do layout:"), align = 'center'),{
           accordion(
             id = 'accordion_sign_test',
             accordionItem(
               title = 'Testes estatísticos',
               status = accordionStatus,
               collapsed = FALSE,
               actionButton("load_sign_test",
                     strong('Carregue!'),
                     style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                     width = "80%",
                     class = "btn-info"
               )
             )
           )
         }

         ),
           column(9,
                  h3('Teste do Sinal', style="text-align:center; font-size:50px;"),
                  br(),
                  uiOutput('sign_test_results'),
                  align = 'center'
           ),
           column(12, hr())
  )
}