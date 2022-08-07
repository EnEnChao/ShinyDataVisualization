import_tridimensional_page <- function (){
  tabPanel('Carregue os dados',
           column(4,
                  h3(strong('Insira um arquivo excel já existente ou utilize um exemplar:')),
                      selectInput(inputId = 'mesh_file_selector',
                                  label = 'Escolha a próxima ação:',
                                  choices = c(
                                    " " = "null",
                                    "Importar arquivo .zip (NÃO IMPLEMENTADO)" = "import",
                                    "Usar um arquivo exemplo" = "example"
                                  ),
                                  selected = "null"
                      ),
                      conditionalPanel('input.mesh_file_selector == "example"',
                                       selectInput('examp_select_mesh',
                                                   'Escolha os dados de exemplo',
                                                   choices = c(
                                                     'zoneA' = 'zoneA',
                                                     'zoneB' = 'zoneB'
                                                   ),
                                                   selected = 'zoneA'
                                       ),
                                       actionButton("load_tridimensional",
                                                    strong('Carregue!'),
                                                    style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                                                    width = "80%",
                                                    class = "btn-info"
                                       )
                      ),align = 'center'
           ),
           column(8,
                        fluidPage(fluidRow(column(
        8,
        tagList(h2(strong('Insira os dados para um gráfico em Mesh:'),
                          uiOutput('mesh_insert_result'),align = 'center'),
                )
      )))
           ),
           column(12,hr())
  )
}