import_bidimensional_page <- function (){
  tabPanel('Carregue seus dados',
    column(4,
           tabPanel(
             h2(strong("Controle de opções:")),
             h3(strong('Insira um arquivo excel já existente ou utilize um exemplar:')),
             selectInput(inputId = 'file_selector_bi',
                         label = 'Escolha a próxima ação:',
                         choices = c(
                           " " = "null",
                           "Importar arquivo .xlsx" = "import",
                           "Usar um arquivo exemplo" = "example"
                         ),
                         selected = "null"
             ),
             conditionalPanel(condition = "input.file_selector_bi == 'import'", {
               #Arquivo a ser carregado
               fileInput(inputId = "file_imported_bi", "Insira o arquivo", accept = '.xlsx')
             }),
             conditionalPanel(condition = "input.file_selector_bi == 'example'", {
               selectInput('examp_select_bi',
                           'Escolha os dados de exemplo',
                           choices = c(
                             'Combustível' = 'gas',
                             'Ansiedade' = 'anxiety'
                           ),
                           selected = 'gas'
               ) }),
             conditionalPanel(condition = "input.file_selector_bi != 'null'",
                              textInput('title_id_import_bi', 'Digíte o título', 'Título'),
                              actionButton("load_bidimensional",
                                           strong('Carregue!'),
                                           style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                                           width = "80%",
                                           class = "btn-info"
                              )

             )
           ),align = 'center'),
           column(8,
                  column(12, uiOutput('title_name_import_bi'), align = 'center'),
                    uiOutput("table_import_bi_data_output"),
           ),
           column(12,hr())
  )
}