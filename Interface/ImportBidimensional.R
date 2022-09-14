import_bidimensional_page <- function (){
  tabPanel('Importe seus dados',
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
             conditionalPanel(condition = "input.file_selector_bi == 'import'",
               #Arquivo a ser carregado
               fileInput(inputId = "file_imported_bi", "Insira o arquivo", accept = '.xlsx'),
               switchInput(inputId = 'imported_bi_as.factor', offLabel = 'Categorias', onLabel = 'Fatores', value = TRUE)
             ),
             conditionalPanel(condition = "input.file_selector_bi == 'example'", {
               selectInput('examp_select_bi',
                           'Escolha os dados de exemplo',
                           choices = c(
                             'Combustível' = 'gas',
                             'Ansiedade' = 'anxiety',
                             'Escolaridade' = 'escolaridade'
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

insert_bidimensional <- function (){
    tabPanel('Digite seus dados na planilha',
    column(4,
           tabPanel(
             h2(strong("Controle de opções:")),
             #Selecione se deseja utilizar um arquivo existente ou um exemplar
             h3(strong('Digite ou cole na planilha a esquerda:')),
             #Caso tenha escolhido alguma das opções, aparece o botão grande " Carregue! "
             textInput('title_id_insert_bi', 'Digíte o título', 'Título'),
             switchInput(inputId = 'inserted_bi_as.factor', offLabel = 'Categorias', onLabel = 'Fatores', value = TRUE),
             actionButton("load_spreadsheet_bi",
                          strong('Carregue!'),
                          style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                          width = "80%",
                          class = "btn-info"
             )
           ),align = 'center'),
           column(8,
                  column(12, uiOutput('title_name_insert_bi'), align = 'center'),
                  shinycssloaders::withSpinner(
                    rHandsontableOutput("user_data_bi", height = '400px'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  )

           ),
  column(12,hr())
)
}