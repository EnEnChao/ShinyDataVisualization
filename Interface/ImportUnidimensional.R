import_unidimensional_page <- function (){
  tabPanel('Importe seus dados',
    column(4,
           tabPanel(
             h2(strong("Controle de opções:")),
             #Selecione se deseja utilizar um arquivo existente ou um exemplar
             h3(strong('Insira um arquivo excel já existente ou utilize um exemplar:')),
             selectInput(inputId = 'file_selector_uni',
                         label = 'Escolha a próxima ação:',
                         choices = c(
                           " " = "null",
                           "Importar arquivo .xlsx" = "import",
                           "Usar um arquivo exemplo" = "example"
                         ),
                         selected = "null"
             ),
             #Caso deseje utilizar um arquivo já existente
             conditionalPanel(condition = "input.file_selector_uni == 'import'", {
               #Arquivo a ser carregado
               fileInput(inputId = "file_imported", "Insira o arquivo", accept = '.xlsx')
             }),
             #Caso selecione um exemplo
             conditionalPanel(condition = "input.file_selector_uni == 'example'", {
               selectInput('examp_select',
                           'Escolha os dados de exemplo',
                           choices = c(
                             'Obs. de Laboratorios' = 'labs',
                             'Distribuições aleatórias' = 'distributions',
                             'Votações na flórida' = 'florida',
                             'Construções' = 'construction',
                             'Temperatura - Lincoln' = 'lincoln_temperature',
                             'demografia - Midwest' = 'midwest'
                           ),
                           selected = 'labs'
               ) }),
             #Caso tenha escolhido alguma das opções, aparece o botão grande " Carregue! "
             conditionalPanel(condition = "input.file_selector_uni != 'null'",
                              textInput('title_id_import', 'Digíte o título', 'Título'),
                              actionButton("load_unidimensional",
                                           strong('Carregue!'),
                                           style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                                           width = "80%",
                                           class = "btn-info"
                              )

             )
           ),align = 'center'),
           column(8,
                  column(12, uiOutput('title_name_import'), align = 'center'),
                    uiOutput("table_import_data_output"),
           ),
  column(12,hr())
)
}

insert_unidimensional <- function (){
    tabPanel('Digite seus dados na planilha',
    column(4,
           tabPanel(
             h2(strong("Controle de opções:")),
             #Selecione se deseja utilizar um arquivo existente ou um exemplar
             h3(strong('Digite ou cole na planilha a esquerda:')),
             #Caso tenha escolhido alguma das opções, aparece o botão grande " Carregue! "
             textInput('title_id_insert', 'Digíte o título', 'Título'),
             actionButton("load_spreadsheet",
                          strong('Carregue!'),
                          style = "border-radius: 10px; border-width: 3px; font-size: 20px;",
                          width = "80%",
                          class = "btn-info"
             )
           ),align = 'center'),
           column(8,
                  column(12, uiOutput('title_name_insert'), align = 'center'),
                  shinycssloaders::withSpinner(
                    rHandsontableOutput("user_data", height = '400px'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
                  )
           ),
  column(12,hr())
)
}