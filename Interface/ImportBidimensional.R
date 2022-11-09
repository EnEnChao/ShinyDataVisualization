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
                awesomeRadio(inputId = 'imported_bi_type', label = "Escolha o tipo de teste",
                             choices = c('Uma coluna' = 'uni_data', 'Comparando duas Médias' = 'two_col', 'ANOVA' = 'anova', 'ANOVA dois grupos' = 'anova_2groups' , 'ANCOVA' = 'ancova', 'MANOVA' = 'manova'),
                             inline = FALSE, status = "success"),
               # switchInput(inputId = 'imported_bi_as.factor', offLabel = 'Categorias', onLabel = 'Fatores', value = TRUE)
             ),
             conditionalPanel(condition = "input.file_selector_bi == 'example'", {
               pickerInput('examp_select_bi',
                           'Escolha os dados de exemplo',
                           choices = list(
                             `Uma Variável` = c(
                               'Comprimento da Sépala' = 'Sepal.Lenght',
                               'Quantidade de Resíduos' = 'waste'
                             ),
                             `Duas Médias` = c(
                               'Ratos' = 'mice',
                               'Peso entre gêneros' = 'genderweight'
                             ),
                             `ANOVA` = c(
                               'Combustível' = 'gas2',
                               'Crescimento de Plantas' = 'PlantGrowth'
                             ),
                             `ANOVA medidas repetidas ou misturadas` = c(
                               'Auto estima' = 'selfesteem',
                               'Atuação' = 'performance'
                             ),
                             `ANCOVA` = c(
                               'Combustível' = 'gas3',
                               'Ansiedade' = 'anxiety'
                             ),
                             `MANOVA` = c(
                               'Iris' = 'iris_manova'
                             )
                           ),
                           selected = 'gas'
               )
             }),
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
             awesomeRadio(inputId = 'inserted_bi_type', label = "Escolha o tipo de teste",
                             choices = c("Comparando duas Médias", "ANOVA", "ANOVA dois grupos", 'ANCOVA', 'MANOVA'),
                             inline = TRUE, status = "success"),
             #Caso tenha escolhido alguma das opções, aparece o botão grande " Carregue! "
             textInput('title_id_insert_bi', 'Digíte o título', 'Título'),
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