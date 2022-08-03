statistics_unidimensional_page <- function (){
  tabPanel('Informações gerais',
                     fluidPage(
                       column(11,
                                uiOutput('title_name_summary'), align = 'center',
                               tags$head(tags$style(
                                 HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                     background-color: #506f8f !important; }")
                               )),
                               shinycssloaders::withSpinner(
                                 DTOutput("summary_data_table"),
                                             type = spinnerType,
                                             color = spinnerColor,
                                             size = spinnerSize
                               ),
                               br(),
                               h2(strong('Dados estatísticos:')),
                               shinycssloaders::withSpinner(
                                 DTOutput('summary_text'),
                                             type = spinnerType,
                                             color = spinnerColor,
                                             size = spinnerSize
                               )
                      ),
                       column(1,
                              column(3,
                                     materialSwitch(
                                       label = 'Transcrever tabela de dados estatísticos:',
                                       inputId = "transpose_table",
                                       status = switchStatus,
                                       value = FALSE
                                     )
                              )
                       )
                    )
                )
}
