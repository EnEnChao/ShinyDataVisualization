about_page <- function (){
  tabPanel('Sobre', icon = icon('question'),
             # fluidRow(h4('Este protótipo encontra-se em fase experimental, favor aguardar novas atualizações!')),
             column(12,
                    h4(strong('Descrição do Projeto')),
                    h5('O aplicativo de visualização de dados do Inmetro tem como principal objetivo a visualização e comparação de dados estatísticos
                    de diversos ambientes, com gráficos e tabelas.'),
                    actionButton('tutorial_button', 'Ver artigo com descrição do protótipo'),
                    hr()
             )
    )
}