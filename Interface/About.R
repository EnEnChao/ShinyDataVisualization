about_page <- function (){
  tabPanel('Sobre', icon = icon('question'),
             # fluidRow(h4('Este protótipo encontra-se em fase experimental, favor aguardar novas atualizações!')),
             column(12,
                    h4(strong('Descrição do Projeto')),
                    h5('O site ', strong('Visualização de Dados do Inmetro'), ' tem como principal objetivo disponibilizar o serviço visualização e a análise de dados para pesquisa.
                    O site permite diversos testes, comparações, visualização de distribuições de diversos tipos de dados, e dados estatísticos.'),
                    h5(strong('Ciência de Dados'),', para nós do Inmetro, é um campo de estudo que se destaca pela capacidade de auxiliar a descoberta de informação útil a partir de grandes ou complexas bases de dados,
                     bem como a tomada de decisão orientada por dados. Pode ser definida como um conjunto de estratégias, ferramentas e técnicas para coleta, transformação e análise de dados realizadas por
                     equipes multidisciplinares formadas por pesquisadores com conhecimento substantivo do problema em análise, estatístico, matemáticos e cientistas da computação.
'),
                    actionButton('tutorial_button', 'Ver artigo com descrição do protótipo'),
                    br(), br(),
                    a('Download Tutorial', href = 'Shiny Data Visualization - Tutorial.pdf', target = "_blank"),
                    h4(strong('Equipe de Desenvolvedores')),
                    hr(),
                    wellPanel(),
                    hr()
             )
    )
}