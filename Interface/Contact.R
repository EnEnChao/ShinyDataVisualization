contact_page <- function (){
  tabPanel(
    'Contato', icon = icon('glyphicon glyphicon-envelope', lib = "glyphicon"),
    column(12,
           hr(),
           p(strong('Críticas'), ' e ', strong('sugestões'),' são bem-vindas! Entre em contato com a equipe de Visualização de Dados do Inmetro, pelo endereço a seguir ou preenchendo os campos do formulário:'),
           hr(),
           column(4,
                  textInput('usr_email', 'E-mail'),
                  textInput('usr_email', 'Nome'),
                  textInput('usr_email', 'Telefone'),
                  textInput('usr_email', 'Assunto'),
                  textAreaInput('usr_email', 'Mensagem', rows = 7),
                  actionButton('send_contact', 'Enviar')
           ),
           column(8,
                  h3(strong('Informações de contato: ')),br(),
                  h4(strong('Instituto Nacional de Metrologia, Qualidade e Tecnologia (Inmetro)')),
                  h4(strong('Diretoria de Metrologia Científica e Industrial (Dimci)')),br(),
                  p(strong('Endereço:'),'Av. N. Sra. das Graças, 50 – Xerém – Duque de Caxias – Rio de Janeiro – Brasil',br(),
                    strong('Telefones:'),'(21) 2679-9011/9787',br(),
                    strong('Email:'), a('dimci@inmetro.gov.br'))
           ),
           column(12, hr())

    )

  )
}