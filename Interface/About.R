about_page <- function (){
  tabPanel('Sobre', icon = icon('question'),
             # fluidRow(h4('Este protótipo encontra-se em fase experimental, favor aguardar novas atualizações!')),
             column(12,
                    h4(strong('Descrição do Projeto')),
                    h5('Este aplicativo de estatística é projetado para ajudar estudantes, professores, pesquisadores e profissionais da área, para visualizar e analisar dados de maneira eficiente e intuitiva.
                     Com ele, é possível importar como arquivos Excel, e transformá-los em gráficos interativos e informativos. Além disso, o aplicativo oferece uma ampla gama de testes estatísticos, incluindo testes t
                     , ANOVA, MANOVA e muito mais, para ajudar a obter insights precisos sobre os dados.',br(),br(),' Com sua interface intuitiva e fácil de usar, o aplicativo é perfeito tanto para usuários avançados quanto para aqueles
                      sem experiência prévia em estatística. Além disso, ele oferece recursos de colaboração, permitindo que você compartilhe suas análises e resultados com colegas e equipes. Em resumo, este aplicativo é uma
                       ferramenta poderosa e acessível para quem precisa visualizar e analisar dados.'),
                    actionButton('tutorial_button', 'Ver tutorial'),
                    br(), br(),
                    a('Download Tutorial', href = 'Shiny Data Visualization - Tutorial.pdf', target = "_blank"),
                    h4(strong('Equipe de Desenvolvedores')),
                    hr(),
                    fluidRow(
                      wellPanel(
                        splitLayout(cellWidths = c("25%", "25%"),
                        HTML('<img src="AndreMiyazawa_profile_picture.jpeg" width=30% height=30%>'),
                        HTML('<img src="WericksonRocha_profile_picture.jpg" width=30% height=30%>'),
                                    HTML(''),
                                    HTML(''),
                        align = 'center'
                      ),splitLayout(cellWidths = c("25%", "25%"),
                        h5(strong('André Miyazawa')),
                        h5(strong('Werickson Rocha')),
                                    p(''),
                                    p(''),
                        align = 'center'
                      ),
                      )
                    ),
                    hr()
             )
    )
}