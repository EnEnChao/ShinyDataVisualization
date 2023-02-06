home_page <- function (){
  tabPanel('Home', icon = icon('home'),
           # h2('Home', align = 'center'),
           br(),
           shinycssloaders::withSpinner(
                    slickROutput("home_images", width="1000px", height = '350px'),
                    type = spinnerType,
                    color = spinnerColor,
                    size = spinnerSize
           ),
           br(),
             # HTML('<center><img src="HomeImages/a-imagem-destaca-grafico-setores-grafico-linhas-grafico-barras.png" width=40% height=40%></center>'),
           h2(strong('Informações disponíveis'), align = 'center'),
           hr(),
           wellPanel(
             p(
               column(4,
                      h4(strong('Apresentação de gráficos')),
                      HTML('<img src="bar-chart.png" width=20% height=20%>')
               ),
               column(4,
                      h4(strong('Comparação de dados')),
                      HTML('<img src="statistic.png" width=20% height=20%>'),
               ),
               column(4,
                      h4(strong('Distribuições e testes')),
                      HTML('<img src="statistics2.png" width=20% height=20%>')
               )
             ),
             br(),br(),br(),br(),br(),br(),
             align = 'center'
           ),
           hr()
  )
}