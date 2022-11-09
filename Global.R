#Versão
version <- '0.7.2'

#Constantes
intervalo_global_de_confianca <- 0.05
intervalo_global_de_confianca_Mbox <- 0.001
significancia_de_aproximacao <- 4

options(encoding = 'UTF-8')
options(warn = -1)

#Margem
margin <- list(
        l = 0,
        r = 0,
        b = 10,
        t = 55
)
#Fonte do título
gFontTit <- list(
    family = "Italic",
    size = 30,
    color = 'dark gray'
)
fontTit <<- gFontTit

#Fonte dos eixos
gFontAxis <- list(
  size = 20
)
fontAxis <<- gFontAxis

#Fonte utilizada no geral
gGenericFont <- list(
    family = "Italic",
    size = 18
)
genericFont <<- gGenericFont

bgColorPlotly <- 'transparent'
legendColorPlotly <- "#E2E2E2"

accordionStatus <- 'primary'
radioGroupStatus <- 'primary'
switchStatus <- 'primary'

spinnerType <- 5
spinnerColor <- 'purple'
spinnerSize <- 0.6
