library(shiny)
library(shinyalert)
library(MVN)
library(markdown)
library(car)
source("grafico.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #Comando para seleccionar la base o base por defecto
  observe({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header,
                        sep=input$sep)
    #Comando para seleccionar los nombres de las columnas 
    updateCheckboxGroupInput(session, "names",
                             choices=colnames(dt[,]),
                             selected=c("x1","x2","x3","x4"))
    
  })
  
  #---------------------#Pestaña para mostrar datos-------------------------- 
  output$inputData <- renderTable({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    dt
  })
  
  #---------------------#Funcion y estadistico dde prueba--------------------
  To <- function(base,C0){
    
    #Vector de medias de la poblacion
    Mmues <- c(unname(colMeans(base)))
    
    #Constantes necesarias
    n <- nrow(base)
    k <- nrow(C0)
    
    #Matriz de varianza muestral
    sigma <- cov(base)
    S <- C0%*%sigma%*%t(C0) 
    Scom <- solve(S)
    
    #obtencion del estadistico de prueba
    x0 <- n*t(C0%*%Mmues)%*%Scom%*%(C0%*%Mmues)
    
    a <- ((n-1)*k)/(n-k)
    
    #Valor p de la prueba
    value <- pf(q=x0/a, df1=k, df2=n-k, lower.tail=F)
    res <- list(estadistico=x0, 
                n=n,
                k=k,
                Mmues=Mmues, 
                S=S,
                df1=k,
                df2=n-k,
                a=a,
                value=value)
    
    return(res)
  }
  
  #-----------------------#Pestaña para mostrar resultados-------------------
  #------------------------------#Mostrar qqplot-----------------------------
  output$qqplot <- renderPlot({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    dist <- mahalanobis(dt[,vv],
                        center=colMeans(dt[,vv]),
                        cov=var(dt[,vv]))
    qqPlot(dist, dist="chisq", df=length(dt[,vv]), 
           pch=19,las=1,
           ylab="Distancias de Mahalanobis",
           xlab="Cuantiles de una chi-cuadrada con p grados de libertad")
    
    grid()
  })
  
  #----------Pestaña para mostrar mensaje de alerta Prueba Royston-----------
  
  output$roystonm <- renderPrint({
    
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    valorqq <- mvn(data=dt[,vv], mvnTest="royston", multivariatePlot="qq")
    
    aa <- valorqq[["multivariateNormality"]]$MVN
    
    if (aa=="NO"){
      shinyalert(
        title = "Cuidado",
        text = "No se cumple supuesto de normalidad multivariada",
        closeOnEsc = F,
        closeOnClickOutside = F,
        html = F,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = F,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
    }else {
      shinyalert(
        title = "Proceda",
        text = "Se cumple supuesto de normalidad",
        closeOnEsc = F,
        closeOnClickOutside = F,
        html = F,
        type = "success",
        showConfirmButton = T,
        showCancelButton = F,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = F
      )
      
    }
    
  })
  
  #------------------------------#Prueba royston-----------------------------
  output$royston <- renderPrint({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    valorqq <- mvn(data=dt[,vv], mvnTest="royston", multivariatePlot="qq")
    valorqq$multivariateNormality
  })
  
  #--------------------------------#Prueba mardia-----------------------------
  output$mardia <- renderPrint({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    valorqq <- mvn(data=dt[,vv], mvnTest="mardia", multivariatePlot="qq")
    valorqq$multivariateNormality
  })
  
  #-----------------------------#Pestaña de resultados------------------------
  #---------------------------#mostrar grados de libertad---------------------
  output$titleValorp <- renderText({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    #Ingresar matriz de contrastes
    contrasteMed <- input$contrasteIng
    contrasteMed <- as.numeric(unlist(strsplit(contrasteMed, "[\n, \t]")))
    contrasteMed <- contrasteMed[!is.na(contrasteMed)]
    contrasteMed <- matrix(contrasteMed, ncol=ncol(dt[,vv]), 
                           nrow=length(contrasteMed)/ncol(dt[,vv]), 
                           byrow=T)
    
    ph <- To(base=dt[,vv], C0=contrasteMed)
    paste0('Distribución F (', ph$df1,",", ph$df2,")")
  })
  
  #--------------------------------#Mostrar grafico-------------------------
  output$grafico1 <- renderPlot({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    contrasteMed <- input$contrasteIng
    contrasteMed <- as.numeric(unlist(strsplit(contrasteMed, "[\n, \t]")))
    contrasteMed <- contrasteMed[!is.na(contrasteMed)]
    contrasteMed <- matrix(contrasteMed, ncol=ncol(dt[,vv]), 
                           nrow=length(contrasteMed)/ncol(dt[,vv]), 
                           byrow=T)
    
    ph <- To(base=dt[,vv], C0=contrasteMed)
    
    
    shadow.dist(dist='df', param=list(df1=ph$df1, df2=ph$df2),
                a=(as.numeric(ph$estadistico)/ph$a), type='upper', 
                col.shadow='blue', xlim=c(0,10))
    
  })
  
  #---------------------#Mostral vector de medias muestrales-----------------
  output$med_muestra <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    
    contrasteMed <- input$contrasteIng
    contrasteMed <- as.numeric(unlist(strsplit(contrasteMed, "[\n, \t]")))
    contrasteMed <- contrasteMed[!is.na(contrasteMed)]
    contrasteMed <- matrix(contrasteMed, ncol=ncol(dt[,vv]), 
                           nrow=length(contrasteMed)/ncol(dt[,vv]), 
                           byrow=T)
    
    Mprima <- To(base=dt[,vv], C0=contrasteMed)
    round(Mprima$Mmues, 2)
  })
  
  #-----------------#Mostrar contrastes ingresado por el usuario-------------
  output$contraste_ho <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    
    contrasteMed <- input$contrasteIng
    contrasteMed <- as.numeric(unlist(strsplit(contrasteMed, "[\n, \t]")))
    contrasteMed <- contrasteMed[!is.na(contrasteMed)]
    contrasteMed <- matrix(contrasteMed, ncol=ncol(dt[,vv]), 
                           nrow=length(contrasteMed)/ncol(dt[,vv]), 
                           byrow=T)
    contrasteMed
  })
  
  #--------------#Matriz de varianza covarianza de contraste---------------
  output$contraste_muestra <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    
    contrasteMed <- input$contrasteIng
    contrasteMed <- as.numeric(unlist(strsplit(contrasteMed, "[\n, \t]")))
    contrasteMed <- contrasteMed[!is.na(contrasteMed)]
    contrasteMed <- matrix(contrasteMed, ncol=ncol(dt[,vv]), 
                           nrow=length(contrasteMed)/ncol(dt[,vv]), 
                           byrow=T)
    
    Mprima <- To(base=dt[,vv], C0=contrasteMed)
    round(Mprima$S, 2)
  })
  
  
  #-------------#Mostrar resultado de la prueba de hipotesis----------------
  output$resul1<- renderText({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    contrasteMed <- input$contrasteIng
    contrasteMed <- as.numeric(unlist(strsplit(contrasteMed, "[\n, \t]")))
    contrasteMed <- contrasteMed[!is.na(contrasteMed)]
    contrasteMed <- matrix(contrasteMed, ncol=ncol(dt[,vv]), 
                           nrow=length(contrasteMed)/ncol(dt[,vv]), 
                           byrow=T)
    
    ph <- To(base = dt[,vv], C0=contrasteMed)
    paste0('El estadístico de prueba es To=', round(ph$estadistico, 2),
           ', con un valor-p=',round(ph$value, 4),
           " ,concluya según el nivel de significancia. ")
  })
  
})