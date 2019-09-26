library(shiny)
library(shinyalert)
library(MVN)
library(markdown)
library(car)
source("grafico.R")

shinyServer(function(input,output,session){

  #----------Comando para seleccionar la base o base por defecto--------
  observe({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header,
                        sep=input$sep)
    
    #-------Comando para seleccionar los nombres de las columnas--------
   updateCheckboxGroupInput(session, "names",
                             choices=colnames(dt[,]),
                             selected=c("x1","x2","x3"))
                             
  })
  
  #----------------------Pestaña para mostrar datos------------------- 
  output$inputData <- renderTable({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    dt
  })
  
  #--------------------Función Estadistico de Prueba-------------------
  Tamano <- function(base,M0){
    n <- nrow(base)
    
    # Condición para comprobar tamaño de muestra
    if (n<=30){
      Mmues <- c(unname(colMeans(base))) 
      
      S <- cov(base) 
      Scom <- solve(S)
      p <- length(M0) 
      #Constante de correcion
      a <-((n-1)*p/(n-p))
      
      # Estadistico de prueba
      T0 <- n*(t(Mmues-M0))%*%Scom%*%(Mmues-M0)
      
      # Valor p de la prueba
      value <- pf(q=T0/a, df1=p, df2=n-p, lower.tail=F)
      res <- list(estadistico=T0, p=p, n=n, Mmues=Mmues, 
                  S=S, a=a, df1=p, df2=n-p, value=value)
    }
    
    else {
      Mmues <- c(unname(colMeans(base))) 
      
      S <- cov(base) 
      Scom <- solve(S)
      p <- length(M0) 
      
      # Estadistico de prueba
      x0 <- n*(t(Mmues-M0))%*%Scom%*%(Mmues-M0)
      
      # Valor p de la prueba
      value <- pchisq(q=x0, df=p, lower.tail=F)
      res <- list(estadistico=x0, p=p, n=n, Mmues=Mmues, 
                  S=S, df=p, value=value)
    }
  }
  
  #-----------Pestaña para mostrar media muestral-----------------
  output$med_muestra <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    
    Mprima <- Tamano(base=dt[,vv], M0=vectorMed)
    round(Mprima$Mmues, 2)
    
    })
  
  output$med_ho <- renderPrint({
    
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    round(vectorMed,2)
    
    })
  
  output$S_muestra <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    
    Mprima <- Tamano(base = dt[,vv], M0=vectorMed)
    round(Mprima$S, 2)
    })

  
  #-----------------Pestaña para mostrar resultados QQplot-------------
  output$grafico1 <- renderPlot({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    ph <- Tamano(base = dt[,vv], M0=vectorMed)
    
    if (ph$n<=30){
      
      shadow.dist(dist='df', param=list(df1=ph$df1, df2=ph$df2),
                  a=(as.numeric(ph$estadistico)/ph$a), type='upper', 
                  col.shadow='blue', xlim=c(0,10))
    }
    else{
      
      shadow.dist(dist='dchisq', param=list(df=ph$p),
                  a=as.numeric(ph$estadistico), type='upper', 
                  col.shadow='blue')
    }
    
  })
  
  output$titleValorp<- renderText({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    
    ph <- Tamano(base = dt[,vv], M0=vectorMed)
    
    if (ph$n<=30){
      paste0('Distribución F (', ph$p,",",ph$n-ph$p,")")
    }
    else{
      paste0('Distribución Chi-cuadrado (', ph$p,")")
    }
      
  })
  
  output$resul1<- renderText({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    
    ph <- Tamano(base = dt[,vv], M0=vectorMed)
    
    if (ph$n<=30){
      paste0('El estadístico de prueba es To =', round(ph$estadistico, 2),
             ', con un valor-p = ',round(ph$value, 4),
             " ,concluya según el nivel de significancia. ")
    }
    else {
      paste0('El estadístico de prueba es Xo=', round(ph$estadistico, 2),
             ', con un valor-p=',round(ph$value, 4),
             " ,concluya según el nivel de significancia.")
    }    
  })
  
  output$qqplot <- renderPlot({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    require(car)
    dist<-mahalanobis(dt[,vv],
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
  
  #----------Pestaña para mostrar Prueba Royston-----------
  output$royston <- renderPrint({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    # Para sacar los NA de la variable
    
    require(MVN)
      valorqq<-mvn(data= dt[,vv],mvnTest = "royston", multivariatePlot= "qq")
      valorqq$multivariateNormality
  })
  
  output$mardia <- renderPrint({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    valorqq<-mvn(data= dt[,vv],mvnTest = "mardia", multivariatePlot= "qq")
    valorqq$multivariateNormality
  })
  

})
