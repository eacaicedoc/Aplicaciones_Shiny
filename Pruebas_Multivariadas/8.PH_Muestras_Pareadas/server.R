library(shiny)
library(shinyalert)
library(MVN)
library(markdown)
library(car)
source("grafico.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #------------Comando para seleccionar la base o base por defecto--------
  observe({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header,
                        sep=input$sep)
    
    #-------------Comando para seleccionar los nombres de las columnas 
    updateCheckboxGroupInput(session, "names",
                             choices=colnames(dt[,]),
                             selected=c("x1","x2","y1","y2"))
    
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
  To <- function(base){
    
    if(ncol(base)>=4){
      #Numero de observaciones
      n <- nrow(base)
      #Numero de muestras 
      d <- ncol(base)/2
      
      #Creacion de vectores diferencia
      diferencias <- vector()
      for (i in 1:d) {
        diferencias[i]=base[i]-base[d+i]
      }
      
      diferencias <- data.frame(matrix(unlist(diferencias), nrow=nrow(base), 
                                       byrow=F),
                                stringsAsFactors=FALSE)
      
      colnames(diferencias) <- paste("dif", 1:ncol(diferencias))
      
      #Medias de diferencias
      Medias <- as.vector(apply(diferencias, MARGIN=2, FUN=mean))
      p <- length(Medias)
      
      #Matirz varianza covarianza de difrencias
      S <- cov(diferencias) 
      Scom <- solve(S)
      
      #Estadistico de prueba
      x0 <- n*t(Medias)%*%Scom%*%Medias
      
      #Constante de correcion
      a <- ((n-1)*p)/(n-p)
      
      #Valor p de la prueba
      value <- pf(q=x0/a, df1=p, df2=n-p, lower.tail=F)
      res <- list(estadistico=x0, n=n, diferencias=diferencias,
                  Medias=Medias, S=S, df1=p, df2=n-p, a=a, value=value)
    }
    else{
      shinyalert(
        title = "Cuidado",
        text = "La base de datos debe tener cuatro o mas variables",
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
    }
    
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
    
    ph <- To(base=dt[,vv])
    paste0('Distribución F (', ph$df1, ",", ph$df2,")")
  })
  
  #--------------------------------#Mostrar grafico-------------------------
  output$grafico1 <- renderPlot({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    ph <- To(base=dt[,vv])
    
    shadow.dist(dist='df', param=list(df1=ph$df1, df2=ph$df2),
                a=(as.numeric(ph$estadistico)/ph$a), type='upper', 
                col.shadow='blue', xlim=c(0,10))
    
  })
  
  #---------------------#Mostral vector de diferencias muestrales-----------
  output$med_diferencias <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    
    Mprima <- To(base=dt[,vv])
    round(Mprima$diferencias, 2)
  })
  
  #---------------------#Mostral vector de medias muestrales-----------------
  output$med_muestra <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    
    Mprima <- To(base=dt[,vv])
    round(Mprima$Medias, 2)
  })
  
  #-------------------#Matriz de varianza covarianza muestral-----------------
  output$S_muestra <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    
    Mprima <- To(base=dt[,vv])
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
    
    ph <- To(base=dt[,vv])
    paste0('El estadístico de prueba es To=', round(ph$estadistico, 2),
           ', con un valor-p=',round(ph$value, 4),
           " ,concluya según el nivel de significancia. ")
  })
  
})