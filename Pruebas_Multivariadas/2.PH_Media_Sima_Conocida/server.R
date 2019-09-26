library(shiny)
library(shinyalert)
library(markdown)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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
                             selected=c("Estatura","Peso"))
    
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
  To <- function(base,sigma,M0){
    
    Mmues <- c(unname(colMeans(base))) 
    
    # Constantes 
    n <- nrow(base)
    p <- length(M0)
    
    S <- sigma 
    Scom <- solve(S)
    
    # Estadistico de prueba
    x0 <- n*(t(Mmues-M0))%*%Scom%*%(Mmues-M0)
    
    # Valor p de la prueba
    value <- pchisq(q=x0, df=p, lower.tail=F)
    
    res <- list(estadistico=x0, p=p, n=n, Mmues=Mmues, 
                S=S, df=p, value=value)
  }
  
  #-----------------Pestaña para mostrar resultados QQplot-------------
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
  
  
  #------------Pestaña para mostrar resultados Prueba Royston-----------
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
  
  #-------------Pestaña para mostrar resultados Prueba Mardia------------
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
  
  #----------------------------Pestaña de resultados--------------------  
  #---------------Mostrar distribucion con grados de libertad-----------
  output$titleValorp <- renderText({
    
    vv <- c(input$names)
    inFile <- input$file1
    
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]

    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt[,vv]), nrow=ncol(dt[,vv]), 
                      dimnames=list(names(dt[,vv])), byrow=T) 
    
    ph <- To(base=dt[,vv], sigma=Matrizs, M0=vectorMed)
    paste0('Distribución Chi-cuadrado (', ph$p,")")
    
  })
  
  #-----------------------Mostrar grafico valor p---------------------
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
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt[,vv]), nrow=ncol(dt[,vv]), 
                      dimnames=list(names(dt[,vv])), byrow=T) 
    
    ph <- To(base=dt[,vv], sigma=Matrizs, M0=vectorMed)
    
    shadow.dist(dist='dchisq', param=list(df=ph$p),
                a=as.numeric(ph$estadistico), type='upper', 
                col.shadow='blue', xlim=c(0,10))

  })
  
  #-------------Mostral vector de medias muestrales-----------------------
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
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt[,vv]), nrow=ncol(dt[,vv]), 
                      dimnames=list(names(dt[,vv])), byrow=T) 
    
    
    Mprima <- To(base=dt[,vv], sigma=Matrizs, M0=vectorMed)
    round(Mprima$Mmues, 2)
    
  })
  
  #-------------------Matriz de varianza covarianza muestral---------------
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
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt[,vv]), nrow=ncol(dt[,vv]), 
                      dimnames=list(names(dt[,vv])), byrow=T)
    
    Mprima <- To(base=dt[,vv], sigma=cov(dt),  M0=vectorMed)
    round(Mprima$S, 2)
    
  })
  
  #--------------Mostrar vector de medias ingresado por el usuario---------
  output$med_ho <- renderPrint({
    
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    round(vectorMed,2)
    
  })
  
  #-----Mostrar matriz de varianza covarianza ingresado por el usuario----
  output$matriz_ho <- renderPrint({
    
    inFile <- input$file1
    
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
 
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt[,vv]), nrow=ncol(dt[,vv]), 
                      dimnames=list(names(dt[,vv])), byrow=T)
    round(Matrizs, 2)
    
  })
  
  #-------------Mostrar resultado de la prrueba de hipotesis-------------
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
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt[,vv]), nrow=ncol(dt[,vv]), 
                      dimnames=list(names(dt[,vv])), byrow=T)
    
    ph <- To(base = dt[,vv], sigma=Matrizs, M0=vectorMed)
    paste0('El estadístico de prueba es Xo=', round(ph$estadistico, 2),
            ', con un valor-p=',round(ph$value, 4)," ,
           concluya según el nivel de significancia. ")
    
  })
  
})