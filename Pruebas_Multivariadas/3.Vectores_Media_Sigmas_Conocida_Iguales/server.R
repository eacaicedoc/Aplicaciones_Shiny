library(shiny)
library(shinyalert)
library(MVN)
library(markdown)
library(car)
source("grafico.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #----------Comando para seleccionar la base o base por defecto-------------
  observe({
    #------------------------ Comando selección base 1-------------------
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1,
                         sep=input$sep1)
    #--------------------Seleccionar los nombres de las variables base 1------------ 
    updateCheckboxGroupInput(session, "names1",
                             choices=colnames(dt1[,]),
                             selected=c("x","y","z"))
    
    #------------------------ Comando selección base 2-------------------
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2,
                         sep=input$sep2)
    #----------------Seleccionar los nombres de las variables base 2------------ 
    updateCheckboxGroupInput(session, "names2",
                             choices=colnames(dt2[,]),
                             selected=c("x","y","z"))
    
  })

#----------------------Pestaña para mostrar datos----------------------------------- 
        #-----------------------mostrar datos1----------------------------
  output$inputData1 <- renderTable({
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    dt1
  })
        #-----------------------mostrar datos2----------------------------
  output$inputData2 <- renderTable({
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    dt2
  })  
  
#-------------Función Estadistico de Prueba------------------------------   
  Xo <- function(base1,base2,sigma){
    
    #Medias muestrales 
    Mm1 <- c(unname(colMeans(base1))) 
    Mm2 <- c(unname(colMeans(base2))) 
    
    #n y m tamaños poblacion
    n <- nrow(base1)
    m <- nrow(base2)
    p <- length(Mm1)
    
    #Matriz ingresada por usuario
    S <- sigma
    Scom <- solve(sigma)
    
    N <-((n*m)/(n+m))
    
    #obtención del estadístico de prueba
    X0 <- N*(t(Mm1-Mm2))%*%Scom%*%(Mm1-Mm2)
    
    #Valor p de la prueba
    value <- pchisq(q=X0, df=p, lower.tail=F)
    
    res <- list(estadistico=X0, p=p, n=n, m=m, Mm1=Mm1, Mm2=Mm2,
                S=S, value=value, Scom=Scom, N=N)
  }
  
  #-----------------Pestaña para mostrar resultados QQplot--------------------
  output$qqplot <- renderPlot({
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    par(mfrow=c(1, 2), bg='gray98')
    
    
    dist1 <- mahalanobis(dt1[,vv1], center=colMeans(dt1[,vv1]),
                         cov=var(dt1[,vv1]))
    
    qqPlot(dist1, dist="chisq", df=length(dt1[,vv1]), 
           pch=19,las=1,
           ylab="Distancias de Mahalanobis",
           xlab="Cuantiles de una chi-cuadrada con p grados de libertad",
           main="QQ plot Población 1")
    
    dist2 <- mahalanobis(dt2[,vv2], center=colMeans(dt2[,vv2]),
                         cov=var(dt2[,vv2]))
    
    qqPlot(dist2, dist="chisq", df=length(dt2[,vv2]), 
           pch=19,las=1,
           ylab="Distancias de Mahalanobis",
           xlab="Cuantiles de una chi-cuadrada con p grados de libertad",
           main="QQ plot Población 2")
    
    grid()
  })

  
#-----------------Pestaña para mostrar resultados Prueba Royston--------------------    
        #------------------Prueba royston Poblacion 1--------------------------------
  output$royston1 <- renderPrint({
    
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                        sep=input$sep1)
    
    valorqq1 <- mvn(data=dt1[,vv1], mvnTest="royston", multivariatePlot="qq")
    valorqq1$multivariateNormality
    
  })
  
  
  
        #------------------Prueba royston Poblacion 2--------------------------------
  output$royston2 <- renderPrint({
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    valorqq2 <- mvn(data=dt2[,vv2], mvnTest="royston", multivariatePlot="qq")
    
    valorqq2$multivariateNormality
  })

  
  output$roystonmg <- renderPrint({
    
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    valorqq1 <- mvn(data=dt1[,vv1], mvnTest="royston", multivariatePlot="qq")
    
    valorqq2 <- mvn(data=dt2[,vv2], mvnTest="royston", multivariatePlot="qq")
    
    
    
    aa1 <- valorqq1[["multivariateNormality"]]$MVN
    aa2 <- valorqq2[["multivariateNormality"]]$MVN
    
    
    if (aa1=="NO" | aa2=="NO"){
      shinyalert(
        title = "Cuidado",
        text = "Una de las muestras no cumple supuesto de normalidad multivariada",
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
        text = "Las dos muestras cumple supuesto de normalidad",
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
  
  
  
#-----------------Pestaña para mostrar resultados Prueba Mardia--------------------     
        #------------------Prueba mardia Población 1-----------------------------
  output$mardia1 <- renderPrint({
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                        sep=input$sep1)
    
    valorqq1 <- mvn(data=dt1[,vv1], mvnTest="mardia", multivariatePlot="qq")
    valorqq1$multivariateNormality
  })  
  
        #------------------Prueba mardia Población 2-----------------------------
  output$mardia2 <- renderPrint({
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    valorqq2 <- mvn(data=dt2[,vv2], mvnTest="mardia", multivariatePlot="qq")
    valorqq2$multivariateNormality
  }) 

#-----------------------Pestaña de resultados-------------------------------------------------  
        #-----------------Mostrar distribucion con grados de libertad----------------------
  output$titleValorp<- renderText({
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt1[,vv1]), nrow=ncol(dt1[,vv1]), 
                      dimnames=list(names(dt1[,vv1])), byrow=T)
    
    ph <- Xo(base1=dt1[,vv1], base2=dt2[,vv2], sigma=Matrizs)
    paste0('Distribución chi cuadrado (', ph$p,")")
  })
  
          #-----------------Mostrar grafico valor p-----------------------------------------
  output$grafico1 <- renderPlot({
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt1[,vv1]), nrow=ncol(dt1[,vv1]), 
                      dimnames=list(names(dt1[,vv1])), byrow=T) 
    
    ph <- Xo(base1=dt1[,vv1], base2=dt2[,vv2], sigma=Matrizs)
    
    
    shadow.dist(dist='dchisq', param=list(df=ph$p),
                a=as.numeric(ph$estadistico), type='upper', 
                col.shadow='blue', xlim=c(0,10))
    
  })
  
         #-----------------Vector de medias muestral, Población 1----------------------
  output$med_muestra1 <- renderPrint({
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt1[,vv1]), nrow=ncol(dt1[,vv1]), 
                      dimnames=list(names(dt1[,vv1])), byrow=T)  
    
    
    Mprima <- Xo(base1=dt1[,vv1], base2=dt2[,vv2], sigma=Matrizs)
    round(Mprima$Mm1, 2)
  })
  
  #-----------------Vector de medias muestral, Población 2----------------------
  output$med_muestra2 <- renderPrint({
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt1[,vv1]), nrow=ncol(dt1[,vv1]), 
                      dimnames=list(names(dt1[,vv1])), byrow=T)  
    
    
    Mprima <- Xo(base1=dt1[,vv1], base2=dt2[,vv2], sigma=Matrizs)
    round(Mprima$Mm2, 2)
  })
  
  #-----------------Matriz de varianza y covarainza poblacional----------------------
  output$S_muestra <- renderPrint({
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt1[,vv1]), nrow=ncol(dt1[,vv1]), 
                      dimnames=list(names(dt1[,vv1])), byrow=T)  
    
    
    Mprima <- Xo(base1=dt1[,vv1], base2=dt2[,vv2], sigma=Matrizs)
    round(Mprima$S, 2)
  })
  
  #----------------------------Resultado de valor p---------------------
  output$resul1 <- renderPrint({
    vv1 <- c(input$names1)
    inFile1 <- input$file1
    if(is.null(inFile1)) 
      dt1 <- read.delim2('base1.txt')
    else dt1 <- read.csv(inFile1$datapath, header=input$header1, 
                         sep=input$sep1)
    
    vv2 <- c(input$names2)
    inFile2 <- input$file2
    if(is.null(inFile2)) 
      dt2 <- read.delim2('base2.txt')
    else dt2 <- read.csv(inFile2$datapath, header=input$header2, 
                         sep=input$sep2)
    
    Matrizs <- input$matrizIng
    Matrizs <- as.numeric(unlist(strsplit(Matrizs, "[\n, \t]")))
    Matrizs <- Matrizs[!is.na(Matrizs)]
    Matrizs <- matrix(Matrizs, ncol=ncol(dt1[,vv1]), nrow=ncol(dt1[,vv1]), 
                      dimnames=list(names(dt1[,vv1])), byrow=T)  
    
    ph <- Xo(base1=dt1[,vv1], base2=dt2[,vv2], sigma=Matrizs)
    
    paste0('El estadístico de prueba es Xo=', round(ph$estadistico, 2),
           ', con un valor-p=',round(ph$value, 4),
           " ,concluya según el nivel de significancia. ")
    
  })
  
})