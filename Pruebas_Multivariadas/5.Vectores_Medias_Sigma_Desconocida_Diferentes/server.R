library(shiny)
library(shinyalert)
library(MVN)
library(markdown)
library(psych)
library(car)
source("grafico.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #Comando para seleccionar la base o base por defecto
  
  #---------------Comando para seleccionar la base o base por defecto-------------
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
  Tamano <- function(base1,base2) {
    n <- nrow(base1)
    m <- nrow(base2)
    
    if ((n<=30) | (m<=30)) {
      #Medias muestrales 
      Mm1 <- c(unname(colMeans(base1))) 
      Mm2 <- c(unname(colMeans(base2))) 
      
      S1 <- cov(base1)
      V1 <- S1/n
      S2 <- cov(base2)
      V2 <- S2/m
      
      Sp <- V1+V2
      Scom <- solve(Sp)
      
      #Estadístico de prueba
      T0 <- (t(Mm1-Mm2))%*%Scom%*%(Mm1-Mm2)
      
      p <- length(Mm1) 
      
      num <- tr(Sp)+(tr(Sp))^2
      d <- (1/(n-1))*(tr(V1)+tr(V1)^2)
      d2 <- (1/(m-1))*(tr(V2)+tr(V2)^2)
      de <- d+d2
      
      v <- num/de
      a <- (v*p)/v-p+1
      
      #Valor p de la prueba
      value <- pf(q=T0/a, df1=p, df2=v-p+1, lower.tail=F)
      
      res <- list(estadistico=T0, n=n, df1=p, df2=n+m-p-1, m=m, Mm1=Mm1,
                  Mm2=Mm2, S=Sp, value=value, Scom=Scom, v=v, a=a)
    }
    
    else {
      #Medias muestrales 
      Mm1 <- c(unname(colMeans(base1))) 
      Mm2 <- c(unname(colMeans(base2))) 
      
      S1 <- cov(base1)
      V1 <- S1/n
      S2 <- cov(base2)
      V2 <- S2/m
      
      Sp <- V1+V2
      Scom <- solve(Sp)
      
      #Estadístico de prueba
      X0 <- (t(Mm1-Mm2))%*%Scom%*%(Mm1-Mm2)
      
      p <- length(Mm1) 
      
      #Valor p de la prueba
      value <- pchisq(q=X0, df=p, lower.tail=F)
      
      res <- list(estadistico=X0, n=n, df=p, p=p, m=m, Mm1=Mm1,
                  Mm2=Mm2, S=Sp, value=value, Scom=Scom)
    }
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
    
    
    dist1 <- mahalanobis(dt1[,vv1],
                         center=colMeans(dt1[,vv1]),
                         cov=var(dt1[,vv1])) #Cambiar por matriz var ingresada
    qqPlot(dist1, dist="chisq", df=length(dt1[,vv1]), 
           pch=19,las=1,
           ylab="Distancias de Mahalanobis",
           xlab="Cuantiles de una chi-cuadrada con p grados de libertad",
           main="QQ plot Población 1")
    
    dist2 <- mahalanobis(dt2[,vv2],
                         center=colMeans(dt2[,vv2]),
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
    
    ph <- Tamano(base1=dt1[,vv1], base2=dt2[,vv2])
    
    if ((ph$n<=30)|(ph$m<=30)){
      paste0('Distribución F (', ph$df1,",", ph$df2,")")
    }
    else {
      paste0('Distribución Chi-cuadrado (', ph$p,")")
    }
    
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
    
    ph <- Tamano(base1=dt1[,vv1], base2=dt2[,vv2])
    
    if ((ph$n<=30) | (ph$m<=30)) {
      
      shadow.dist(dist='df', param=list(df1=ph$df1, df2=ph$df2),
                  a=(as.numeric(ph$estadistico)/ph$a), type='upper', 
                  col.shadow='blue', xlim=c(0,10))
    }
    
    else {
      
      shadow.dist(dist='dchisq', param=list(df=ph$p),
                  a=as.numeric(ph$estadistico), type='upper', 
                  col.shadow='blue', xlim=c(0,10))
    }
    
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
    
    Mprima <- Tamano(base1=dt1[,vv1], base2=dt2[,vv2])
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
    
    Mprima <- Tamano(base1=dt1[,vv1], base2=dt2[,vv2])
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
    
    Mprima <- Tamano(base1=dt1[,vv1], base2=dt2[,vv2])
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
    
    ph <- Tamano(base1=dt1[,vv1], base2=dt2[,vv2])
    
    if ((ph$n<=30) | (ph$m<=30)) {
      paste0('El estadístico de prueba es To=', round(ph$estadistico, 2),
             ', con un valor-p=',round(ph$value, 4),
             " ,concluya según el nivel de significancia.")
    }
    else {
      paste0('El estadístico de prueba es X0=', round(ph$estadistico, 2),
             ', con un valor-p=',round(ph$value, 4),
             " ,concluya según el nivel de significancia. ")
    }
    
  })
  
})