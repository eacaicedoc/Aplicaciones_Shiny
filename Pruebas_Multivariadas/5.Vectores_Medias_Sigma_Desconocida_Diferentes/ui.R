library(shiny)
library(shinyalert)
library(markdown)

#-----------------------------------------Interfaz de aplicacion----------------------------------------------
shinyUI(pageWithSidebar(
  headerPanel(title=HTML("Prueba de hipótesis sobre dos vectores &mu; Normal multivariada 
                         &Sigma;<sub>1</sub> &ne; &Sigma;<sub>2</sub> Desconocida."),
              windowTitle="PH medias"),
  #-----------------------------------------Panel de instrucciones-------------------------------------------------------
  sidebarPanel(
    h5('Esta aplicación permite realizar una prueba de hipótesis sobre 
       el vector de medias poblacionale de una distribución normal 
       multivariada.'),
    
    h6('La aplicación por defecto usa los datos de un ejemplo particular de Análisis 
      Multivariado, sin embargo, el usuario puede subir su propia base de datos para 
      usar la app.'),
    
    #------------------------Ventana para seleccionar base de datos 1 de usuario-------------------------------------------
    h4("Base de datos Población 1"),
    h6("Use el siguiente botón para cargar base de datos 1."),
    fileInput(inputId='file1',
              label = NULL,
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )),
    #Seleccion de separado de datos 
    h6("¿Cuál es la separación de los datos 1?"),
    selectInput(inputId="sep1",
                label=NULL, 
                choices=list(Tab='\t', Comma=',',
                             Semicolon=';', 'space'=' '),
                selected=';'),
    
    #Lectura de encabezado de base de datos
    checkboxInput(inputId='header1',
                  label='¿Tiene encabezado la base de datos 1?', 
                  value=TRUE),
    
    #------------------------Ventana para seleccionar base de datos 2 de usuario-------------------------------------------
    h4("Base de datos Población 2"),
    h6("Use el siguiente botón para cargar base de datos 2."),
    fileInput(inputId='file2',
              label = NULL,
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )),
    #Seleccion de separado de datos 
    h6("¿Cuál es la separación de los datos 2?"),
    selectInput(inputId="sep2",
                label=NULL, 
                choices=list(Tab='\t', Comma=',',
                             Semicolon=';', 'space'=' '),
                selected=';'),
    
    #Lectura de encabezado de base de datos
    checkboxInput(inputId='header2',
                  label='¿Tiene encabezado la base de datos 2?', 
                  value=TRUE),
    
    
    
    #-----------------Seleccionar los nombres de las columnas de la base de datos--------------------------------------------
    #----------Seleccion Variables Poblacion 1-----------------------
    checkboxGroupInput(inputId="names1", 
                       label=" Elija la variables cuantitativas para realizar 
                       la prueba de hipótesis, de la Población 1.", 
                       choices=c("x","y","w")),
    
    #----------Seleccion Variables Poblacion 2-----------------------
    checkboxGroupInput(inputId="names2", 
                       label=" Elija la variables cuantitativas para realizar 
                       la prueba de hipótesis, de la Población 2.", 
                       choices=c("x","y", "z")),
    
    #----------------------Creditos de Autores-------------------------
    img(src="https://fhernanb.github.io/docs/logo_unal_negro.png",
        height = 56, width = 140),
    br(),
    br(),
    tags$a(href="https://srunal.github.io", "https://srunal.github.io"),
    br(),
    tags$a(href="https://satorozu.github.io", "Santiago Toro Z."),
    br(),
    tags$a(href="https://eacaicedoc.github.io", "Edwin Caicedo Ch."),
    br(),
    tags$a(href="https://fhernanb.github.io", "Freddy Hernández B."),
    h6("")
    
    ),
  
  
  #------------- Configuracion de ventanas de la aplicacion para presentar resultados----------------------------------
  mainPanel(
    tabsetPanel(type="pills",
                
                tabPanel("Verificación de Supuestos",
                         #------------------------ Supuestos Distancia Mahalanobis-----------------------------------------
                         plotOutput("qqplot", 
                                    width='600px',
                                    height='280px'),
                         br(),
                         #----------------------------- Supuestos Población 1-----------------------------------------
                         h4("Verificación de Supuestos de Normalidad, Población 1"),
                         h5(" Tabla de resumen prueba Royston"),
                         verbatimTextOutput("royston1"),
                         useShinyalert(),
                         verbatimTextOutput("roystonmg"),
                         h5(" Tabla de resumen prueba Mardia"),
                         verbatimTextOutput("mardia1"),
                         br(),
                         br(),
                         #----------------------------- Supuestos Poblacion 2------------------------------------------
                         h4("Verificación de Supuestos de Normalidad, Población 2"),
                         h5(" Tabla de resumen prueba Royston"),
                         verbatimTextOutput("royston2"),
                         h5(" Tabla de resumen prueba Mardia"),
                         verbatimTextOutput("mardia2")),
                
                
                
                tabPanel("Resultados",
                         #-------------------------------Gráfico valor-p-----------------------------------------
                         h4("Valor-P de la prueba"),
                         textOutput("titleValorp"),
                         plotOutput("grafico1", width='500px'),
                         #------------------Información con la cual se crea estadistico---------------------------
                         h4("Vector de medias muestral, Población 1"),
                         verbatimTextOutput('med_muestra1'),
                         h4("Vector de medias muestral, Población 2"),
                         verbatimTextOutput('med_muestra2'),
                         h4("Matriz de covarianzas Poblacional"),
                         verbatimTextOutput('S_muestra'),
                         #---------------------Resultado Prueba de Hipotesis------------------------------------
                         h4("Resultados de la prueba de hipótesis:"),
                         textOutput("resul1"),
                         h5(HTML("Sí Valor-p < &alpha;, entonces se rechaza H<sub>0</sub>: 
                                 &mu;<sub>1</sub> = &mu;<sub>2</sub>."))),
                
                tabPanel("Datos Población 1",
                         "A continuación los datos que está usando 
                         la aplicación, para la Población 1."
                         , uiOutput('inputData1')),
                
                tabPanel("Datos Población 2",
                         "A continuación los datos que está usando 
                         la aplicación, para la Población 2."
                         , uiOutput('inputData2')),
                
                tabPanel("Teoría",
                         includeHTML("Include.html"))))
    ))
