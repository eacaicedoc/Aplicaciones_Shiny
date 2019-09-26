library(shiny)
library(shinyalert)
library(markdown)

#Interfaz de aplicacion
shinyUI(pageWithSidebar(
  headerPanel(title=HTML("Prueba de hipótesis acerca de dos vetores de medias
                          poblacional &mu;<sub>1</sub> y &mu;<sub>1</sub> Normal multivariada. 
                         Observaciones Pareadas"),
              windowTitle="PH media"),
  
  #-----------------------Panel de instrucciones-----------------------------
  sidebarPanel(
    h5('Esta aplicación permite realizar una prueba de hipótesis sobre 
       el vector de medias poblacionale de una distribución normal 
       multivariada.'),
    
    h6('La aplicación por defecto usa los datos de un ejemplo particular de Análisis 
      Multivariado, sin embargo, el usuario puede subir su propia base de datos para 
      usar la app.'),
    
    #------------Ventana para seleccionar base de datos de usuario---------
    fileInput(inputId='file1',
              label='Use el siguiente botón para cargar su base de datos.',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )),
    
    #----------------Lectura de encabezado de base de datos----------------
    checkboxInput(inputId='header',
                  label='¿Tiene encabezado la base de datos?', 
                  value=TRUE),
    
    #-------------------Seleccion de separado de datos---------------------
    selectInput(inputId="sep",
                label="¿Cuál es la separación de los datos?", 
                choices=list(Tab='\t', Comma=',',
                             Semicolon=';', 'space'=' '),
                selected=';'),
    
    #--------Seleccionar los nombres de las columnas de la base de datos----
    checkboxGroupInput(inputId="names", 
                       label=" Elija la variables cuantitativa para realizar 
                       la prueba de hipótesis.", 
                       choices=c("x1","x2","y1","y2")),
    
    img(src="https://fhernanb.github.io/docs/logo_unal_negro.png",
        height=56, width=140),
    br(),
    br(),
    tags$a(href="https://srunal.github.io", "https://srunal.github.io"),
    br(),
    tags$a(href="https://satorozu.github.io", "Santiago Toro Z."),
    br(),
    tags$a(href="https://eacaicedoc.github.io", "Edwin Alexader Caicedo Ch."),
    br(),
    tags$a(href="https://fhernanb.github.io/", "Freddy Hernández B."),
    h6("")
    
    ),
  
  #----Configuracion de ventanas de la aplicacion para presentar resultados-----
  mainPanel(
    tabsetPanel(type="pills",
                
                tabPanel("Verificación de Supuestos",
                         useShinyalert(),
                         verbatimTextOutput("roystonm"),
                         h5("QQ plot para las distancias de Mahalanobis"),
                         plotOutput("qqplot", width='500px'),
                         h5(" Tabla de resumen prueba Royston"),
                         verbatimTextOutput("royston"),
                         h5(" Tabla de resumen prueba Mardia"),
                         verbatimTextOutput("mardia")),
                
                tabPanel("Resultados",
                         h4("Valor-P de la prueba"),
                         textOutput("titleValorp"),
                         plotOutput("grafico1", width='500px'),
                         h4("Vector muestral de diferencias"),
                         verbatimTextOutput('med_diferencias'),
                         h4("Vector de medias muestral de diferencias"),
                         verbatimTextOutput('med_muestra'),
                         h4("Matriz de covarianzas muestral de diferencias"),
                         verbatimTextOutput('S_muestra'),
                         h4("Resultados de la prueba de hipótesis:"),
                         textOutput("resul1"),
                         h5(HTML("Sí Valor-p < &alpha;, entonces se rechaza H<sub>0</sub>: 
                                 &mu;<sub>Y</sub> - &mu;<sub>X</sub>=0."))),
                
                tabPanel("Datos",
                         "A continuación los datos que está usando 
                         la aplicación.", uiOutput('inputData')),
                
                tabPanel("Teoría",includeHTML("Include.html"))))
                         ))