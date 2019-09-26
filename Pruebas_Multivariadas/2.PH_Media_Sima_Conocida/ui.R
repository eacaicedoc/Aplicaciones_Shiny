library(shiny)
library(shinyalert)
library(MVN)
library(markdown)
library(car)
source("grafico.R")

#------------------------Interfaz de aplicacion------------------------
shinyUI(pageWithSidebar(
  headerPanel(title=HTML("Prueba de hipótesis sobre &mu; Normal multivariada &Sigma; Conocida"),
              windowTitle="PH media"),
  
  #----------------------Panel de instrucciones------------------------
  sidebarPanel(
    h5('Esta aplicación permite realizar una prueba de hipótesis sobre 
       el vector de medias poblacionales de una distribución normal 
       multivariada.'),
  
  h6('La aplicación por defecto usa los datos de un ejemplo particular de Análisis 
      Multivariado, sin embargo, el usuario puede subir su propia base de datos para 
      usar la app.'),
  
  #-------------Ventana para seleccionar base de datos------------------
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
  
  #--------------Lectura de encabezado de base de datos-----------------
  checkboxInput(inputId='header',
                label='¿Tiene encabezado la base de datos?', 
                value=TRUE),
  
  #-------------------Seleccion de separado de datos--------------------
  selectInput(inputId="sep",
              label="¿Cuál es la separación de los datos?", 
              choices=list(Tab='\t', Comma=',',
                           Semicolon=';', 'space'=' '),
              selected=';'),
  
  #------Seleccionar los nombres de las columnas de la base de datos----
  checkboxGroupInput(inputId="names", 
                     label=" Elija la variables cuantitativa para realizar 
                     la prueba de hipótesis.", 
                     choices=c("Estatura","Peso")),
  
  #--------------Manera de ingresar el vector de medias-----------------
  p(strong(HTML("Ingrese el valor de referencia &mu;<sub>0</sub> para probar
                H<sub>0</sub>: &mu; = &mu;<sub>0</sub>. Escriba los valores del 
                vector separados por un espacio, así como se muestra a continuación:"))),
  tags$textarea(id="vectorIng", cols=20, rows=2, "70 170"),
  
  #-----------Manera de ingreasar la matriz de varianza covarianza-------
  p(strong(HTML("Ingrese el valor de referencia
                            &sum; para probar
                H<sub>0</sub>: &mu; = &mu;<sub>0</sub>. Escriba los valores de la matriz separados 
                por un espacio, para asignar valores a la siguiente fila separar por 'Enter'. 
                Así como se muestra a continuación:"))),
  tags$textarea(id="matrizIng", cols=20, rows=6,
                  "20  100\n100  100"),
  
  br(),
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
  
  #--Configuracion de ventanas de la aplicacion para presentar resultados--
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
                         h4("Vector de medias muestral"),
                         verbatimTextOutput('med_muestra'),
                         h4("Matriz de covarianzas muestral"),
                         verbatimTextOutput('S_muestra'),
                         h4("Vector de medias de referencia"),
                         verbatimTextOutput('med_ho'),
                         h4("Matriz de varianza covarianza de referencia"),
                         verbatimTextOutput('matriz_ho'),
                         h4("Resultados de la prueba de hipótesis:"),
                         textOutput("resul1"),
                         h5(HTML("Sí Valor-p < &alpha;, entonces se rechaza 
                                 H<sub>0</sub>: &mu; = &mu;<sub>0</sub>."))),
                
                tabPanel("Datos",
                         "A continuación los datos que está usando 
                         la aplicación.", uiOutput('inputData')),
                
                tabPanel("Teoría", 
                         includeHTML("Include.html")))
  )))