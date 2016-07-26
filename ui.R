library(shiny)

#Define user interfqace

shinyUI(fluidPage(
  
  # Application title
  titlePanel(title="Estadística descriptiva: Índices (no) resistentes"),
  
  ################################################################
  
  #sidebar Layout: The menu where the user clicks, selects, or writes
  sidebarLayout(
  
    ##################################  
    # Sidebar panel
    sidebarPanel(("Introducir datos"),
                 
                 # Use if it has to be written instead of being chosen from multiple options
                 #textInput(inputId="Variable",
                 #           label="Especifique el nombre de la variable",
                 #          value="Número de amigos"),
                 
                 # Seleccionar una de las dos variables
                 selectInput(inputId="Variable",
                             label="Especifique la variable",
                             choices=list("Número de amigos","Valoración de los amigos"),
                             selected=F,multiple=F,selectize=T),
                 
                 # Seleccionar uno de los tres tipos de escala de medida
                 selectInput(inputId="EscalaM",
                             label="Escala de medida",
                             choices=list("Ordinal","Intervalo","Razón"),
                             selected=F,multiple=F,selectize=T),
                 
                 # Se introduce texto explicativo
                 helpText("El tipo de gráfico posible cambia según la escala de medida"),
                                  
                 # Viene de server.R, donde se define qué opciones contiene "figuras"
                 uiOutput("figuras"),
                 # Introduce espacio: una línea en blanco
                 br(),
                                  
                 # If it does not change with input: it should be declared here
                 #radioButtons(inputId="Grafico",
                 #            label="Escifique el tipo de gráfico",
                 #            choice=list("Boxplot","Histograma"),
                 #            selected=NULL,inline=FALSE),
                 
                 
                 # Cargar un archivo de datos
                 helpText("Utilice archivos .txt y 'friends' como nombre de la columna"),
                 fileInput(inputId="LosDatos",label="Cargar archivo de datos",multiple=F),
                 
                 # Definir el tipo de separador utilizado en el archivo de datos
                 radioButtons(inputId="sep",label="Separador",
                              choices=c(Coma=",",Tabulador="\t",Espacio=" "),
                              selected=","), 
                 
                 # El usuario selecciona un valor mediante un deslizador
                 # animate=T activa la posibilidad de "play" de los valores (tipo Hans Rosling)
                 sliderInput(inputId="ValorDiez",
                              label="Introduzca el valor del último dato",
                              min=0,max=20,value=5,step=NULL,round=F,animate=T),
                 
                 
                 # Optional button that gives control to the user:
                 # Information not automatically updated
                 #submitButton(text="Actualice la información",icon=NULL)
                 
                 # Solo para ver cómo funciona
                 numericInput(inputId="tusamigos", label="¿Cuántos amigos tienes tú?", value=0),
                 
                 # Countador (después del espacio)
                 br(),
                 h6(textOutput("counter"))
                 
                ),
    
    
    ##################################      
    # Main panel
    mainPanel(              
              # Hace que la organización se base en pestañas
              tabsetPanel(type="tabs",
                          # Se escipecifica cómo se llama y qué contiene cada pestaña 
                          # Con el verbatimTextOutput se aprovecha una salida de R, tal cual: aquí summary
                          tabPanel("Datos", 
                                   tableOutput("datos"),
                                   textOutput("escala"),
                                   verbatimTextOutput("sum"),
                                   plotOutput("plotstrip"),
                                   verbatimTextOutput("tusam"),
                                   verbatimTextOutput("DatosC")
                                  ),
                          
                          # Se escipecifica cómo se llama y qué contiene cada pestaña
                          # Con el verbatimTextOutput se aprovecha una salida de R, tal cual: aquí tabla
                          tabPanel("Tendencia central", 
                                   verbatimTextOutput("tc"),
                                   # Incluir botón que permita descargar la tabla
                                   downloadButton(outputId="downTC",label="Guardar tabla (tab-delimited)")),
                          
                          # Se escipecifica cómo se llama y qué contiene cada pestaña 
                          # Con el verbatimTextOutput se aprovecha una salida de R, tal cual: aquí tabla
                          tabPanel("Dispersión", verbatimTextOutput("disp")),
                          
                          # Se escipecifica cómo se llama y qué contiene cada pestaña 
                          # Con el plotOutput se introduce un gráfico definido en server.R
                          tabPanel("Representación gráfica",
                                   plotOutput("plot1"),
                                   # Incluir botón que permita descargar el gráfico
                                   downloadButton(outputId="downplot1",label="Guardar gráfico")),
                          
                          # Se escipecifica cómo se llama y qué contiene cada pestaña 
                          # Para mostrar imagen, vídeo y pdf: se necesita directorio "www" dentro de working directory
                          tabPanel("Ubicación", 
                                   tags$img(src="logo_ub.png",width=383,height=122),
                                   helpText("Cerca del Campus Mundet"),
                                   tags$video(src="valdaran.mp4",type="video/mp4"),
                                   tags$iframe(style="height:400px; width:100%; scrolling=yes",
                                               src="Psico.pdf"),
                                   tags$iframe(style="height:400px; width:100%; scrolling=yes",
                                               src="https://www.dropbox.com/s/wjx451m4uunkd9m/RTcomplement.pdf?raw=1")
                                  )
                          
                          
                         )
              
             )

    ##################################   
    )
  
################################################################ 
))    
