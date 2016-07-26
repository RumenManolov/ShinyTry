# Needs to be loaded
library(shiny)

#Define max file size
options(shiny.maxRequestSize=9*1024^2)


shinyServer(
  
  function(input,output){
    
######################################################    
    # OPTIONAL: Creating functions, so that objects are declared only once,
    #  not within every output$...

    #ultimoValor <- reactive({
    #  as.numeric(input$ValorDiez)
    #})
    # Use as a function: object name + brackets
    #ultimoValor()
    
######################################################
    # Diferentes aspectos del input que se pueden imprimir como texto
    output$myVariable <- renderText(input$Variable)
    output$myGrafico <- renderText(input$Grafico)
    output$ultValor <- renderText(paste("Último valor", input$ValorDiez))

#####################################################
    # Código necesario para leer el archivo de datos
    DatosCargados <- reactive({
      archivo <- input$LosDatos
      if(is.null(archivo)){return()}
      read.table(file=archivo$datapath,
                 sep=input$sep,
                 header=TRUE,
                 fill=TRUE)
    })


#####################################################
    # Para imprimir texto
    output$escala <- renderText(
      if ( (input$EscalaM=="Nominal") || (input$EscalaM=="Ordinal") ) print("No son apropiados í­ndices basados en momentos") else
        print("Son apropiados í­ndices basados en momentos") )
      

#####################################################
   # Para imprimir salidas de R
   output$tusam <- renderPrint({
     # Último valor
     ultimoValor <- as.numeric(input$ValorDiez)
     # Los 10 valores
     todos <- c(5,6,6,7,7,3,5,5,4,ultimoValor)
     # Guardar el número de amigos del usuario
     tuyo <- input$tusamigos
     
     counter <- 0
     for (i in 1:length(todos))
       if (tuyo >= todos[i]) counter <- counter + 1
       
     cat("Tienes tantos o más amigos que el", 
           round( (counter/length(todos)*100),2), 
           "% de las personas del gráfico", "\n") 
   })

#####################################################
# Para imprimir  salidas de R: stem-and-leaf de los datos cargados
   output$DatosC <- renderPrint({
     datosnum <- as.data.frame(DatosCargados()) 
     ultimoValor <- as.numeric(input$ValorDiez)
     todos <- c(datosnum$friends,ultimoValor)
     stem(todos)
     
   })
#####################################################    
   # Para imprimir una tabla que R ofrece automáticamente 
   output$datos <- renderTable({
      
      ultimoValor <- as.numeric(input$ValorDiez)
      datosnum <- as.data.frame(DatosCargados())
      todos <- c(datosnum$friends,ultimoValor)
      table(todos)
      
    })
    
#####################################################   
    # Para imprimir un resumen que R ofrece automáticamente
    output$sum <- renderPrint({
      ultimoValor <- as.numeric(input$ValorDiez)
      datosnum <- as.data.frame(DatosCargados())
      todos <- c(datosnum$friends,ultimoValor)
      summary(todos)         
    }) 

#####################################################    
    # Se realizan diferentes cálculos de interés en la pestaña TC
    output$tc <- renderPrint({
      # Guardar el valor introducido por el usuario
      ultimoValor <- as.numeric(input$ValorDiez)
      # Guardar con el resto de los valores
      datosnum <- as.data.frame(DatosCargados())
      todos <- c(datosnum$friends,ultimoValor)
      # Calcular los índices
      media <- round(mean(todos),2)
      mediana <- round(median(todos),2)
      promcuart <- round(as.numeric( (quantile(todos,p=0.25)+quantile(todos,p=0.75))/2 ),2)
      trimedia <- round(as.numeric( (quantile(todos,p=0.25)+quantile(todos,p=0.75) + 2*mediana) / 4 ),2)
      trimmed <- round(mean(todos,trim=0.1),2)
      
      # Se crea una tabla que se imprimirá
      tablita <- 1:(2*5)
      dim(tablita) <- c(5,2)
      tablita[,1] <- c("Media","Mediana","Promedio de los cuartiles", "Trimedia", "Media recortada al 10%")
      tablita[,2] <- format(round( as.numeric(c(media,mediana,promcuart,trimedia,trimmed)),2), nsmall = 2) 
      tablita <- as.data.frame(tablita)
      colnames(tablita) <- c("Índice de tendencia central", "Valor")
      print(tablita,quote=FALSE, right=TRUE,row.names = FALSE)
    }) 
    
#####################################################    
   # Se realizan diferentes cálculos de interés en la pestaña disp
    output$disp <- renderPrint({
      # Guardar el valor introducido por el usuario
      ultimoValor <- as.numeric(input$ValorDiez)
      # Guardar con el resto de los valores
      datosnum <- as.data.frame(DatosCargados())
      todos <- c(datosnum$friends,ultimoValor)
      # Calcular los índices
      desvest <- sd(todos)*((length(todos)-1)/(length(todos)))
      MAD <- median( abs(todos-median(todos)))
      DM <-  sum( abs(todos-mean(todos))) / length(todos)
      coefvar <- desvest / mean(todos)
      coefvarQ <- (quantile(todos,p=0.75)-quantile(todos,p=0.25)) / (quantile(todos,p=0.75)+quantile(todos,p=0.25))

      # Se crea una tabla que se imprimirá
      tablitaD <- 1:(2*5)
      dim(tablitaD) <- c(5,2)
      tablitaD[,1] <- c("Desviación estándar",
                       "Mediana de las desviaciones absolutas",
                       "Desviación media", 
                       "Coeficiente de variación", 
                       "Coeficiente de variación cuartílico")
      tablitaD[,2] <- format(round( as.numeric(c(desvest,MAD,DM,coefvar,coefvarQ)),2), nsmall = 2)
      tablitaD <- as.data.frame(tablitaD)
      colnames(tablitaD) <- c("Índice de dispersión", "Valor")
      print(tablitaD,quote=FALSE, right=TRUE,row.names = FALSE)
    }) 

#####################################################    
   # Se crea el gráfico del tipo escogido
   output$plot1 <- renderPlot({
      # Guardar el tipo de gráfico que se debe hacer
      plotting <- input$Grafico
      # Guardar el valor introducido por el usuario
      ultimoValor <- as.numeric(input$ValorDiez)
      # Guardar con el resto de los valores
      datosnum <- as.data.frame(DatosCargados())
      todos <- c(datosnum$friends,ultimoValor)
      
      # Hacer el gráfico según el tipo escogido por el usuario
      if (plotting=="Boxplot") 
      {
        boxplot(todos,ylim=c(0,20),horizontal=TRUE,xlab=input$Variable)
        points(todos[1:9],jitter(rep(1,length(todos[1:9]),factor=0.05)),col="blue",pch=17,cex=1)
        points(ultimoValor,1,col="green",pch=17,cex=1.6)
        points(mean(todos),1,pch=4,col="red",cex=2)
     
      }
      
      if (plotting=="Histograma")
      {
        hist(todos,breaks=seq(0,20,2),xlab=input$Variable)
        abline(v=mean(todos),col="red")
      }
      
      if (plotting=="Barras")
      {
        counting <- table(todos)
        barplot(counting,xlab=input$Variable)
      }
      
      })
    
#####################################################    
   # Se crea un gráfico del tipo "por defecto" 
   output$plotstrip <- renderPlot({
      # Guardar el valor introducido por el usuario
      ultimoValor <- as.numeric(input$ValorDiez)
      # Guardar con el resto de los valores
      datosnum <- as.data.frame(DatosCargados())
      todos <- c(datosnum$friends,ultimoValor)
      
      # Representar el gráfico
      library(graphics)
      stripchart(todos,method="jitter",vertical=T,jitter=1,xlab=input$Variable)
      })
    
#####################################################    
   # Código para guardar el gráfico con la función downloadHandler
   output$downplot1 <- downloadHandler(
     filename=function(){
       # Se especifica el nombre que tendra el archivo y el tipo (.pnd por defecto)
       paste("graf","png",sep=".")
     },
     
     content= function(file){
       # Función para guardar como png
       png(file)
       
       # Qué guardar: se repite todo el código de antes
       plotting <- input$Grafico
       ultimoValor <- as.numeric(input$ValorDiez)
       datosnum <- as.data.frame(DatosCargados())
       todos <- c(datosnum$friends,ultimoValor)
       
       if (plotting=="Boxplot") 
       {
         boxplot(todos,ylim=c(0,20),horizontal=TRUE)
         points(todos[1:9],jitter(rep(1,length(todos[1:9]),factor=0.05)),col="blue",pch=17,cex=1)
         points(ultimoValor,1,col="green",pch=17,cex=1.6)
         points(mean(todos),1,pch=4,col="red",cex=2)
         
       }
       
       if (plotting=="Histograma")
       {
         hist(todos,breaks=seq(0,20,2))
         abline(v=mean(todos),col="red")
       }
       
       if (plotting=="Barras")
       {
         counting <- table(todos)
         barplot(counting,xlab=input$Variable)
       }
       
        dev.off()
     }
     )  
    
#####################################################
    # Código para guardar la tabla  con la función downloadHandler
    output$downTC <- downloadHandler(
      filename=function(){
        # Se especifica el nombre que tendra el archivo y el tipo (.txt por defecto)
        paste("dat","txt",sep=".")
      },
      
      content= function(file){
        
        # Qué guardar: se repite todo el código de antes 
        ultimoValor <- as.numeric(input$ValorDiez)
        datosnum <- as.data.frame(DatosCargados())
        todos <- c(datosnum$friends,ultimoValor)
        
        media <- round(mean(todos),2)
        mediana <- round(median(todos),2)
        promcuart <- round(as.numeric( (quantile(todos,p=0.25)+quantile(todos,p=0.75))/2 ),2)
        trimedia <- round(as.numeric( (quantile(todos,p=0.25)+quantile(todos,p=0.75) + 2*mediana) / 4 ),2)
        trimmed <- round(mean(todos,trim=0.1),2)
        
        tablita <- 1:(2*5)
        dim(tablita) <- c(5,2)
        tablita[,1] <- c("Media","Mediana","Promedio de los cuartiles", "Trimedia", "Media recortada al 10%")
        tablita[,2] <- format(round( as.numeric(c(media,mediana,promcuart,trimedia,trimmed)),2), nsmall = 2) 
        tablita <- as.data.frame(tablita)
        colnames(tablita) <- c("Índice de tendencia central", "Valor")
        print(tablita,quote=FALSE, right=TRUE,row.names = FALSE)
        
        # Imprimir la tabla en el archivo, cols separadas por tabuladores (\t)
        write.table(tablita,file,sep="\t",row.names = FALSE,quote=FALSE)
      }
    )


#####################################################    
    # Utilizar la función reactive para que las cosas se puedan guardar con una sola llamada
    # Utilizar la función switch para ir cambiando las opciones según el input$EscalaM: es como un "if"
    # Se guarda la variable "graph" como función "graph()" que se puede llamar ya en cualquier
    #  parte del server.R
    graph <- reactive({
      switch(input$EscalaM,
             "Intervalo" = list("Boxplot","Histograma"),
             "Razón" = list("Boxplot","Histograma"),
             "Ordinal" = list("Boxplot", "Barras")
            )  
    })

 
#####################################################    
    #Things that will change according to initial user input
    output$figuras <- renderUI({
      
      
      radioButtons(inputId="Grafico",
                  label="Escifique el tipo de gráfico",
                  choices=graph(),
                  selected=NULL,inline=FALSE)
      
      })

##################################################### 
   # Include a counter
   output$counter <- renderText({
     if (!file.exists("counter.Rdata"))
       counter <- 0
     else
       load(file="counter.Rdata")
     counter <- counter + 1
     save(counter, file="counter.Rdata")
     paste0("Visited: ", counter)
   })
  
#####################################################    
  }
  
  )