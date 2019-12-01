#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



PredictImage=function(imagem,modelo,pickle,linhas){
  require(reticulate)
  use_python("/usr/bin/python3")
  require(keras)
  u=modelo
 # u=load_model_hdf5(modelo)
  source_python("classify_function.py")
  l1=prediction(u,pickle,imagem)
  l1=data.frame(l1)
  l1=l1[order(l1$probability,decreasing=TRUE),]
  if(nrow(l1)<linhas)
    print(l1)
  else
    print(l1[1:linhas,])
}

library(shiny)
library(keras)
library(reticulate)
require(leaflet)
use_python("/usr/bin/python3")

# Define UI for application that draws a histogram
ui <- fluidPage(
     includeCSS("www/custom.css"),
     #       column(6,align="center",imageOutput("ginger")),   
   # Application title
   titlePanel("Carrapp"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput(inputId = "SelectImage",label = "Selecione a imagem",accept = c('.jpg','.png')),
        # fileInput(inputId = "SelectModel",label = "Selecione o modelo",accept = c('.model','.hdf5')),
        # fileInput(inputId = "SelectPickle",label = "Selecione o arquivo binarizer pickle",accept = c('.pickle')),
	numericInput(inputId="latitude",label="Latitude da imagem inserida",min=-50,max=50,value=-28.4),
	numericInput(inputId="longitude",label="Longitude da imagem inserida",min=-50,max=50,value=-40.5),
	numericInput(inputId="linhas",label="numero de classes preditas",min=1,max=1000,value=5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="Referenciador",
          tabPanel("Classify Species",verbatimTextOutput("Probabilities"),verbatimTextOutput("Probabilitiesv2"),verbatimTextOutput("Upa"))
          #tabPanel("Classify Health",verbatimTextOutput("Probabilities2"))
        ),
        plotOutput("distPlot"),
        leafletOutput("Map"),   
         h3("Aplicativo desenvolvido pela equipe DexlLab!\n\n")
         #h4("Caso queira saber mais , acesse http://www.saude.gov.br/saude-de-a-z \n\n")
         
      
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000000*1024^2)
  
output$ginger <- renderImage({
          return(list(
            src = "www/img/logo.png",
            contentType = "image/png",
            width = 300,
            height = 200,
            alt = "Face"
          ))
        }, deleteFile = FALSE)


  plot_jpeg = function(path, add=FALSE)
  {
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[2:1] # get the resolution, [x, y]
    if (!add) # initialize an empty plot area if add==FALSE
      plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
  }
   
   output$distPlot <- renderPlot({
     if(!is.null(input$SelectImage)){
       
      #library(raster)  
      #myJPG <- stack(input$SelectImage$datapath)  
      #plotRGB(myJPG)
      plot_jpeg(input$SelectImage$datapath)
    }
   })

#TestarRisco=function(imagem,modelo,pickle,linhas){
TextarRisco<-reactive({
 if(!is.null(input$SelectImage)){
	require(reticulate)
	use_python("/usr/bin/python3")
	require(keras)
	u=LeituraModeloEspecies()
	#u=load_model_hdf5(modelo)
	pickle="Classes.csv"
	imagem=input$SelectImage$datapath
	source_python("classify_function.py")
	l1=prediction(u,pickle,imagem)
	l1=data.frame(l1)
	l1=l1[order(l1$probability,decreasing=TRUE),]
	#if(nrow(l1)<linhas)
#		print(l1)
#	else
#		print(l1[1:linhas,])
	aux=l1[,1]
	Escolhido=aux[1]
	#Escolhido=unlist(l1[1:1]) 
	Risco=c('amblyomma')
	if(Escolhido %in% Risco){
		return(1)

	}
	return(-1)
}
})


UpdateMap<-reactive({
 if(!is.null(input$SelectImage)){
       source("Mapa.R")
      #library(raster)  
      #myJPG <- stack(input$SelectImage$datapath)  
      #plotRGB(myJPG)
	#lat=input$Latitude
	#long=input$longitude
	lat=input$latitude
	long=input$longitude
	data=FilterDataForMapAmount(lat,long,10)
	return(data)
      #plot_jpeg(input$SelectImage$datapath)
    }
})
output$Map <- renderLeaflet({
     if(!is.null(input$SelectImage)  ){
    Risco=TextarRisco()
    if(Risco==1){
    	data=UpdateMap()
    	GerarMapa(data)
    }
}
   })
output$Upa <- renderPrint({
   if(!is.null(input$SelectImage)  ){
   Risco=TextarRisco()
   if(Risco==1){ 
   data=UpdateMap()
    data$nome=as.character(data$nome)
    cat("A unidade basica de atendimento mais proxima é a de",data$nome[1],"\n")
   }
   }
})
   
   output$Probabilities=renderPrint({
     if(!is.null(input$SelectImage)  ){
        source("KerasRAndPythonScript.R")
        #PredictImage(input$SelectImage$datapath,input$SelectModel$datapath,input$SelectPickle$datapath,input$linhas)
       modelo=LeituraModeloEspecies()
       PredictImage(input$SelectImage$datapath,modelo,"Classes.csv",input$linhas)
      # print(df)
       
     }    
   })

   output$Probabilitiesv2=renderPrint({
     if(!is.null(input$SelectImage)  ){
        source("KerasRAndPythonScript.R")
        #PredictImage(input$SelectImage$datapath,input$SelectModel$datapath,input$SelectPickle$datapath,input$linhas)
       modelo=LeituraModeloEspecies()
       PredictImagev2(input$SelectImage$datapath,modelo,"Classes.csv",input$linhas)
      # print(df)
       
     }    
   })
   
   output$Probabilities2=renderPrint({
     if(!is.null(input$SelectImage) ){
       source("KerasRAndPythonScript.R")
       #PredictImage(input$SelectImage$datapath,input$SelectModel$datapath,input$SelectPickle$datapath,input$linhas)
       modelo=LeituraModeloDoencas()
       
       PredictImage(input$SelectImage$datapath,modelo,"SickvsHealthyFixed.pickle",input$linhas)
       
       
     }    
   })
   
   LeituraModeloEspecies<-reactive({
     u=load_model_hdf5("Carrapatos1.h5")
     return(u)
   })
   
   LeituraModeloDoencas<-reactive({
     u=load_model_hdf5("DiseasedPlantsV4.model")
     return(u)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

