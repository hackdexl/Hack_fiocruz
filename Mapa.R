GerarMapa<-function(df){
require(leaflet)
leaflet(data = df) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~nome, label = ~tipo)

}


FilterDataForMap<-function(lat,long,cut=0.05){
require(data.table)
Data=fread("ubs_funcionamentonone.csv.csv")
Lats=Data$lat
Longs=Data$long
EuclideanDistance=sqrt((Lats-lat)^2 +(Longs-long)^2)
NewData<-Data[EuclideanDistance<cut,]
EuclideanDistance=EuclideanDistance[EuclideanDistance<cut]
if(nrow(NewData)==0){
	print("Não há ubs neste raio do usuario")
	return(NULL)
}
NewData=data.frame(NewData$lat,NewData$long,NewData$no_fantasia,"ubs")
NewData=NewData[order(EuclideanDistance	),]
names(NewData)=c('lat','long','nome','tipo')
return(NewData)

}

FilterDataForMapAmount<-function(lat,long,Size=10){
require(data.table)
Data=fread("ubs_funcionamentonone.csv.csv")
Lats=Data$lat
Longs=Data$long
EuclideanDistance=sqrt((Lats-lat)^2 +(Longs-long)^2)
if(length(EuclideanDistance)<Size)
	Size=length(EuclideanDistance)
Indices=order(EuclideanDistance)[1:Size]
NewData<-Data[Indices,]

NewData=data.frame(NewData$lat,NewData$long,NewData$no_fantasia,"ubs")
names(NewData)=c('lat','long','nome','tipo')
return(NewData)

}
