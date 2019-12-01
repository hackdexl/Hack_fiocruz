

PredictImage=function(imagem,modelo,pickle,linhas){
	require(reticulate)
	use_python("/usr/bin/python3")
	require(keras)
	#u=load_model_hdf5(modelo)
	u=modelo
	source_python("classify_function.py")
	l1=prediction(u,pickle,imagem)
	l1=data.frame(l1)
	names(l1)=c('Classe','Probabilidade')
	l1=l1[order(l1$probability,decreasing=TRUE),]
	if(nrow(l1)<linhas)
		print(l1)
	else
		print(l1[1:linhas,])
	#aux=l1[,1]
	#Escolhido=aux[1]
	##Escolhido=unlist(l1[1:1]) 
	Risco=c('amblyomma')
	#if(Escolhido %in% Risco){
	#	cat("O carrapato foi identificado como do genero ",Escolhido,"\n","Favor se dirigir ao centro de atendimento medico mais proximo de sua localização\n")
	#}else{	
	#cat("O carrapato foi identificado como do genero ",Escolhido,"\n")
	#}
}


PredictImagev2=function(imagem,modelo,pickle,linhas){
	require(reticulate)
	use_python("/usr/bin/python3")
	require(keras)
	u=modelo
	#u=load_model_hdf5(modelo)
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
		cat("O carrapato foi identificado como do genero ",Escolhido,"\n","Caso o carrapato tenha entrado em contato com o ser humano\nRecomenda-se que este se diriga ao centro de atendimento medico mais proximo de sua localização\nCaso queira saber mais acesse:\n\n\t http://www.saude.gov.br/saude-de-a-z")
	}else{	
	cat("O carrapato foi identificado como do genero ",Escolhido,"\nCaso queira saber mais acesse:\n\n\t http://www.saude.gov.br/saude-de-a-z")
	}
}
