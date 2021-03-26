#carrega bibliotecas
library(randomForest)
library(Metrics)
library(sf)
library(tmap)
library(sp)
library(raster)
library(RColorBrewer)
library(tidyverse)

set.seed(1)

#traz os dados
dados = read.csv("workshop_cemeai_spatinterp_data.csv",T)
shp = read_sf("lim_poly.shp")

#rasterização
#mapaRasterizado = raster(shp, res=10)
blocos = 200
mapaRasterizado = raster(shp, ncol=blocos,nrow=blocos)

mapaRasterizado1 = mapaRasterizado
mapaRasterizado2 = mapaRasterizado
values(mapaRasterizado1) = coordinates(mapaRasterizado)[,1]
values(mapaRasterizado2) = coordinates(mapaRasterizado)[,2]

mapaStack = stack(mapaRasterizado1, mapaRasterizado2)
names(mapaStack) =  c("POINT_X","POINT_Y")

metricas = data.frame(MAE = numeric(),RMSE = numeric(),BIAS = numeric(),COR = numeric(),MAPE = numeric())

for(i in c("A","B")){
  dadosNivel = dados %>% filter(str_detect(ID,i))
  dadosTreino = dadosNivel %>% filter(set=="cal_candidate")
  dadosTeste = dadosNivel %>% filter(set=="validation")
  
  inicio <- Sys.time()
  modelRF <- randomForest::randomForest(Ca ~ POINT_X+POINT_Y, data=dadosTreino, ntree=500, mtry=2)
  fim <- Sys.time()
  tempo.modelagem <- fim - inicio
  
  inicio <- Sys.time()
  valueInterpolation =  predict(modelRF, dadosTeste)  
  fim <- Sys.time()
  tempo.interpolacaoTeste <- fim - inicio
  
  MAE = mae(dadosTeste$Ca,valueInterpolation)
  RMSE = rmse(dadosTeste$Ca,valueInterpolation)
  BIAS = bias(dadosTeste$Ca,valueInterpolation)
  COR = cor(dadosTeste$Ca,valueInterpolation)
  MAPE = mape(dadosTeste$Ca,valueInterpolation)
  
  metricas[nrow(metricas)+1,] = c(MAE,RMSE,BIAS,COR,MAPE)
  
  cores <- brewer.pal(9,"YlOrRd")
  dadosSp = st_as_sf(dadosNivel,coords = c("POINT_X","POINT_Y"))
  m = tm_shape(shp) + tm_polygons() + tm_shape(dadosSp) +
    tm_dots(col="Ca", palette = cores, title=paste("Ca",i), size=0.2, style= "cont") +
    tm_legend(legend.outside=TRUE)
  print(m)
  
  #interpolação
  inicio <- Sys.time()
  pm = predict(mapaStack, modelRF)
  fim <- Sys.time()
  tempo.interpolacaoMapa <- fim - inicio
  
  mapaRF = mask(pm, shp)
  
  cores <- brewer.pal(9,"YlOrRd")
  
  inicio <- Sys.time()
  plot(mapaRF, col = cores)
  fim <- Sys.time()
  tempo.mapear <- fim - inicio
  
  mapaCsv = rasterToPoints(mapaRF)
  write.csv(mapaCsv, paste("mapaRF-",i,"-",blocos,"x",blocos,".csv",sep=""))
  
  print(tempo.modelagem)
  print(tempo.interpolacaoTeste)
  print(tempo.interpolacaoMapa)
  print(tempo.mapear)
}

rownames(metricas) = c("A","B")

id = 2
valueInterpolation =  predict(modelRF, dadosTeste, predict.all=T)  
todas_as_estimativas = valueInterpolation$individual[id,]
hist(todas_as_estimativas,10,main=paste("Histograma das saídas de todas as árvores para a amostra",id))
lines(c(valueInterpolation$aggregate[id],valueInterpolation$aggregate[id]),c(0,200),col="red",lty=2)
quantile(todas_as_estimativas,c(0.025,0.975))