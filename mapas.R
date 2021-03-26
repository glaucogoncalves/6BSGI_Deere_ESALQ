library(RColorBrewer)
library(tidyverse)
library(raster)
library(sf)

geraMapa = function(dados, blocos, titulo, salvar=F, suave=T){
  shp = read_sf("lim_poly.shp")
  mapaRasterizado = raster(shp, ncol=blocos,nrow=blocos)
  mapa = rasterize(dados[,1:2],mapaRasterizado,dados[,3],fun=mean)
  mapaMasked = mask(mapa,shp)
  cores = brewer.pal(9,"Oranges")
  if (suave) {
    mapaFinal = disaggregate(mapaMasked, 10, method='bilinear')
  } else {
    mapaFinal = mapaMasked
  }
  if (salvar) {
    png(paste(titulo,".png",sep=""),width = 1000, height = 700)
  }
  plot(mapaFinal, col=cores, main=titulo, zlim=c(0,lim_max))
  contorno <- rasterToContour(mapaFinal)
  plot(contorno, add=TRUE, lwd=0.2)
  plot(shp, add=TRUE, lwd=0.5)
  if (salvar) {
    dev.off()  
  }
}

gerarPNG = T

################ RF - A ################
dados = read.csv("mapaRF-A-200x200.csv",header = T)
dados = dados %>% dplyr::select(2:4)
geraMapa(dados,200,"RF (A)", salvar = gerarPNG)

################ RF - B ################
dados = read.csv("mapaRF-B-200x200.csv",header = T)
dados = dados %>% dplyr::select(2:4)
geraMapa(dados,200,"RF (B)", salvar = gerarPNG)

################ SPDE - A ################
dados = read.csv("spde_teste_Ca_A.csv",header = T)
dados = dados %>% select(1:3)
geraMapa(dados,122,"SPDE (A)", salvar = gerarPNG)

################ SPDE - B ################
dados = read.csv("spde_teste_Ca_B.csv",header = T)
dados = dados %>% select(1:3)
geraMapa(dados,122,"SPDE (B)", salvar = gerarPNG)

################ RBF - A ################
dados = read.csv("res_rbf_A_200_200.csv",header = T)
geraMapa(dados,135,"RBF (A)", salvar = gerarPNG)

################ RBF - B ################
dados = read.csv("res_rbf_B_200_200.csv",header = T)
geraMapa(dados,135,"RBF (B)", salvar = gerarPNG)

################ RBF - PU - A ################
dados = read.csv("res_pu_A_200_200.csv",header = T)
geraMapa(dados,135,"RBF PU (A)", salvar = gerarPNG)

################ RBF - PU - B ################
dados = read.csv("res_pu_B_200_200.csv",header = T)
geraMapa(dados,135,"RBF PU (B)", salvar = gerarPNG)

################ RF + Features - A ################
dados = read.csv("malha_saulo_A_200x200.csv",header = T)
dados = dados %>% dplyr::select(1:2,4)
geraMapa(dados,135,"RF + Features (A)", salvar = gerarPNG)

################ RF + Features - B ################
dados = read.csv("malha_saulo_B_200x200.csv",header = T)
dados = dados %>% dplyr::select(1:2,4)
geraMapa(dados,135,"RF + Features (B)", salvar = gerarPNG)

################ RF + Features - A - 0.05 ################
dados = read.csv("malha_saulo_A_200x200.csv",header = T)
dados = dados %>% dplyr::select(1:2,3)
geraMapa(dados,135,"RF + Features (A) 0.05", salvar = gerarPNG)

################ RF + Features - A - 0.95 ################
dados = read.csv("malha_saulo_A_200x200.csv",header = T)
dados = dados %>% dplyr::select(1:2,5)
geraMapa(dados,135,"RF + Features (A) 0.95", salvar = gerarPNG)

################ RF + Features - B - 0.05 ################
dados = read.csv("malha_saulo_B_200x200.csv",header = T)
dados = dados %>% dplyr::select(1:2,3)
geraMapa(dados,135,"RF + Features (B) 0.05", salvar = gerarPNG)

################ RF + Features - A - 0.95 ################
dados = read.csv("malha_saulo_B_200x200.csv",header = T)
dados = dados %>% dplyr::select(1:2,5)
geraMapa(dados,135,"RF + Features (B) 0.95", salvar = gerarPNG)

################ RBF - IMQ - A ################
dados = read.csv("rbf_imq_200x200_A.csv",header = T)
dados = dados %>% dplyr::select(2:3,4)
geraMapa(dados,175,"RBF IMQ (A)", salvar = gerarPNG)

################ RBF - IMQ - B ################
dados = read.csv("rbf_imq_200x200_B.csv",header = T)
dados = dados %>% dplyr::select(2:3,4)
geraMapa(dados,175,"RBF IMQ (B)", salvar = gerarPNG)

################ MLS - A ################
dados = read.csv("MLS_A_mean.csv",header = F)
geraMapa(dados,240,"MLS (A)", salvar = gerarPNG)

################ MLS - B ################
dados = read.csv("MLS_B_mean.csv",header = F)
geraMapa(dados,240,"MLS (B)", salvar = gerarPNG)

################ MLS - A ################
dados = read.csv("MLS_A_var.csv",header = F)
geraMapa(dados,240,"MLS (A) - Var", salvar = gerarPNG)

################ MLS - B ################
dados = read.csv("MLS_B_var.csv",header = F)
geraMapa(dados,240,"MLS (B) - Var", salvar = gerarPNG)