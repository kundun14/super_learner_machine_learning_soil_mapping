#Código para crear archivo rds de covariables

#directorio de trabajo donde estan las covariables que generaste en GEE
setwd("C:/Users/aylin/OneDrive/Escritorio/INFyS/R/Bio_enviro/bio_enviro_1km")

#carga librerias
library(raster)
#genera una lista de archivos que terminen en tif de nuestro directorio
lis <- list.files(pattern='.tif')
#lee un raster de referencia
ref <- raster('elevation.tif')
#genera un raster stack vacio - coleccion de capas raster en un vector
todos_stack <- stack()
# iniciamos un ciclo que va de 1 a la longitud de lis (39 capas)
for (i in 1:length(lis)){
  # leemos la capa i (1:39)
  refi <- raster(lis[i])
  #remuestreamos esa capa al raster de referencia
  refi <- resample (refi, ref, method='ngb')
  #luego guardamos esa capa en el stack
  todos_stack <- stack(todos_stack, refi)
  #vemos progreso
  print(i)
  #cerramos el ciclo
}
#vemos los nombres de la nueva capa
names(todos_stack)
#salvar esta capa como obejto nativo de R
saveRDS(as(todos_stack, 'SpatialPixelsDataFrame'), file='predictores_test.rds')
#end
