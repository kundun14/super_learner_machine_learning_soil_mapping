rm(list=ls()) # borrar directorio de trabajo

#instalar paquetes

list.of.packages = c("landmap", "rgdal", "geoR", "plotKML", "raster", "glmnet", "xgboost" , "kernlab", "deepnet", "mlr", "mapview")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#cargar paquetes

library(landmap)
library(rgdal)
library(geoR)
library(plotKML)
library(raster)
library(glmnet)
library(xgboost)
library(kernlab)
library(deepnet)
library(mlr)
library(mapview)

#covariables sin transformar
covar = readRDS('covariates_1km_10PCA_6OGC_scaled.rds') # covariables transformadas a PC

#covar = readRDS('covariates_1km.rds') # covariables originales

names(covar) # nombres de las variables
spplot(covar["NDVI"]) # grafica de una  covariable
writeRaster(covar["dem.1"], dem_1.tiff) # guardar dem en directorio
projection(covar)
# Coordinate Reference System (CRS) arguments: +proj=lcc +lat_0=40
# +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=WGS84 +units=m
# +no_defs

#descarga de internet de limites geograficos de mexico
lim = getData('GADM', country='MEX', level=1)
projection(lim) 
lim = spTransform(lim, CRS(projection(covar))) # proyecta el CRS al CRS de covar
plot(lim)

# recortar covariable a un area de interes mas reducida 
covar_cr = crop(covar, drawExtent()) # drawExtent()  pide ubicar dos puntos 
spplot(covar_cr["covariates_1km_10PCA_6OGC_scaled.1"]) #grafica ndvi nueva area

covar = as(covar, 'SpatialPixelsDataFrame') #convertir covar a SpatialPixelsDataFrame

#covar = covar[1:25]

#importar los datos

dat = read.csv('soc_12_2020.xls - soc.csv')

coordinates(dat) = ~ x + y # definicion de coordenas como x + y
#definicion del crs a los puntos
proj4string(dat) = crs('+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=WGS84 +units=m +no_defs')


dat = dat[which(!duplicated(dat@coords)), ] #eliminar coordinadas posiblemente duplicadas

dat = spTransform(dat, CRS(projection(covar))) # reprojectar puntos al crs de los rasters

dat = crop(dat, covar_cr) # recortar al area de estudio

#variable de respuesta

TARGET = dat[dat$soc.t.ha > 0 ,] #no haya data negativos

#visualizaciones interactiva de los puntos

mapview(TARGET['soc.t.ha'])

#aprendizaje 

#tranformacion de variables
TARGET$SOC_log = log1p(TARGET$soc.t.ha) # transformacion logaritmica de los datos y creamos la variable SOC_log
#TARGET@data dataframe 
hist(TARGET$SOC_log)
hist(TARGET$soc.t.ha)

#modelo
#https://gitlab.com/openlandmap/spatial-predictions-using-eml

m = train.spLearner(TARGET["SOC_log"],
                    covariates=covar_cr, 
                    lambda = 1,
                    spc=FALSE,oblique.coords = FALSE, 
                    buffer.dist = FALSE, 
                    parallel=TRUE)

# weights:  55
# initial  value 162.250578 
# iter  10 value 9.457803
# iter  20 value 4.387461
# iter  30 value 2.110312
# iter  40 value 1.785704
# iter  50 value 1.749550
# iter  60 value 1.639463
# iter  70 value 1.174541
# iter  80 value 0.966459
# iter  90 value 0.916167
# iter 100 value 0.912027
# final  value 0.912027 
# stopped after 100 iterations
# # weights:  55
# initial  value 342.865311 
# iter  10 value 7.119734
# iter  20 value 1.491436
# iter  30 value 0.223451
# iter  40 value 0.064469
# iter  50 value 0.043912
# iter  60 value 0.033643
# iter  70 value 0.023505
# iter  80 value 0.000134
# iter  80 value 0.000082
# iter  80 value 0.000081
# final  value 0.000081 
# converged
# # weights:  55
# initial  value 148.400202 
# iter  10 value 8.683862
# iter  20 value 6.297404
# iter  30 value 4.998756
# iter  40 value 4.919123
# iter  50 value 3.868360
# iter  60 value 2.609418
# iter  70 value 2.574461
# iter  80 value 2.552773
# iter  90 value 2.527799
# iter 100 value 2.508969
# final  value 2.508969 
# stopped after 100 iterations
# # weights:  55
# initial  value 334.112100 
# iter  10 value 5.899789
# iter  20 value 0.540576
# iter  30 value 0.082897
# iter  40 value 0.050048
# iter  50 value 0.036563
# iter  60 value 0.019135
# iter  70 value 0.006893
# iter  80 value 0.001702
# final  value 0.000080 
# converged
# # weights:  55
# initial  value 302.067740 
# iter  10 value 8.440660
# iter  20 value 3.233035
# iter  30 value 1.136978
# iter  40 value 0.914098
# iter  50 value 0.262145
# iter  60 value 0.213724
# iter  70 value 0.196909
# iter  80 value 0.186800
# iter  90 value 0.186043
# iter 100 value 0.184092
# final  value 0.184092 
# stopped after 100 iterations
# # weights:  55
# initial  value 195.615387 
# iter  10 value 8.795647
# iter  20 value 4.540796
# iter  30 value 1.033623
# iter  40 value 0.827118
# iter  50 value 0.561419
# iter  60 value 0.057336
# iter  70 value 0.001928
# iter  80 value 0.001838
# iter  90 value 0.001791
# iter 100 value 0.001667
# final  value 0.001667 
# stopped after 100 iterations
# # weights:  55
# initial  value 350.150881 
# iter  10 value 9.501382
# iter  20 value 7.602655
# iter  30 value 2.168889
# iter  40 value 0.237431
# iter  50 value 0.135200
# iter  60 value 0.066097
# iter  70 value 0.045236
# iter  80 value 0.019389
# iter  90 value 0.013638
# iter 100 value 0.010498
# final  value 0.010498 
# stopped after 100 iterations
# # weights:  55
# initial  value 192.387992 
# iter  10 value 9.142681
# iter  20 value 5.797266
# iter  30 value 3.432828
# iter  40 value 3.136977
# iter  50 value 3.133097
# iter  60 value 3.132719
# final  value 3.132401 
# converged
# # weights:  55
# initial  value 264.617816 
# iter  10 value 8.924624
# iter  20 value 6.410274
# iter  30 value 5.678271
# iter  40 value 5.578036
# iter  50 value 5.559772
# iter  60 value 5.476491
# iter  70 value 5.382522
# iter  80 value 5.369897
# iter  90 value 5.201027
# iter 100 value 5.195178
# final  value 5.195178 
# stopped after 100 iterations
# # weights:  55
# initial  value 289.797517 
# iter  10 value 3.939156
# iter  20 value 2.478655
# iter  30 value 1.258635
# iter  40 value 0.331142
# iter  50 value 0.221537
# iter  60 value 0.147256
# iter  70 value 0.116499
# iter  80 value 0.096255
# iter  90 value 0.085380
# iter 100 value 0.082455
# final  value 0.082455 
# stopped after 100 iterations
# # weights:  55
# initial  value 346.126435 
# iter  10 value 9.170625
# iter  20 value 6.762731
# iter  30 value 4.994899
# iter  40 value 3.659390
# iter  50 value 1.364727
# iter  60 value 0.870370
# iter  70 value 0.832237
# iter  80 value 0.812817
# iter  90 value 0.748323
# iter 100 value 0.574561
# final  value 0.574561 
# stopped after 100 iterations

#realizar algunas evaluaciones del modelo generado
#podria ser plots residuales etc.

#guardar modelo

saveRDS(m, file='trainSL_SOC_prof_effec.rds')

#m <- readRDS('trainSL_SOC_prof_effec.rds')

#predicciones del modelo
mp = predict(m)

#saveRDS(m, file='trainSL_SOC_prof_effec_predicted.rds')

#cargar modelo

#model = readRDS('trainSL_SOC_prof_effec.rds')

# resumen del modelo

summary(m@spModel$learner.model$super.model$learner.model)

#prediction = readRDS('trainSL_SOC_prof_effec_predicted.rds')

spplot(mp$pred[,c("response","q.lwr","q.upr")])

dim(dat)[1]

mean(dat$soc.t.ha)
sd(dat$soc.t.ha)

cellStats(expm1(stack(mp$pred[,c("response")])), mean)
cellStats(expm1(stack(mp$pred[,c("response")])), sd)


