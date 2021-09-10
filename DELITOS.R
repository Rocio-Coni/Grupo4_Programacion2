
# Librerias y carga de archivos -------------------------------------------
rm(list = ls())
library(tidyverse)
library(psych)
library(PerformanceAnalytics)
library(GGally)
library(lattice)
library(cptcity)
library(ade4)
library(factoextra)
#Cargamos y eliminamos las columnas que no corresponden
dat1 <- read.csv("DEPARTAMENTOS1.csv", sep = ";")
saveRDS(dat1, file = "dataRDS.RDS") #Para guardar
# Modificado --------------------------------------------------------------
# #Eliminamos las filas que no tienen datos
# dat1 <- dat1[(1:26),] 
# 
# #Eliminamos las columnas con NA's mayor al 15%
# delete.na <- function(df, n=0) {
#   df[,colSums(is.na(df))*100/26 <= 15]
# }
# data2 <- delete.na(dat1)

# Cambio de nombre -----------------------------------------------------------------
view(dat1)
names(dat1)
names(dat1)[names(dat1)=="ï..Dep"] <- "Dep"
# Proceso -----------------------------------------------------------------
sc_data <- scale(dat1[,c(2:ncol(dat1))])
describe(dat1)
cov(sc_data)
cor_dt <- cor(sc_data)
chart.Correlation(sc_data, histogram = T, pch = 20)
#procedimiento para PCA
# ggpairs(as.data.frame(sc_data))
levelplot(
  as.matrix(cor_dt),
  col.regions =
    cpt(
      pal = "cb_div_RdBu_11", n = 100, rev = T
    )
)
#PCA
pca_d <- dudi.pca(
  sc_data, 
  scale = F, 
  scannf = F, 
  nf = ncol(sc_data)
)

summary(pca_d)
# Valores
pca_d$eig
sum(pca_d$eig)
# Vectores
pca_d$c1
# Scree plot
fviz_eig(pca_d, addlabels = T)

levelplot(
  as.matrix(pca_d$co),
  col.regions =
    cpt(
      pal = "cb_div_RdBu_11", n = 100, rev = T
    )
)


