rm(list = ls())
pacman::p_load(tidyverse, psych, PerformanceAnalytics, GGally,
               lattice, cptcity, ade4, factoextra)


# Cambiar nombres de variables -------------------------------------------------------------
data0 <- read.csv("DEPARTAMENTOS.csv", sep = ";")
data0
saveRDS(data1, file = "data0RDS.RDS") #Para guardar

#Primeramente reducimos el nombre de nuestras variables paara una mejor visualización 
names(data0) <- c("Dep","Pob","NEP",
                  "S_F","S_M","SNE",
                  "NE_I","NE_PC","ND_PI",
                  "NE_SC","NE_SI","NE_SnUC",
                  "NE_SnUI","NE_SUC","NE_SUI",
                  "NE_Pstgr","NE_NAp","DcP",
                  "DcSegP","DcL","DcVCS",
                  "DcF","DcAP","DcTP",
                  "DcOFM","DcFP","Dt",
                  "DcCBFN","DcH","Otros",
                  "E_18_19","E_20_24","E_25_29",
                  "E_30_34","E_35_39","E_40_44",
                  "E_45_49","E_50_59","E_60_")

View(data0)
tibble(data0)

# Eliminar NA -------------------------------------------------------------
data0 %>% mice::md.pattern(rotate.names=TRUE)
#Observando el grafico, se entiende que para la primera fila hay una observación que tiene NA en la variable DcH
#en la segunda fila,hay una observación que tiene NA en DcBFN y DcH, así sucesivamente
#hasta llegar a la ultima fila, donde una observación tiene 26 NA, el cual definitivamente se eliminará.


#data0 %>% VIM::aggr(numbers=T, sortVar=T )

#Eliminar filas
#Las variables principales que son sustento base de que existen los datos y son reales, vienen a ser cinco
#La variable poblaciom, establecimientos penitenciarios, mujeres y varones, es por ello que si alguno no tiene datos, procedera a ser eliminado

data0 <- data0[!is.na(data0$Pob),]
data0 <- data0[!is.na(data0$NEP),]
data0 <- data0[!is.na(data0$S_F),]
data0 <- data0[!is.na(data0$S_M),]

tibble(data0) %>% print(n=26)
#Como se observa, la cantidad de departamentos se redujo, se elimino al Callao, Moquegua, Madre de Dios

#Eliminar columnas
#Teniendo todos los departamentos con la data principal, se procede a analizar las varibles, teniendo en cuenta que no podemos trabajar con NA,
#se procederá a eliminar aquellas columnas que la contengan

#Examinamos cuantos NA tiene cada columna para darnos una idea
sapply(data0, function(x) sum(is.na(x))) #numero de nulos por columnas

#Eliminamos todas las columnas que tienen al menos un NA
colMeans(is.na(data0))
data1 <- data0[,-which(colMeans(is.na(data0)) > 0)]
tibble(data1) %>% print(n=26)
View(data1)
tibble(data1)
#De 39 variables se eliminaron 10
#Ahora verificamos que nuestra data no tenga NA
data1 %>% mice::md.pattern(rotate.names=TRUE) #verificando que no hay NA

#GRAFICO DE PASTEL
glimpse(data1)
total <- dplyr::select(data1,15:20)
suma <- colSums (total)
porcent <- cbind(suma,prop = prop.table(suma))*100
grafico <- data.frame(porcent)
grafico
etiquetas <- c("48.25% DcP","22.52% DcSegP","17.09% DcL","8.75% DcVCS","2.08% DcF","1.27% DcAP")

pie(grafico$prop , col=rainbow(length(grafico$prop)), 
    labels = etiquetas,
    main = "Delitos en Perú",
    sub = "Evaluación de la cantidad de delitos registrados por tipo.")

legend("topleft", 
       c("D. C. patrominio",
         "D. C. la seguridad publica",
         "D. C. la libertad",
         "D. C. la vida, el cuerpo y la salud",
         "D. C. la familia",
         "D. C. la administracion publica"),
       cex = 0.5,
       fill = rainbow(length(grafico$prop)))

#En el gráfico observamos que el Delito contra el patrimonio es el que más se da a nivel Nacional, seguido por el Delito contra la seguridad pública
#Estos dos abarcan casi el 70% del total ya que van de la mano, la inseguridad en las calles genera robos, extorsión etc
# DESCRIPCION DE LAS VARIABLES --------------------------------------------

# HISTOGRAMAS -------------------------------------------------------------
par(mfrow = c(3,2))
hist(data1$Pob)
hist(data1$NEP)
hist(data1$S_F)
hist(data1$S_M)
hist(data1$SNE)
hist(data1$NE_PC)

par(mfrow = c(3,2))
hist(data1$ND_PI)
hist(data1$NE_SC)
hist(data1$NE_SI)
hist(data1$NE_SnUC)
hist(data1$NE_SnUI)
hist(data1$NE_SUC)

par(mfrow = c(3,2))
hist(data1$NE_SUI)
hist(data1$DcP)
hist(data1$DcSegP)
hist(data1$DcL)
hist(data1$DcVCS)
hist(data1$DcF)

par(mfrow = c(3,2))
hist(data1$DcAP)
hist(data1$E_18_19)
hist(data1$E_20_24)
hist(data1$E_25_29)
hist(data1$E_30_34)
hist(data1$E_35_39)

par(mfrow = c(2,2))
hist(data1$E_40_44)
hist(data1$E_45_49)
hist(data1$E_50_59)
hist(data1$E_60_)


# ANALISIS POR VARIABLES --------------------------------------------------
data2 <- tibble(select(data1, 1,15:20))
data2
ggplot(data=data2, aes(x=Dep, y=DcP, fill= Dep)) + 
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 20, size = 6,hjust = 1, vjust = 1))+
    ggtitle ("Delitos contra el Patrimonio")+
    theme (plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del título
                                     hjust=0.5, #Justificación vertical, para separarlo del gráfico
                                     face="bold"))+ #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
    labs(x = "Departamentos",y = "Cantidad de Delitos")

ggplot(data=data2, aes(x=Dep, y=DcSegP, fill= Dep)) + 
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 20, size = 6,hjust = 1, vjust = 1))+
    ggtitle ("Delitos contra la Seguridad Publica")+
    theme (plot.title = element_text(size=rel(2), 
                                     hjust=0.5, 
                                     face="bold"))+ 
    labs(x = "Departamentos",y = "Cantidad de Delitos")

ggplot(data=data2, aes(x=Dep, y=DcL, fill= Dep)) + 
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 20, size = 6,hjust = 1, vjust = 1))+
    ggtitle ("Delitos contra la Libertad")+
    theme (plot.title = element_text(size=rel(2), 
                                     hjust=0.5, 
                                     face="bold"))+ 
    labs(x = "Departamentos",y = "Cantidad de Delitos")

ggplot(data=data2, aes(x=Dep, y=DcVCS, fill= Dep)) + 
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 20, size = 6,hjust = 1, vjust = 1))+
    ggtitle ("Delitos contra la Vida el Cuerpo y la Salud")+
    theme (plot.title = element_text(size=rel(2), 
                                     hjust=0.5, 
                                     face="bold"))+ 
    labs(x = "Departamentos",y = "Cantidad de Delitos")

ggplot(data=data2, aes(x=Dep, y=DcF, fill= Dep)) + 
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 20, size = 6,hjust = 1, vjust = 1))+
    ggtitle ("Delitos contra la Familia")+
    theme (plot.title = element_text(size=rel(2), 
                                     hjust=0.5, 
                                     face="bold"))+ 
    labs(x = "Departamentos",y = "Cantidad de Delitos")

ggplot(data=data2, aes(x=Dep, y=DcAP, fill= Dep)) + 
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 20, size = 6,hjust = 1, vjust = 1))+
    ggtitle ("Delitos contra la Administración Publica")+
    theme (plot.title = element_text(size=rel(2), 
                                     hjust=0.5, 
                                     face="bold"))+ 
    labs(x = "Departamentos",y = "Cantidad de Delitos")



