RESOLUCIÓN DEL EXAMEN PARCIAL
================
GRUPO 4
5/8/2021

|              Integrante              |  Código  |
| :----------------------------------: | :------: |
|      Chavez Chipana Joel Jorge       | 17160041 |
|         Coba Bautista Kevin          | 17160042 |
| Conislla Huamanyalli Rocio del Pilar | 17160181 |
|   Silvestre Jimenez Brenda Pamela    | 17160045 |
|   Tuanama Satalaya Pierina Isabel    | 17160054 |

-----

## INDICE

[**PARTE 1**](#parte1)

[**PARTE 2**](#parte2)

[**PARTE 3**](#parte3)

<style type="text/css">
.badCode {
background-color: ligthgray;
}
</style>

## PARTE 1

1.  Se tiene una variable x (no necesariamente temperatura) que depende
    de la elevación. Se sabe que entre los 1000 y 3000 metros, esta
    variable se ve reducido en 2 unidades cada 500 metros. Entre los
    3000 y 4000 metros, varía en 0.5 unidades, y a una altitud mayor, su
    valor es constante. Cree una función que permita obtener el valor de
    esta variable, únicamente con el dato de la elevación.

<!-- end list -->

  - El valor de la variable x a 1000 metros es de 81.4 unidades.

Solución:

``` r badCode
funcion <- function(h, x = 81.4){
  if (h == 1000){
    var1 = 81.4
    return(var1)
  } else if (h<=3000 & h> 1000){
    var1 = x - 2*((h-1000)%/%500)
    return(var1)
  } else if (h > 3000 & h<=4000){
    var1 = x - 8 - 0.5*((h-3000)%/%500)
    return(var1)
  } else if (h > 4000){
    var1 = x - 9
    return(var1)
  } else {
    print("error!")
  }
}

funcion(1000)
```

    ## [1] 81.4

``` r badCode
funcion(1500)
```

    ## [1] 79.4

``` r badCode
funcion(5000)
```

    ## [1] 72.4

``` r badCode
funcion(999)
```

    ## [1] "error!"

2.  Resolver el siguiente sistema de ecuaciones.

\[
3a+2b-2c=0\\
2a-1b+3c=9\\
a+4b+2c=-4
\]

Solución:

``` r badCode
k <- matrix(c(3,2,1,2,-1,4,-2,3,2), ncol=3, nrow=3)
l <- c(0,9,-4)
solve(k,l)
```

    ## [1]  2 -2  1

## PARTE 2

1.  A partir de los datos adjuntos, se solicita lo siguiente:

<!-- end list -->

1.  Calcular la precipitación acumulada anual (Valores observados) para
    la cuenca asignada.

Solución:

``` r badCode
# Primero debemos cargar la data a trabajar
data1 <- as_tibble(read.csv("mods_clima_uh.csv"))

ppacmu <- data1 %>% dplyr::select(uh_name, bh_month , bh_pc , bh_esc) %>% 
  dplyr::filter(uh_name == "Cuenca Tumbes" , bh_esc == "Observado")%>% 
  summarise(ppacum = sum(bh_pc, na.rm = T))
ppacmu
```

    ## # A tibble: 1 x 1
    ##   ppacum
    ##    <dbl>
    ## 1   852.

2.  Calcular el porcentaje de sesgo (% PBIAS) de los escenarios
    climàticos(ACCESS, HADGEM2 , MPI) respecto a los datos observados
    para cada mes ( enero- diciembre) de cada variable, para la cuenca
    asignada.

Solución:

``` r badCode
data2 <- data1 %>% dplyr::select(uh_name, bh_month , bh_pc , bh_esc) %>% 
  dplyr::filter(uh_name == "Cuenca Tumbes") %>%
  pivot_wider(names_from = "bh_esc", values_from = "bh_pc")
library(hydroGOF)
(OB_ACCES <-pbias(data2$`ACCESS 1.0` , data2$Observado , na.rm = T))
```

    ## [1] 25.9

``` r badCode
(OB_HadGE <-pbias(data2$`HadGEM2-ES` , data2$Observado , na.rm = T))
```

    ## [1] 13.8

``` r badCode
(OB_MPIEM <-pbias(data2$`MPI-ESM-LR` , data2$Observado , na.rm = T))
```

    ## [1] -1.5

3.  De la pregunta anterior, ¿Cuál es el escenario climático más
    preciso? Fundamente su respuesta.

Solución:

  - El modelo mejor estiamdo es el MPI-ESM-LR ya que tiene un valor
    cercano a 0 que es el optimo pero su valor negativo nos indica una
    subestimacion del modelo.

<!-- end list -->

4.  Graficar, con ggplot2, la precipitación (enero a diciembre)
    observada y modelos climáticos.

Solución:

``` r badCode
cuenca <- data1 %>% dplyr::select(uh_name, bh_month , bh_pc , bh_esc) %>% 
  dplyr::filter(uh_name == "Cuenca Tumbes")%>% 
  rename(Cuenca = uh_name , month = bh_month , pp = bh_pc, Escenario = bh_esc)

#Gráfico con boxplot
ggplot(data = cuenca ,mapping = aes(x = Escenario, y = pp, fill = Escenario)) + 
  geom_boxplot()+
  labs(title = "Precipitación de Enero a Diciembre",  x = "Escenarios",  y = "Precipitación")+
  scale_fill_manual(values=heat.colors(4))+
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "beige"),
    panel.grid.minor = element_line(linetype = "dotted")
  )+
  labs(x= "Meses", y ="Temperatura")
```

![](resolucion_PARCIAL_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r badCode
#Gráfico con lineas y puntos
ggplot(cuenca, 
       mapping = aes(x = month, 
                     y = pp,
                     color = Escenario))+
  geom_line()+
  scale_x_continuous(
    breaks = c(1:12),
    labels = month.abb,
  )+
  theme(
    panel.background = element_rect(fill = "beige"),
    panel.grid.minor = element_line(linetype = "dotted")
  )+
  ggtitle("PRECIPITACION DE ENERO A DICIEMBRE")+
  labs(x = "Meses", 
       y = "Precipitación")
```

![](resolucion_PARCIAL_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

## PARTE 3

1.  Se tiene el conjunto de datos de temperatura diaria (período 1928 -
    2015) de ciertas estaciones meteorológicas (temperatureDataset.csv),
    donde cada una de estas están asociadas a un código único (p.e.
    qc00000208). Si existen valores iguales a -99.9, considerarlos como
    missing values y convertirlos a NA.

1.a. Determine la cantidad de missing values para los años hidrológicos
Sep1983-Agos1984 y Sep1997-Agos1998.

Solución:

``` r badCode
#Cargamos el dataset de temperatura
data3 <- as_tibble(read.csv("temperatureDataset.csv", na.strings = -99.9))

data4 <- data3 %>% select(DATE, qc00000805)%>%
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y"))%>% 
  rename(temp = qc00000805)%>% 
  arrange(DATE)

data4 %>% dplyr::filter(DATE >="1983-09-01" , DATE <="1984-08-31" |DATE >="1997-09-01" , DATE <="1997-09-01") %>% 
  summarise( miss_invalue =sum(is.na(temp)))
```

    ## # A tibble: 1 x 1
    ##   miss_invalue
    ##          <int>
    ## 1            1

1.b. Calcule la serie de tiempo de temperatura mensual (si el de días
con missing values, en un mes, supera el 5%, la temperatura mensual
sería considerado como un NA). Además, identifique, visualmente,
posibles valores atípicos y describa una posible causa.

Solución:

``` r badCode
data6 <- data4 %>% 
  group_by(DATE = str_sub(DATE, 1, 7)) %>% 
  mutate(MissVal = sum(is.na(temp))*100/n()) %>% 
  summarise(
    temp = mean(temp, na.rm = T),
    MissVal = unique(MissVal)
  ) %>% 
  mutate(
    temp = ifelse(MissVal >=5, NA, temp),
    DATE = as.Date(sprintf("%1$s-01", DATE)),
    month = str_sub(DATE, 6, 7)
  )
ggplot(data = data6, mapping = aes(x = month, y = temp, fill = month)) + 
  geom_boxplot()+
  scale_fill_manual(values=rainbow(12))+
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "beige"),
    panel.grid.minor = element_line(linetype = "dotted")
  )+
  scale_x_discrete(
    labels = month.abb,
  )+
  labs(x= "Meses", y ="Temperatura")
```

![](resolucion_PARCIAL_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

1.c. Determine la cantidad de missing values de la serie de tiempo a
paso mensual para los años 2005 y 2010.

Solución:

``` r badCode
(data5 <- data4 %>% 
  group_by(DATE = str_sub(DATE, 1, 7)) %>% 
  mutate(MissVal = sum(is.na(temp))) %>% 
  summarise(
    temp = mean(temp, na.rm = T),
    MissVal = unique(MissVal)
  ) %>% 
  dplyr::filter(DATE >= "2005-01-01" & DATE < "2005-12-31" |
                  DATE >= "2010-01-01" & DATE < "2010-12-31") %>% 
  mutate(
    DATE = as.Date(sprintf("%1$s-01", DATE)),
    month = str_sub(DATE, 6, 7)
  ))
```

    ## # A tibble: 22 x 4
    ##    DATE        temp MissVal month
    ##    <date>     <dbl>   <int> <chr>
    ##  1 2005-02-01  25.0       0 02   
    ##  2 2005-03-01  24.8       0 03   
    ##  3 2005-04-01  24.5       0 04   
    ##  4 2005-05-01  24.5       0 05   
    ##  5 2005-06-01  24.8       0 06   
    ##  6 2005-07-01  23.7       0 07   
    ##  7 2005-08-01  25.1       0 08   
    ##  8 2005-09-01  24.5       0 09   
    ##  9 2005-10-01  24.4       0 10   
    ## 10 2005-11-01  24.8       0 11   
    ## # ... with 12 more rows

1.d. Cree una función que calcule, a partir de los datos de temperatura
mensual, la climatología (Ene-Dic). Obtener la climatología para los
períodos 1980-1995 y 1996-2010. Plotear sus resultados en una sola
gráfica para describir sus diferencias y/o similitudes (entre
climatologías).

Solución:

``` r badCode
prueba1 <- function(data,year1,year2){
  data %>% mutate( año= as.integer(str_sub(DATE, 1,4))) %>% 
    dplyr::filter(año %in% year1:year2) %>% 
    group_by(month) %>% 
    summarise(tempmean =mean(temp,na.rm = T)) %>% 
    mutate(años = sprintf( "%1s - %1s" , year1 , year2))
  }

pur <-prueba1(data6,1980,1995)
par <-prueba1(data6,1996,2010)
ejer_f <-rbind(pur,par)

ggplot(ejer_f, 
       mapping = aes(x = month, 
                     y = tempmean,
                     group=años,
                     color= años))+
  geom_line()+
  scale_x_discrete(
    labels = month.abb,
  )+
  theme(
    panel.background = element_rect(fill = "beige"),
    panel.grid.minor = element_line(linetype = "dotted")
  )+
  ggtitle("Climatologia")+
  labs(x = "Meses", 
       y = "Temperatura (ºC)")
```

![](resolucion_PARCIAL_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

1.e. Plotear (boxplot) la variabilidad de los valores mensuales
(Ene-Dic) para el período 1980-2013 y describirlo correctamente.

Solución

``` r badCode
data6 %>%
  dplyr::filter(DATE >="1980-01-01" & DATE <="2013-12-31") %>%
  ggplot(mapping = aes(x =month, y= temp, fill=month))+
  geom_boxplot()+
  scale_fill_manual(values=terrain.colors(12))+
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "beige"),
    panel.grid.minor = element_line(linetype = "dotted")
  )+
  scale_x_discrete(
    labels = month.abb,
  )+
  labs(x= "Meses", y ="Temperatura")
```

![](resolucion_PARCIAL_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
