#Carga de datos
conexionIKEA<-dbConnect(odbc(),"RETO 4")
ikea<-dbReadTable(conexionIKEA, "muebles")

# RESUMEN -----------------------------------------------------------------
#1 CANTIDAD POR SEGMENTOS
#Estadisticos Medidas de Posicion
#Estadisticos Medidas de Dispersion
#Estadisticos Medidas de Forma
#Boxplot

# LIMPIAMOS OUTLIERS ------------------------------------------------------
ikea<-ikea%>%
  filter(!product_category_1_name=="UNKNOWN")
lillie.test(ikea$qty) #Se trata de una distribucion normal
Boxplot_Ikea<-
  plot_ly(ikea, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Ikea",
    yaxis = list(
      title = "Cantidad Articulos"))
ikea<-ikea%>%
  filter(qty>=1&qty<=300) #Eliminamos valores que consideramos outliers


# Segmentamos Clientes  y Sur-Norte -------------------------------------------------

ikea<-ikea%>%
  mutate(SegmentoCliente = case_when(
    between(qty, 1, 2) ~ "Comprador_Puntual",
    between(qty, 2, 10) ~ "Familias",
    between(qty, 10, 50) ~ "Mudanza",
    qty> 50 ~ "Empresas"))

table(ikea$SegmentoCliente)

Ikea_Norte<-ikea%>%
  filter(store=="Norte")
Ikea_Sur<-ikea%>%
  filter(store=="Sur")

Compradores_Puntuales_Norte<-ikea%>%
  filter(SegmentoCliente=="Comprador_Puntual"& store=="Norte")
Compradores_Puntuales_Sur<-ikea%>%
  filter(SegmentoCliente=="Comprador_Puntual"& store=="Sur")

Familias_Norte<-ikea%>%
  filter(SegmentoCliente=="Familias"& store=="Norte")
Familias_Sur<-ikea%>%
  filter(SegmentoCliente=="Familias"& store=="Sur")

Mudanza_Norte<-ikea%>%
  filter(SegmentoCliente=="Mudanza"&store=="Norte")
Mudanza_Sur<-ikea%>%
  filter(SegmentoCliente=="Mudanza"&store=="Sur")

Empresas_Norte<-ikea%>%
  filter(SegmentoCliente=="Empresas"&store=="Norte")
Empresas_Sur<-ikea%>%
  filter(SegmentoCliente=="Empresas"&store=="Sur")

# Analisis General --------------------------------------------------------
#NORTE
frecuencias <- data.frame(table(Ikea_Norte$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_Norte<-  Ikea_Norte %>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_Norte=as.data.frame(quantile(Ikea_Norte$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                      0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Ikea_Norte$qty, probs = 0.25)
Q3=quantile(Ikea_Norte$qty, probs = 0.75)
Cuartiles_Norte<-as.data.frame(rbind(Q1, Q3))

IQR<-round(IQR(Ikea_Norte$qty),2)
Medidas_Dispersion_Norte<-Ikea_Norte%>%
  summarise(
    IQR=IQR,
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_Norte<-round(Medidas_Dispersion_Norte,2)

Medidas_Forma_Norte<-Ikea_Norte%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_Norte<-round(Medidas_Forma_Norte,2)

#Graficamos
Boxplot_Ikea_Norte<-
  plot_ly(Ikea_Norte, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Ikea Norte",
    yaxis = list(
      title = "Cantidad Articulos") )

HistIkeaNorte <- plot_ly(Ikea_Norte, x = ~qty, type = "histogram", nbinsx = 60)%>%
  layout(
    title = "Histograma Ikea Norte",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))

#SUR
frecuencias <- data.frame(table(Ikea_Sur$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_Sur<-  Ikea_Sur %>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_Sur=as.data.frame(quantile(Ikea_Sur$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                  0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Ikea_Sur$qty, probs = 0.25)
Q3=quantile(Ikea_Sur$qty, probs = 0.75)
Cuartiles_Sur<-as.data.frame(rbind(Q1, Q3))

Medidas_Dispersion_Sur<-Ikea_Sur%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_Sur<-round(Medidas_Dispersion_Sur,2)

Medidas_Forma_Sur<-Ikea_Sur%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_Sur<-round(Medidas_Forma_Sur,2)

#Graficamos
Boxplot_Ikea_Sur<-
  plot_ly(Ikea_Sur, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Ikea Sur",
    yaxis = list(
      title = "Cantidad Articulos"
    )
  )

HistIkeaSur <- plot_ly(Ikea_Sur, x = ~qty, type = "histogram", nbinsx = 60)%>%
  layout(
    title = "Histograma Ikea Sur",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))

# Analisis Compradores Puntuales ------------------------------------------
#NORTE
frecuencias <- data.frame(table(Compradores_Puntuales_Norte$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_CP_Norte<-  Compradores_Puntuales_Norte %>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_CP_Norte=as.data.frame(quantile(Compradores_Puntuales_Norte$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                  0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Compradores_Puntuales_Norte$qty, probs = 0.25)
Q3=quantile(Compradores_Puntuales_Norte$qty, probs = 0.75)
Cuartiles_CP_Norte<-as.data.frame(rbind(Q1, Q3))

Medidas_Dispersion_CP_Norte<-Compradores_Puntuales_Norte%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_CP_Norte<-round(Medidas_Dispersion_CP_Norte,2)

Medidas_Forma_CP_Norte<-Compradores_Puntuales_Norte%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_CP_Norte<-round(Medidas_Forma_CP_Norte,2)

#GRAFICAMOS
HistCPNorte <- plot_ly(Compradores_Puntuales_Norte, x = ~qty, type = "histogram", nbinsx = 10)%>%
  layout(
    title = "Histograma Compradores Puntuales Ikea Norte",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))

#SUR
frecuencias <- data.frame(table(Compradores_Puntuales_Sur$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_CP_Sur<-  Compradores_Puntuales_Sur %>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_CP_Sur=as.data.frame(quantile(Compradores_Puntuales_Sur$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                              0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Compradores_Puntuales_Sur$qty, probs = 0.25)
Q3=quantile(Compradores_Puntuales_Sur$qty, probs = 0.75)
Cuartiles_CP_Sur<-as.data.frame(rbind(Q1, Q3))

Medidas_Dispersion_CP_Sur<-Compradores_Puntuales_Sur%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_CP_Sur<-round(Medidas_Dispersion_Sur,2)

Medidas_Forma_CP_Sur<-Compradores_Puntuales_Sur%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_CP_Sur<-round(Medidas_Forma_CP_Sur,2)

#GRAFICAMOS
HistCPSur <- plot_ly(Compradores_Puntuales_Sur, x = ~qty, type = "histogram", nbinsx = 10)%>%
  layout(
    title = "Histograma Compradores Puntuales Ikea Sur",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))

# Analisis Familias -------------------------------------------------------
#NORTE
frecuencias <- data.frame(table(Familias_Norte$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_Familias_Norte<-Familias_Norte%>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_Familias_Norte=as.data.frame(quantile(Familias_Norte$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                               0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Familias_Norte$qty, probs = 0.25)
Q3=quantile(Familias_Norte$qty, probs = 0.75)
Cuartiles_Familias_Norte<-as.data.frame(rbind(Q1, Q3))
IQR<-round(IQR(Familias_Norte$qty),2)
Medidas_Dispersion_Familias_Norte<-
  Familias_Norte%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_Familias_Norte<-round(Medidas_Dispersion_Familias_Norte,2)

Medidas_Forma_Familias_Norte<-Familias_Norte%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_Familias_Norte<-round(Medidas_Forma_Familias_Norte,2)

#Graficamos
Boxplot_Familias_Norte<-
  plot_ly(Familias_Norte, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Ikea Familias Norte",
    yaxis = list(
      title = "Cantidad Articulos"
    )
  )
Hist_Familias_Norte <- plot_ly(Familias_Norte, x = ~qty, type = "histogram", nbinsx = 30)%>%
  layout(
    title = "Histograma Familias Ikea Norte",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))
#SUR 
frecuencias <- data.frame(table(Familias_Sur$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_Familias_Sur<-Familias_Sur%>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_Familias_Sur=as.data.frame(quantile(Familias_Sur$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                           0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Familias_Sur$qty, probs = 0.25)
Q3=quantile(Familias_Sur$qty, probs = 0.75)
Cuartiles_Familias_Sur<-as.data.frame(rbind(Q1, Q3))

Medidas_Dispersion_Familias_Sur<-
  Familias_Sur%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_Familias_Sur<-round(Medidas_Dispersion_Familias_Sur,2)

Medidas_Forma_Familias_Sur<-Familias_Sur%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_Familias_Sur<-round(Medidas_Forma_Familias_Sur,2)

#Graficamos
Boxplot_Familias_Sur<-
  plot_ly(Familias_Sur, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Ikea Familias Sur",
    yaxis = list(
      title = "Cantidad Articulos"
    )
  )
Hist_Familias_Sur <- plot_ly(Familias_Sur, x = ~qty, type = "histogram", nbinsx = 30)%>%
  layout(
    title = "Histograma Familias Ikea Sur",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))
# Analisis Mudanza --------------------------------------------------------

#NORTE
frecuencias <- data.frame(table(Mudanza_Norte$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_Mudanza_Norte<-  Mudanza_Norte %>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_Mudanza_Norte=as.data.frame(quantile(Mudanza_Norte$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                             0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Mudanza_Norte$qty, probs = 0.25)
Q3=quantile(Mudanza_Norte$qty, probs = 0.75)
Cuartiles_Mudanza_Norte<-as.data.frame(rbind(Q1, Q3))
IQR<-round(IQR(Mudanza_Norte$qty),2)
Medidas_Dispersion_Mudanza_Norte<-Mudanza_Norte%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_Mudanza_Norte<-round(Medidas_Dispersion_Mudanza_Norte,2)

Medidas_Forma_Mudanza_Norte<-Mudanza_Norte%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_Mudanza_Norte<-round(Medidas_Forma_Mudanza_Norte,2)

#Graficamos
Boxplot_Mudanzas_Norte<-
  plot_ly(Mudanza_Norte, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Ikea Mudanza Norte",
    yaxis = list(
      title = "Cantidad Articulos"
    )
  )
Hist_Mudanzas_Norte <- plot_ly(Mudanza_Norte, x = ~qty, type = "histogram", nbinsx = 40)%>%
  layout(
    title = "Histograma Mudanza Ikea Norte",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))

#SUR 
frecuencias <- data.frame(table(Mudanza_Sur$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_Mudanza_Sur<-  Mudanza_Sur %>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_Mudanza_Sur=as.data.frame(quantile(Mudanza_Sur$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                         0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Mudanza_Sur$qty, probs = 0.25)
Q3=quantile(Mudanza_Sur$qty, probs = 0.75)
Cuartiles_Mudanza_Sur<-as.data.frame(rbind(Q1, Q3))

Medidas_Dispersion_Mudanza_Sur<-Mudanza_Sur%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_Mudanza_Sur<-round(Medidas_Dispersion_Mudanza_Sur,2)

Medidas_Forma_Mudanza_Sur<-Mudanza_Sur%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_Mudanza_Sur<-round(Medidas_Forma_Mudanza_Sur,2)

#Graficamos
Boxplot_Mudanzas_Sur<-
  plot_ly(Mudanza_Sur, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Ikea Mudanza Sur",
    yaxis = list(
      title = "Cantidad Articulos"
    )
  )
Hist_Mudanzas_Sur <- plot_ly(Mudanza_Sur, x = ~qty, type = "histogram", nbinsx = 40)%>%
  layout(
    title = "Histograma Mudanza Ikea Sur",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))

# Analisis Empresas -------------------------------------------------------

#NORTE
frecuencias <- data.frame(table(Empresas_Norte$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_Empresas_Norte<- Empresas_Norte%>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_Empresas_Norte=as.data.frame(quantile(Empresas_Norte$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                               0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Empresas_Norte$qty, probs = 0.25)
Q3=quantile(Empresas_Norte$qty, probs = 0.75)
Cuartiles_Empresas_Norte<-as.data.frame(rbind(Q1, Q3))
IQR<-round(IQR(Empresas_Norte$qty),2)
Medidas_Dispersion_Empresas_Norte<-Empresas_Norte%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_Empresas_Norte<-round(Medidas_Dispersion_Empresas_Norte,2)

Medidas_Forma_Empresas_Norte<-Empresas_Norte%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_Empresas_Norte<-round(Medidas_Forma_Empresas_Norte,2)

#Graficamos
Boxplot_Empresas_Norte<-
  plot_ly(Empresas_Norte, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Empresas Ikea Norte",
    yaxis = list(
      title = "Cantidad Articulos"
    )
  )
Hist_Empresas_Norte <- plot_ly(Empresas_Norte, x = ~qty, type = "histogram", nbinsx = 50)%>%
  layout(
    title = "Histograma Empresas Ikea Norte",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))

#SUR
frecuencias <- data.frame(table(Empresas_Sur$qty))
moda <- frecuencias[which.max(frecuencias$Freq),1]
Medidas_Posicion_Empresas_Sur<- Empresas_Sur%>%
  summarise(
    Max= max(qty),
    Min=min(qty),
    Media = mean(qty),
    Moda=moda,
    Mediana= median(qty))
Percentiles_Empresas_Sur=as.data.frame(quantile(Empresas_Sur$qty, prob = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                          0.6, 0.7, 0.8, 0.9, 1)))
Q1=quantile(Empresas_Sur$qty, probs = 0.25)
Q3=quantile(Empresas_Sur$qty, probs = 0.75)
Cuartiles_Empresas_Sur<-as.data.frame(rbind(Q1, Q3))
IQR<-round(IQR(Empresas_Sur$qty),2)
Medidas_Dispersion_Empresas_Sur<-Empresas_Sur%>%
  summarise(
    Varianza=var(qty),
    Desviacion=sd(qty),
    coeficiente_de_variacion=sd(qty)/mean(qty))
Medidas_Dispersion_Empresas_Sur<-round(Medidas_Dispersion_Empresas_Sur,2)

Medidas_Forma_Empresas_Sur<-Empresas_Sur%>%
  summarise(
    Simetria=skewness(qty),
    Curtosis=kurtosis(qty))
Medidas_Forma_Empresas_Sur<-round(Medidas_Forma_Empresas_Sur,2)

#Graficamos
Boxplot_Empresas_Sur<-
  plot_ly(Empresas_Sur, y=~qty, type='box', text=~qty)%>% 
  layout(
    title = "Boxplot de Empresas Ikea Sur",
    yaxis = list(
      title = "Cantidad Articulos"
    )
  )
Hist_Empresas_Sur <- plot_ly(Empresas_Sur, x = ~qty, type = "histogram", nbinsx = 50)%>%
  layout(
    title = "Histograma Empresas Ikea Sur",
    font=t,
    xaxis = list(
      title = "Cantidad"
    ),
    yaxis = list(
      title = "Freq"
    ))

