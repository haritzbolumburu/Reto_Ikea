
# CONEXION SQL-R ----------------------------------------------------------

conexionIKEA<-dbConnect(odbc(),"RETO 4")

######################################################
#Carga de datos
conexionIKEA<-dbConnect(odbc(),"RETO 4")
ikea<-dbReadTable(conexionIKEA, "muebles")
ikea<-ikea%>%
  filter(!product_category_1_name=="UNKNOWN")

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
#GUARDAMOS BOXPLOTS
setwd("./GRAFICOS")
saveWidget(Boxplot_Ikea, "Boxplot_Ikea.html", selfcontained = T)
saveWidget(Boxplot_Ikea_Norte, "Boxplot_Ikea_Norte.html", selfcontained = T)
saveWidget(Boxplot_Ikea_Sur, "Boxplot_Ikea_Sur.html", selfcontained = T)
saveWidget(Boxplot_Familias_Norte, "Boxplot_Familias_Norte.html", selfcontained = T)
saveWidget(Boxplot_Familias_Sur, "Boxplot_Familias_Sur.html", selfcontained = T)
saveWidget(Boxplot_Mudanzas_Norte, "Boxplot_Mudanzas_Norte.html", selfcontained = T)
saveWidget(Boxplot_Mudanzas_Sur, "Boxplot_Mudanzas_Sur.html", selfcontained = T)
saveWidget(Boxplot_Empresas_Norte, "Boxplot_Empresas_Norte.html", selfcontained = T)
saveWidget(Boxplot_Empresas_Sur, "Boxplot_Empresas_Sur.html", selfcontained = T)

#GUARDAMOS HISTOGRAMAS
saveWidget(HistIkeaNorte, "HistIkeaNorte.html", selfcontained = T)
saveWidget(HistIkeaSur, "HistIkeaSur.html", selfcontained = T)
saveWidget(HistCPNorte, "HistCPNorte.html", selfcontained = T)
saveWidget(HistCPSur, "HistCPSur.html", selfcontained = T)
saveWidget(Hist_Familias_Norte, "Hist_Familias_Norte.html", selfcontained = T)
saveWidget(Hist_Familias_Sur, "Hist_Familias_Sur.html", selfcontained = T)
saveWidget(Hist_Mudanzas_Norte, "Hist_Mudanzas_Norte.html", selfcontained = T)
saveWidget(Hist_Mudanzas_Sur, "Hist_Mudanzas_Sur.html", selfcontained = T)
saveWidget(Hist_Empresas_Norte, "Hist_Empresas_Norte.html", selfcontained = T)
saveWidget(Hist_Empresas_Sur, "Hist_Empresas_Sur.html", selfcontained = T)

#######################################################################
# GENERAL CATEGORIA 3
#######################################################################


# Consulta desde SQL ------------------------------------------------------


consulta3<-dbGetQuery(conexionIKEA,"Select membership_id, product_category_3_name, sum(qty) from muebles group by membership_id, product_category_3_name" )

# limpiamos los datos 

summary(consulta3$`sum(qty)`)

boxplot_total<-plot_ly(consulta3, y=~`sum(qty)`, type = "box")%>%layout(title="Boxplot cantidad")
saveWidget(boxplot_total, "boxplot_total.html", selfcontained=T)

quantile(consulta3$`sum(qty)`, probs = 0.999)

consulta3<-filter(consulta3, `sum(qty)` <= quantile(consulta3$`sum(qty)`, probs = 0.999))

consulta3<-filter(consulta3, `sum(qty)` >=1)

boxplot_total_filtrado<-plot_ly(consulta3, y=~`sum(qty)`, type = "box")%>%layout(title="Boxplot cantidad")
saveWidget(boxplot_total_filtrado, "boxplot_total_filtrado.html", selfcontained=T)

min(consulta3$`sum(qty)`)

max(consulta3$`sum(qty)`)

recomspread3<-spread(consulta3 , product_category_3_name, `sum(qty)`)

# quitamos la instancia de los que no son socios

recomspread3<-recomspread3[-1,]

# quitamos la variable unknown y membershipid

recomspread3<-recomspread3[,-c(1,207)]

# corregir los titulos de las variables

names(recomspread3)<-str_replace_all(names(recomspread3), "�s", "s")

names(recomspread3)<-str_replace_all(names(recomspread3), "'s", "s")

names(recomspread3)<-str_replace_all(names(recomspread3), "&", "and")

names(recomspread3)<-str_replace_all(names(recomspread3), "\\.", ",")

names(recomspread3)<-str_remove_all(names(recomspread3),",$")

names(recomspread3)<-str_remove_all(names(recomspread3),",$")

names(recomspread3)

#quitar filas con muchos NA y poca variedad de categoria
# por lo menos 7 categorias compradas

miss_case_summary(recomspread3)

miss3<-miss_case_summary(recomspread3)

miss3<-filter(miss3, n_miss>204)

nrow(miss3) # hay 36606 variables con mas de 204 missings

quitar3<-miss3$case

recomspread3<-recomspread3[-quitar3,]

# convertir en matriz

recomspread3<-as.matrix(recomspread3)

recomspread3<-as(recomspread3, "realRatingMatrix")





# EVALUACION MODELOS TOPNLIST -----------------------------------------------------

min(rowCounts(recomspread3))

set.seed(7)

eval_scheme <- evaluationScheme(recomspread3, method = "split", train = 0.9, given = 7,
                                goodRating = 1)

# lista de algoritmos a testear
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "POPULAR" = list(name="POPULAR", param= NULL),
              "UBCF" = list(name = "UBCF", param = NULL),
              "IBCF" = list(name = "IBCF", param = NULL),
              "SVD" =list(name= "SVD", param=NULL))



# evaluacion de modelos y n

set.seed(7)
eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,2,3,4,5,6,7,8,9,10))

plot(eval, legend = "topleft")
title("Curva ROC")
png(filename = "modelos_total_roc.png")
plot(eval, legend = "topleft")
title("Curva ROC")
dev.off()


plot(eval,"prec/rec")
title("Precision/Recall")
png(filename = "modelos_total_precrec.png")
plot(eval,"prec/rec")
title("Precision/Recall")
dev.off()


getConfusionMatrix(eval[["POPULAR"]])
getConfusionMatrix(eval[["SVD"]])

# SVD / POPULAR son los mejores

##### SELECCIONAR EL K , NN y DISTANCIA OPTIMO #####

# SVD


vector_k <- c(2,3,4,5,6,7)

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="SVD", param=list( k = k))
})

names(models_to_evaluate) <- paste0("SVD_k_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))


plot(list_results, annotate = 1, legend = "topleft") # el k optimo es 5-6-7
title("curva ROC")
png(filename = "svd_parametro_total_roc.png")
plot(list_results, annotate = 1, legend = "topleft") 
title("curva ROC")
dev.off()


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # el k optimo es 5-6
title("Precision/Recall")
png(filename = "svd_parametro_total_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision/Recall")
dev.off()


# nos quedamos con el 6 porq es algo mejor que el 5 y hace el efecto bañera en el 
# grafico prec/rec. Tiene tambienen valores parecidos a 7 con que puede ser el intermedio
# se ejecuta rapido 
# k pequeños por la factorizacion de matrices


# UBCF

vector_k <- c(2,3,4,5,7,10,20,30,40,50)

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="UBCF", param=list( nn = k))
})

names(models_to_evaluate) <- paste0("UBCF_nn_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))


plot(list_results, annotate = 1, legend = "topleft") # el n optimo es 2-3
title("curva ROC")
png(filename = "ubcf_parametro_total_roc.png")
plot(list_results, annotate = 1, legend = "topleft") 
title("curva ROC")
dev.off()


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # el k optimo es 2-3
title("Precision/Recall")
png(filename = "ubcf_parametro_total_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision/Recall")
dev.off()


# nn = 2
# tarda en ejecutarse
# peor TPR y precision con n bajos



# IBCF

vector_k <- c("euclidean", "canberra","pearson", "cosine")

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="IBCF", param=list( method = k))
})

names(models_to_evaluate) <- paste0("IBCF_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))


plot(list_results, annotate = 1, legend = "topleft")# la distancia optima es pearson-cosine
title("curva ROC")
png(filename = "ibcf_parametro_total_roc.png")
plot(list_results, annotate = 1, legend = "topleft")
title("curva ROC")
dev.off()


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # la distancia optima es cosine
title("Precision/Recall")
png(filename = "ibcf_parametro_total_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision/Recall")
dev.off()


# method cosine
# tarda en ejecutarse
# peor TPR y precision con n bajos




# COMPARAMOS EL POPULAR, SVD K=6 , UBCF NN=2 y IBCF = COSINE


set.seed(7)

eval_scheme <- evaluationScheme(recomspread3, method = "split", train = 0.9, given = 7,
                                goodRating = 1)

# lista de algoritmos a testear
algos <- list("POPULAR" = list(name="POPULAR", param= NULL),
              "UBCF_2" = list(name = "UBCF", param = list(nn=2)),
              "IBCF_C" = list(name = "IBCF", param = list(method="cosine")),
              "SVD_6" =list(name= "SVD", param=list(k=6)))


# evaluacion de modelos y n

set.seed(7)
eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,2,3,4,5,6,7,8,9,10))


plot(eval, legend = "topleft")
title("Curva ROC")
png(filename = "modelos_idoneos_total_roc.png")
plot(eval, legend = "topleft")
title("Curva ROC")
dev.off()



plot(eval,"prec/rec")
title("Precision/Recall")
png(filename = "modelos_idoneos_total_precrec.png")
plot(eval,"prec/rec")
title("Precision/Recall")
dev.off()



getConfusionMatrix(eval[["SVD_6"]])
getConfusionMatrix(eval[["UBCF_2"]])
getConfusionMatrix(eval[["POPULAR"]])
getConfusionMatrix(eval[["IBCF_C"]])

# con valores n bajos el svd tienen mejores rates y con altos el ubcf
# el ubcf tarda mucho mas y el svd es el modelo mas rapido
# nos intersa el svd y el popular que son mas rapidos y con menores rates
# en valores n bajos



# EVALUACION RATINGS ------------------------------------------------------

min(rowCounts(recomspread3))

set.seed(7)

e <- evaluationScheme(recomspread3,
                      method="split", train=0.9,
                       k=1,given=7)


nrow(getData(e, "train"))/nrow(recomspread3)# % para train
nrow(getData(e, "known"))/nrow(recomspread3)# el % para test
nrow(getData(e, "unknown")) # para contrastar
unique(rowCounts(getData(e, "known"))) # 7 articulos por usuario


train<-as(getData(e, "train"),"matrix")

known<-as(getData(e, "known"),"matrix")

unknown<-as(getData(e, "unknown"),"matrix")




#USER BASED

r <- Recommender(getData(e, "train"), "UBCF", param=list(nn=2))

p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
USERBASED<-calcPredictionAccuracy(p, getData(e, "unknown"))




#POPULAR

r <- Recommender(getData(e, "train"), "POPULAR")


p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
POPULAR<-calcPredictionAccuracy(p, getData(e, "unknown"))


#ITEM-BASED

r <- Recommender(getData(e, "train"), "IBCF", param=list(method="cosine"))


p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
ITEMBASED<-calcPredictionAccuracy(p, getData(e, "unknown"))


# SVD

r <- Recommender(getData(e, "train"), "SVD", param=list(k=6))

p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
SVD<-calcPredictionAccuracy(p, getData(e, "unknown"))





#RESULTADOS DE LOS 3 MODELOS
####################################################

USERBASED<-as.matrix(USERBASED)
USERBASED



POPULAR<-as.matrix(POPULAR)
POPULAR



ITEMBASED<-as.matrix(ITEMBASED)
ITEMBASED

SVD<-as.matrix(SVD)
SVD


#MATRIZ DE ACCURACY

colnames(USERBASED) <- "USERBASED"

colnames(POPULAR) <- "POPULAR"

colnames(ITEMBASED) <- "ITEMBASED"

colnames(SVD) <- "SVD"

MATRIZ3<-cbind(USERBASED,POPULAR,ITEMBASED,SVD)
MATRIZ3

# el popular da el mejor, despues el svd y despues el user based
# por ultimo el item based da errores altos



# OTRAS EVALUACIONES ------------------------------------------------------

# user coverage - item coverage - novedity



################# SVD

#USER COVERAGE

#vamos a definir esta funcion
#para ver cuantos valores diferentes de NA hay en las predicciones
r <- Recommender(getData(e, "train"), "SVD", param=list(k=6))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%


# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor


################ UBCF

#USER COVERAGE

r <- Recommender(getData(e, "train"), "UBCF", param=list(nn=2))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%

# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor

############### POPULAR

#USER COVERAGE

r <- Recommender(getData(e, "train"), "POPULAR")
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%


## ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor

# todos dan el user coverage al 100% con que no es representativo
# el user based parece tener mejor item coverage aunque la distribucion 
# sigue estando a la izquierda


################ IBCF

#USER COVERAGE

r <- Recommender(getData(e, "train"), "IBCF", param=list(method="cosine"))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%

# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor


###### NOVEDITY

# SVD

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                              method="POPULAR")

rsvd<-Recommender(getData(e, "train"),
                 method="SVD", param=list(k=6))

popular2 <- predict(popular, getData(e, "known"),
                             type="topNList")

rsvd2 <- predict(rsvd, getData(e, "known"),
                    type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rsvd2<-as(rsvd2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rsvd2,2,f1),0.75)
vuelta<-t(rsvd2) ; dim(vuelta)
summary(colSums(rsvd2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rsvd2,na.rm=TRUE)>=quantile(apply(rsvd2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50 

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 74% de nuevos



#IBCF

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                     method="POPULAR")

rib<-Recommender(getData(e, "train"),
                  method="IBCF", param=list(method="cosine"))

popular2 <- predict(popular, getData(e, "known"),
                    type="topNList")

rib2 <- predict(rib, getData(e, "known"),
                 type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rib2<-as(rib2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rib2,2,f1),0.75)
vuelta<-t(rib2) ; dim(vuelta)
summary(colSums(rib2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rib2,na.rm=TRUE)>=quantile(apply(rib2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 52% de nuevo




#UBCF

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                     method="POPULAR")

rib<-Recommender(getData(e, "train"),
                 method="UBCF", param=list(nn=2))

popular2 <- predict(popular, getData(e, "known"),
                    type="topNList")

rib2 <- predict(rib, getData(e, "known"),
                type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rib2<-as(rib2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rib2,2,f1),0.75)
vuelta<-t(rib2) ; dim(vuelta)
summary(colSums(rib2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rib2,na.rm=TRUE)>=quantile(apply(rib2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 42% de nuevo



# el user coverage no es significativo ya que todos recoiendan el 100%
# el item coverage tampoco es muy significativo ya que todas las distribucuones 
# tienen una gran freacencia en la izquierda, en valores bajos
# en el caso de novelity, es svd tiene muy buenos resultados ofreciendo un 74% 
# de productos novedosos mietras que el item based y el user based un 40-50 %


# RESULTADO ---------------------------------------------------------------

# EL MEJOR MODELO SERIA EL SVD K=6 POR SER UN 72% NOVEDOSO ADEMAS QUE 
# TARDA POCO EN EJECUTARSE Y NO OFRECE ERRORES MUY ALTO. LOS VALORES DE LOS RATES 
# SON LOS MAS ALTOS EN N BAJOS Y ES LO QUE NOS INTERESA

# POPULAR TAMBIEN SERIA UNA BUENA OPCION PERO OFRECE LOS MISMOS PRODUCTOS
# A LOS USUARIOS CON QUE NO ES NADA NOVEDOSO Y PERSONALIZADO

#EL USER BASED TIENE UNOS RATES Y ERRORES ALGO MAS ALTOS Y NOVEDITY MUCHO MAS 
# BAJO, ADEMAS TARDA MUCHO EN EJECUTARSE Y PUEDE SER UN PROBLEMA PARA EL CLIENTE

# EL ITEM BASED ES EL QUE PEORES RESULTADOS HA MOSTRADO CON ERRORES LLEGANDO AL 
# INFINITO Y PEORES RATES

#############################################################################
# COMPARACIO SUR Y NORTE --------------------------------------------------
#############################################################################


# NORTE ------------------------------------------------------------------



consultanorte<-dbGetQuery(conexionIKEA,"Select membership_id, product_category_3_name, sum(qty) from muebles where store like 'Norte' group by membership_id, product_category_3_name" )

summary(consultanorte$`sum(qty)`)

boxplot_norte<-plot_ly(consultanorte, y=~`sum(qty)`, type = "box")%>%layout(title="Boxplot cantidad norte")
saveWidget(boxplot_norte, "boxplot_norte.html", selfcontained=T)

quantile(consultanorte$`sum(qty)`, probs = 0.999)

consultanorte<-filter(consultanorte, `sum(qty)` <= quantile(consultanorte$`sum(qty)`, probs = 0.999))

consultanorte<-filter(consultanorte, `sum(qty)` >=1)

boxplot_norte_filtrado<-plot_ly(consultanorte, y=~`sum(qty)`, type = "box")%>%layout(title="Boxplot cantidad norte")
saveWidget(boxplot_norte_filtrado, "boxplot_norte_filtrado.html", selfcontained=T)

min(consultanorte$`sum(qty)`)

max(consultanorte$`sum(qty)`)

recomspreadnorte<-spread(consultanorte , product_category_3_name, `sum(qty)`)

# quitamos la instancia de los que no son socios

recomspreadnorte<-recomspreadnorte[-1,]

# quitamos la variable membershipid

recomspreadnorte<-recomspreadnorte[,-c(1)]

# corregir los titulos de las variables

names(recomspreadnorte)<-str_replace_all(names(recomspreadnorte), "�s", "s")

names(recomspreadnorte)<-str_replace_all(names(recomspreadnorte), "'s", "s")

names(recomspreadnorte)<-str_replace_all(names(recomspreadnorte), "&", "and")

names(recomspreadnorte)<-str_replace_all(names(recomspreadnorte), "\\.", ",")


names(recomspreadnorte)<-str_remove_all(names(recomspreadnorte),",$")

names(recomspreadnorte)<-str_remove_all(names(recomspreadnorte),",$")

names(recomspreadnorte)

#quitar filas con muchos NA y poca variedad de categoria

miss_case_summary(recomspreadnorte)

missnorte<-miss_case_summary(recomspreadnorte)

missnorte<-filter(missnorte, n_miss>200)

nrow(missnorte) # hay 18037 variables con mas de 7 missings

quitarnorte<-missnorte$case

recomspreadnorte<-recomspreadnorte[-quitarnorte,]

# convertir en matriz

recomspreadnorte<-as.matrix(recomspreadnorte)

recomspreadnorte<-as(recomspreadnorte, "realRatingMatrix")




# EVALUACION ----------------------------------------------------

set.seed(7)

min(rowCounts(recomspreadnorte))


eval_scheme <- evaluationScheme(recomspreadnorte, method = "split", train = 0.9, given = 7,
                                goodRating = 1)

# lista de algoritmos a testear
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "POPULAR" = list(name="POPULAR", param= NULL),
              "UBCF" = list(name = "UBCF", param = NULL),
              "IBCF" = list(name = "IBCF", param = NULL),
              "SVD" =list(name= "SVD", param=NULL))


# evaluacion de modelos y n

set.seed(7)
eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,2,3,4,5,6,7,8,9,10))

plot(eval, legend = "topleft")
title("Curva ROC")
png(filename = "modelos_norte_roc.png")
plot(eval, legend = "topleft")
title("Curva ROC")
dev.off()


plot(eval,"prec/rec")
title("Precision/Recall")
png(filename = "modelos_norte_precrec.png")
plot(eval,"prec/rec")
title("Precision/Recall")
dev.off()


getConfusionMatrix(eval[["POPULAR"]])
getConfusionMatrix(eval[["SVD"]])

#SVD 

##### SELECCIONAR EL K OPTIMO #####

# SVD


vector_k <- c(2,3,4,5,6,7)

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="SVD", param=list( k = k))
})

names(models_to_evaluate) <- paste0("SVD_k_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))


plot(list_results, annotate = 1, legend = "topleft") # el k optimo es 6-7
title("curva ROC")
png(filename = "svd_parametro_norte_roc.png")
plot(list_results, annotate = 1, legend = "topleft") 
title("curva ROC")
dev.off()



plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # el k optimo es 6-7
title("Precision/Recall")
png(filename = "svd_parametro_norte_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision/Recall")
dev.off()

# nos quedamos con el 6 porq es algo mejor que el 5 y hace el efecto bañera en el 
# grafico prec/rec. Tiene tambienen valores parecidos a 7 con que puede ser el intermedio
# se ejecuta rapido 
# k pequeños 


# UBCF

vector_k <- c(2,3,4,5,7,10,20,30,40,50)

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="UBCF", param=list( nn = k))
})

names(models_to_evaluate) <- paste0("UBCF_nn_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))


plot(list_results, annotate = 1, legend = "topleft") # el nn optimo es 2-5
title("curva ROC")
png(filename = "ubcf_parametro_norte_roc.png")
plot(list_results, annotate = 1, legend = "topleft") 
title("curva ROC")
dev.off()


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # el n optimo es 2-5
title("Precision/Recall")
png(filename = "ubcf_parametro_norte_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision/Recall")
dev.off()

#nn=3
# tarda en ejecutarse
# peor TPR y precision



# IBCF

vector_k <- c("euclidean", "canberra","pearson", "cosine")

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="IBCF", param=list( method = k))
})

names(models_to_evaluate) <- paste0("IBCF_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))

plot(list_results, annotate = 1, legend = "topleft")# la distancia optima es pearson-cosine
title("curva ROC")
png(filename = "ibcf_parametro_norte_roc.png")
plot(list_results, annotate = 1, legend = "topleft")
title("curva ROC")
dev.off()


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # la distancia optima es cosine
title("Precision/Recall")
png(filename = "ibcf_parametro_norte_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision/Recall")
dev.off()

# method cosine
# tarda en ejecutarse
# peor TPR y precision con n bajos


# COMPARAMOS EL POPULAR, SVD K=6 , UBCF NN=3 Y IBCF=COSINE


set.seed(7)

eval_scheme <- evaluationScheme(recomspreadnorte, method = "split", train = 0.9, given = 7,
                                goodRating = 1)

# lista de algoritmos a testear
algos <- list("POPULAR" = list(name="POPULAR", param= NULL),
              "UBCF_3" = list(name = "UBCF", param = list(nn=3)),
              "IBCF_C" = list(name = "IBCF", param = list(method="cosine")),
              "SVD_6" =list(name= "SVD", param=list(k=6)))


# evaluacion de modelos y n

set.seed(7)
eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,2,3,4,5,6,7,8,9,10))


plot(eval, legend = "topleft")
title("Curva ROC")
png(filename = "modelos_norte_idoneos_roc.png")
plot(eval, legend = "topleft")
title("Curva ROC")
dev.off()


plot(eval,"prec/rec")
title("Precision/Recall")
png(filename = "modelos_norte_idoneos_precrec.png")
plot(eval,"prec/rec")
title("Precision/Recall")
dev.off()

getConfusionMatrix(eval[["UBCF_3"]])
getConfusionMatrix(eval[["SVD_6"]])
getConfusionMatrix(eval[["POPULAR"]])
getConfusionMatrix(eval[["IBCF_C"]])

# con valores n bajos el svd tienen mejores rates y con altos el ubcf
# el ubcf tarda mucho mas 



# EVALUACION RATINGS ------------------------------------------------------


min(rowCounts(recomspreadnorte))

set.seed(7)

e <- evaluationScheme(recomspreadnorte,
                      method="split", train=0.9,
                      k=1, given=7)

train<-as(getData(e, "train"),"matrix")

known<-as(getData(e, "known"),"matrix")

unknown<-as(getData(e, "unknown"),"matrix")

#USER BASED

r <- Recommender(getData(e, "train"), "UBCF", param=list(nn=3))

p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
USERBASED<-calcPredictionAccuracy(p, getData(e, "unknown"))


#POPULAR

r <- Recommender(getData(e, "train"), "POPULAR")


p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
POPULAR<-calcPredictionAccuracy(p, getData(e, "unknown"))


#ITEM-BASED

r <- Recommender(getData(e, "train"), "IBCF", param=list(method="cosine"))


p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
ITEMBASED<-calcPredictionAccuracy(p, getData(e, "unknown"))


# SVD

r <- Recommender(getData(e, "train"), "SVD", param=list(k=6))


p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
SVD<-calcPredictionAccuracy(p, getData(e, "unknown"))



#RESULTADOS DE LOS 3 MODELOS
####################################################

USERBASED<-as.matrix(USERBASED)
USERBASED



POPULAR<-as.matrix(POPULAR)
POPULAR



ITEMBASED<-as.matrix(ITEMBASED)
ITEMBASED

SVD<-as.matrix(SVD)
SVD

#MATRIZ DE ACCURACY

colnames(USERBASED) <- "USERBASED"

colnames(POPULAR) <- "POPULAR"

colnames(ITEMBASED) <- "ITEMBASED"

colnames(SVD) <- "SVD"

MATRIZNORTE<-cbind(USERBASED,POPULAR,ITEMBASED, SVD)

MATRIZNORTE



# OTRAS EVALUACIONES ------------------------------------------------------

# user coverage - item coverage - novedity



################### SVD

#USER COVERAGE

#vamos a definir esta funcion
#para ver cuantos valores diferentes de NA hay en las predicciones
r <- Recommender(getData(e, "train"), "SVD", param=list(k=6))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%


# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor


########################### UBCF

#USER COVERAGE

r <- Recommender(getData(e, "train"), "UBCF", param=list(nn=3))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%

# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor

########################### POPULAR

#USER COVERAGE

r <- Recommender(getData(e, "train"), "POPULAR")
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%


## ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor

# todos dan el user coverage al 100% con que no es representativo
# el user based parece tener mejor item coverage aunque la distribucion 
# sigue estando a la izquierda



########################### IBCF

#USER COVERAGE

r <- Recommender(getData(e, "train"), "IBCF", param=list(method="cosine"))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%

# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor




################# NOVEDITY

# SVD

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                     method="POPULAR")

rsvd<-Recommender(getData(e, "train"),
                  method="SVD", param=list(k=6))

popular2 <- predict(popular, getData(e, "known"),
                    type="topNList")

rsvd2 <- predict(rsvd, getData(e, "known"),
                 type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rsvd2<-as(rsvd2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rsvd2,2,f1),0.75)
vuelta<-t(rsvd2) ; dim(vuelta)
summary(colSums(rsvd2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rsvd2,na.rm=TRUE)>=quantile(apply(rsvd2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50 

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 72% de nuevos



#IBCF

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                     method="POPULAR")

rib<-Recommender(getData(e, "train"),
                 method="IBCF", param=list(method="cosine"))

popular2 <- predict(popular, getData(e, "known"),
                    type="topNList")

rib2 <- predict(rib, getData(e, "known"),
                type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rib2<-as(rib2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rib2,2,f1),0.75)
vuelta<-t(rib2) ; dim(vuelta)
summary(colSums(rib2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rib2,na.rm=TRUE)>=quantile(apply(rib2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 42% de nuevo



#UBCF

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                     method="POPULAR")

rib<-Recommender(getData(e, "train"),
                 method="UBCF", param=list(nn=3))

popular2 <- predict(popular, getData(e, "known"),
                    type="topNList")

rib2 <- predict(rib, getData(e, "known"),
                type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rib2<-as(rib2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rib2,2,f1),0.75)
vuelta<-t(rib2) ; dim(vuelta)
summary(colSums(rib2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rib2,na.rm=TRUE)>=quantile(apply(rib2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 38% de nuevo



# svd ofrece categorias mas novedosas


# RESULTADO ---------------------------------------------------------------

# EL MEJOR MODELO SERIA EL SVD K=6 POR SER UN 72% NOVEDOOS Y EL POPULAR
# CON UNOS ERRORES DE 23,4,2  EL SVD 30,5 y 2
# EL USER BASED TIENE MEJOR ITEM COVERAGE POR LA DISTRIBUCION PERO ES MUY LENTO
# SIN EMBARGO LOS ERRORES DE USER BASED SON PARECIDOS AL SVD. SU PRECISION CON N ALTOS TAMBIEN ES MEJOR
# AL IGUAL QUE EL TPR
# ITEM BASED ES EL QUE PEORES RESULTADOS MUESTRA
# SVD ES MAS RAPIDO AL SER K BAJO. ESTO SE VE EN EL N, CUANTO MAS BAJO MEJOR Y EN UBC OTROS ALREVES
# PERO EN ESTE CASO NO NOS INTERESA RECOMENDAR MUCHO PRODUCTOS
# LO QUE HACE DE LAS MATRICES SVD
# EL VALOR DE N = 5 PORQ ES UN VALOR INTERMEDIO CON TPR/FPT Y PRECISION INTERMEDIOS
# N DEPENDERA DEL CLIENTE

# SERIA POPULAR Y SVD  DEPENDIENDO DEL TIPO DE RECOENDACION QUE QUIERA EL CLIENTE

# SVD PORQ TIENE MAS NOVELTY Y MAS RAPIDO Y MEJOR PREC N BAJOS
# POPULAR MENOS ERROR





##########################################################################
# SUR  --------------------------------------------------------------------
#############################################################################

consultasur<-dbGetQuery(conexionIKEA,"Select membership_id, product_category_3_name, sum(qty) from muebles where store like 'Sur' group by membership_id, product_category_3_name" )

summary(consultasur$`sum(qty)`)

boxplot_sur<-plot_ly(consultasur, y=~`sum(qty)`, type = "box")%>%layout(title="Boxplot cantidad sur")
saveWidget(boxplot_sur, "boxplot_sur.html", selfcontained=T)

quantile(consultasur$`sum(qty)`, probs = 0.999)

consultasur<-filter(consultasur, `sum(qty)` <= quantile(consultasur$`sum(qty)`, probs = 0.999))

consultasur<-filter(consultasur, `sum(qty)` >=1)

boxplot_sur_filtrado<-plot_ly(consultasur, y=~`sum(qty)`, type = "box")%>%layout(title="Boxplot cantidad sur")
saveWidget(boxplot_sur_filtrado, "boxplot_sur_filtrado.html", selfcontained=T)

min(consultasur$`sum(qty)`)

max(consultasur$`sum(qty)`)

recomspreadsur<-spread(consultasur , product_category_3_name, `sum(qty)`)

# quitamos la instancia de los que no son socios

recomspreadsur<-recomspreadsur[-1,]

# quitamos la variable unknown y membershipid

recomspreadsur<-recomspreadsur[,-c(1,202)]

# corregir los titulos de las variables

names(recomspreadsur)<-str_replace_all(names(recomspreadsur), "�s", "s")

names(recomspreadsur)<-str_replace_all(names(recomspreadsur), "'s", "s")

names(recomspreadsur)<-str_replace_all(names(recomspreadsur), "&", "and")

names(recomspreadsur)<-str_replace_all(names(recomspreadsur), "\\.", ",")

names(recomspreadsur)<-str_remove_all(names(recomspreadsur),",$")

names(recomspreadsur)<-str_remove_all(names(recomspreadsur),",$")

names(recomspreadsur)

#quitar filas con muchos NA y poca variedad de categoria

miss_case_summary(recomspreadsur)

misssur<-miss_case_summary(recomspreadsur)

misssur<-filter(misssur, n_miss>198)

nrow(misssur) # hay 18572 variables con mas de 7 missings

quitarsur<-misssur$case

recomspreadsur<-recomspreadsur[-quitarsur,]

# convertir en matriz

recomspreadsur<-as.matrix(recomspreadsur)

recomspreadsur<-as(recomspreadsur, "realRatingMatrix")







# EVALUACION ----------------------------------------------------


min(rowCounts(recomspreadsur))

set.seed(7)

eval_scheme <- evaluationScheme(recomspreadsur, method = "split", train = 0.9, given = 7,
                                goodRating = 1)

# lista de algoritmos a testear
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "POPULAR" = list(name="POPULAR", param= NULL),
              "UBCF" = list(name = "UBCF", param = NULL),
              "IBCF" = list(name = "IBCF", param = NULL),
              "SVD" =list(name= "SVD", param=NULL))


# evaluacion de modelos y n

set.seed(7)
eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,2,3,4,5,6,7,8,9,10))


plot(eval, legend = "topleft")
title("Curva ROC")
png(filename = "modelos_sur_roc.png")
plot(eval, legend = "topleft")
title("Curva ROC")
dev.off()


plot(eval,"prec/rec")
title("Precision/Recall")
png(filename = "modelos_sur_precroc.png")
plot(eval,"prec/rec")
title("Precision/Recall")
dev.off()

getConfusionMatrix(eval[["POPULAR"]])
getConfusionMatrix(eval[["SVD"]])

#SVD / POPULAR 

##### SELECCIONAR EL K OPTIMO #####

# SVD


vector_k <- c(2,3,4,5,6,7)

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="SVD", param=list( k = k))
})

names(models_to_evaluate) <- paste0("SVD_k_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))


plot(list_results, annotate = 1, legend = "topleft") # el k optimo es 5-6-7
title("curva ROC")
png(filename = "svd_parametro_sur_roc.png")
plot(list_results, annotate = 1, legend = "topleft") 
title("curva ROC")
dev.off()


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # el k optimo es 6
title("Precision/Recall")
png(filename = "svd_parametro_sur_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision/Recall")
dev.off()

# nos quedamos con el 5 porq es algo mejor que el 6 en valores bajos y parecido en valores altos
# y hace el efecto bañera
# grafico prec/rec. Tiene tambien en valores parecidos a 7 con que puede ser el intermedio
# se ejecuta rapido 
# k pequeños (lo de los matrices)


# UBCF

vector_k <- c(2,3,4,5,7,10,20,30,40,50)

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="UBCF", param=list( nn = k))
})

names(models_to_evaluate) <- paste0("UBCF_nn_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))



plot(list_results, annotate = 1, legend = "topleft")# el nn optimo es 2-3
title("curva ROC")
png(filename = "ubcf_parametro_sur_roc.png")
plot(list_results, annotate = 1, legend = "topleft")
title("curva ROC")
dev.off()


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # el nn optimo es 3
title("Precision/Recall")
png(filename = "ubcf_parametro_sur_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision/Recall")
dev.off()

#nn=3

# tarda en ejecutarse
# peor TPR y precision


# IBCF

vector_k <- c("euclidean", "canberra","pearson", "cosine")

models_to_evaluate <- lapply(vector_k, function(k){
  list(name="IBCF", param=list( method = k))
})

names(models_to_evaluate) <- paste0("IBCF_", vector_k)

set.seed(7)

list_results <- evaluate(eval_scheme,
                         models_to_evaluate,
                         n=c(1,2,3,4,5,6,7,8,9,10))


plot(list_results, annotate = 1, legend = "topleft") # la distancia optima es pearson-cosine
title("curva ROC")
png(filename = "ibcf_parametro_sur_roc.png")
plot(list_results, annotate = 1, legend = "topleft")
title("curva ROC")
dev.off()


plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") # la distancia optima es cosine
title("Precision/Recall")
png(filename = "ibcf_parametro_sur_precrec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright") 
title("Precision/Recall")
dev.off()

# method cosine
# en n bajos mejor y en altos tambien


# COMPARAMOS EL POPULAR, SVD K=5 , UBCF NN=3 Y IBCF=COSINE


set.seed(7)

eval_scheme <- evaluationScheme(recomspreadnorte, method = "split", train = 0.9, given = 7,
                                goodRating = 1)

# lista de algoritmos a testear
algos <- list("POPULAR" = list(name="POPULAR", param= NULL),
              "UBCF_3" = list(name = "UBCF", param = list(nn=3)),
              "IBCF_C" = list(name = "IBCF", param = list(method="cosine")),
              "SVD_5" =list(name= "SVD", param=list(k=5)))


# evaluacion de modelos y n

set.seed(7)
eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,2,3,4,5,6,7,8,9,10))



plot(eval, legend = "topleft")
title("Curva ROC")
png(filename = "modelos_idoneos_sur_roc.png")
plot(eval, legend = "topleft")
title("Curva ROC")
dev.off()


plot(eval,"prec/rec")
title("Precision/Recall")
png(filename = "modelos_idoneos_sur_precrec.png")
plot(eval,"prec/rec")
title("Precision/Recall")
dev.off()



getConfusionMatrix(eval[["UBCF_3"]])
getConfusionMatrix(eval[["SVD_5"]])
getConfusionMatrix(eval[["POPULAR"]])
getConfusionMatrix(eval[["IBCF_C"]])

# con valores n bajos el ubcf y svd tienen mejores rates y con altos el ubcf
# el ubcf tarda mucho mas 


# EVALUACION RATINGS ------------------------------------------------------

min(rowCounts(recomspreadsur))

set.seed(7)

e <- evaluationScheme(recomspreadsur,
                      method="split", train=0.9,
                      k=1, given=7)

train<-as(getData(e, "train"),"matrix")

known<-as(getData(e, "known"),"matrix")

unknown<-as(getData(e, "unknown"),"matrix")

#USER BASED

r <- Recommender(getData(e, "train"), "UBCF", param=list(nn=3))

p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
USERBASED<-calcPredictionAccuracy(p, getData(e, "unknown"))


#POPULAR

r <- Recommender(getData(e, "train"), "POPULAR")


p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
POPULAR<-calcPredictionAccuracy(p, getData(e, "unknown"))


#ITEM-BASED

r <- Recommender(getData(e, "train"), "IBCF", param=list(method="cosine"))


p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
ITEMBASED<-calcPredictionAccuracy(p, getData(e, "unknown"))

# SVD

r <- Recommender(getData(e, "train"), "SVD", param=list(k=5))


p <- predict(r, getData(e, "known"), type="ratings") 
predicciones<-as(p,"matrix")

#calculando Accuracy
SVD<-calcPredictionAccuracy(p, getData(e, "unknown"))



#RESULTADOS DE LOS 3 MODELOS
####################################################

USERBASED<-as.matrix(USERBASED)
USERBASED



POPULAR<-as.matrix(POPULAR)
POPULAR



ITEMBASED<-as.matrix(ITEMBASED)
ITEMBASED

SVD<-as.matrix(SVD)
SVD

#MATRIZ DE ACCURACY

colnames(USERBASED) <- "USERBASED"

colnames(POPULAR) <- "POPULAR"

colnames(ITEMBASED) <- "ITEMBASED"

colnames(SVD) <- "SVD"

MATRIZSUR<-cbind(USERBASED,POPULAR,ITEMBASED, SVD)

MATRIZSUR

# el mejor user based, luego popular y luego svd




# OTRAS EVALUACIONES ------------------------------------------------------

# user coverage - item coverage - novedity



################### SVD

#USER COVERAGE

#vamos a definir esta funcion
#para ver cuantos valores diferentes de NA hay en las predicciones
r <- Recommender(getData(e, "train"), "SVD", param=list(k=5))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%


# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor


########################### UBCF

#USER COVERAGE

r <- Recommender(getData(e, "train"), "UBCF", param=list(nn=3))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%

# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor

########################### POPULAR

#USER COVERAGE

r <- Recommender(getData(e, "train"), "POPULAR")
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%


## ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor

# todos dan el user coverage al 100% con que no es representativo
# el user based parece tener mejor item coverage aunque la distribucion 
# sigue estando a la izquierda



########################### IBCF

#USER COVERAGE

r <- Recommender(getData(e, "train"), "IBCF", param=list(method="cosine"))
p <- predict(r, getData(e, "known"), type="topNList", n=5) # cerramos a 5 recomendaciones
predicciones<-as(p,"matrix")

f1<-function(x){
  return(sum(!is.na(x)))
}

cinco<-apply(predicciones,1,f1) # puede recomendar de todos

cinco # recomienda el 100%

# ITEM COVERAGE
hist(apply(predicciones,2,f1)) # cuanto mas a la derecha mejor




################# NOVEDITY

# SVD

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                     method="POPULAR")

rsvd<-Recommender(getData(e, "train"),
                  method="SVD", param=list(k=5))

popular2 <- predict(popular, getData(e, "known"),
                    type="topNList")

rsvd2 <- predict(rsvd, getData(e, "known"),
                 type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rsvd2<-as(rsvd2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rsvd2,2,f1),0.75)
vuelta<-t(rsvd2) ; dim(vuelta)
summary(colSums(rsvd2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rsvd2,na.rm=TRUE)>=quantile(apply(rsvd2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50 

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 74% de nuevos



#IBCF

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                     method="POPULAR")

rib<-Recommender(getData(e, "train"),
                 method="IBCF", param=list(method="cosine"))

popular2 <- predict(popular, getData(e, "known"),
                    type="topNList")

rib2 <- predict(rib, getData(e, "known"),
                type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rib2<-as(rib2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rib2,2,f1),0.75)
vuelta<-t(rib2) ; dim(vuelta)
summary(colSums(rib2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rib2,na.rm=TRUE)>=quantile(apply(rib2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 40% de nuevo



#UBCF

# comparando resultados con el popular 
popular<-Recommender(getData(e, "train"),
                     method="POPULAR")

rib<-Recommender(getData(e, "train"),
                 method="UBCF", param=list(nn=3))

popular2 <- predict(popular, getData(e, "known"),
                    type="topNList")

rib2 <- predict(rib, getData(e, "known"),
                type="topNList", )



# lista de categorias popular
popular@model[["topN"]]@itemLabels
#Ahora tengo que mirar cuanto de novedosas #son estas predicciones en
#relacion a las que tengo en mi recomendador
rib2<-as(rib2,"matrix")
#vamos a mirar cuales los productos mas frecuentemente sugeridos
quantile(apply(rib2,2,f1),0.75)
vuelta<-t(rib2) ; dim(vuelta)
summary(colSums(rib2,na.rm=TRUE))
itemsSeleccionados<-vuelta[colSums(rib2,na.rm=TRUE)>=quantile(apply(rib2,2,f1),0.75) ,]
dim(itemsSeleccionados) ; rownames(itemsSeleccionados)
#estos son los nombres de las categorias de productos que mas
#estimaciones de suma de ratings acumulan(frecuentemente altamente
#valorados)
intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50])

length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50

(1-length(intersect(rownames(itemsSeleccionados),popular@model[["topN"]]@itemLabels[1:50]))/50)*100
# 44% de nuevo


# RESULTADO ---------------------------------------------------------------

# EL MEJOR MODELO SERIA EL SVD K=5 o UBCF NN=3 
# SVDPOR SER UN 74% NOVEDOSO
# PERO TIENE LOS ERROES ALTOS
# EL USER BASED TIENE MEJOR ITEM COVERAGE POR LA DISTRIBUCION PERO ES MUY LENTO
# SIN EMBARGO LOS ERRORES DE USER BASED SON LOS MEJORES
# ITEM BASED ES EL QUE PEORES RESULTADOS MUESTRA

# USER BASED OFRECE MEJOR PRECISION Y TPR Y FPR
# EL VALOR DE N = 5 PORQ ES UN VALOR INTERMEDIO CON TPR/FPT Y PRECISION INTERMEDIOS
# N DEPENDERA DEL CLIENTE

# SERIA USER BASED Y SVD  
# CON N BAJO SE PODRIA PLANTEAR SVD PERO CON N MAS ELEVADO USER BASED

# SVD PORQ TIENE MAS NOVELTY Y MEJOR CON N BAJO
# USER BASED MENOR ERROR, Y TIENE MEJOR PRECISION, TPR Y FPR








