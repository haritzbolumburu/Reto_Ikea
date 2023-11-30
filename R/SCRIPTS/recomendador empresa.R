

#######################
# RECOMENDADOR EMPRESA
#######################

# conexionIKEA SQL-R ----------------------------------------------------------

conexionIKEA<-dbConnect(odbc(),"RETO 4")


# CATEGORIA 3 -------------------------------------------------------------


consulta3<-dbGetQuery(conexionIKEA,"Select membership_id, product_category_3_name, sum(qty) from muebles group by membership_id, product_category_3_name" )


# Separar en  3 secciones -------------------------------------------------

dfempresa<-select(consulta3, c("membership_id", "sum(qty)"))

dfempresa<-dfempresa%>%group_by(membership_id)%>%summarise_all(sum)

dfempresa<-dfempresa[-1,]

names(dfempresa)<-c("membership_id", "cantidad")

summary(dfempresa$cantidad)

plot_ly(dfempresa, y=~cantidad, type = "box")

quantile(dfempresa$cantidad, probs = 0.999)

dfempresa<-filter(dfempresa, cantidad>= 1 & cantidad<= quantile(dfempresa$cantidad, probs = 0.999))

min(dfempresa$cantidad)

max(dfempresa$cantidad)


ggplot(dfempresa, aes(x=cantidad))+geom_histogram()+theme_minimal()+
  scale_x_continuous(limits = c(0,150))

ggplot(dfempresa, aes(x=cantidad))+geom_histogram()+theme_minimal()+
  scale_x_continuous(limits = c(0,50))

ggplot(dfempresa, aes(x=cantidad))+geom_histogram()+theme_minimal()+
  scale_x_continuous(limits = c(50,200))


dfempresa$segmento<-ifelse(dfempresa$cantidad<=2, "Puntuales",
                           ifelse(dfempresa$cantidad<=10, "Familias", 
                                  ifelse(dfempresa$cantidad<=50, "mudanza", "empresas")))

table(dfempresa$segmento)

mean(dfempresa$cantidad)

puntuales<-filter(dfempresa, segmento=="Puntuales")

mean(puntuales$cantidad)

plot_ly(puntuales, y=~cantidad, type = "box")%>%layout(title="PUNTUALES")



familias<-filter(dfempresa, segmento=="Familias")

mean(familias$cantidad)

plot_ly(familias, y=~cantidad, type = "box")%>%layout(title="FAMILIAS")




mudanza<-filter(dfempresa, segmento=="mudanza")

mean(mudanza$cantidad)

plot_ly(mudanza, y=~cantidad, type = "box")%>%layout(title="MUDANZAS")




empresa<-filter(dfempresa, segmento=="empresas")

mean(empresa$cantidad)

plot_ly(empresa, y=~cantidad, type = "box")%>%layout(title="EMPRESAS")



# GERENAR MATRIZ DE RECOMENDACION -------------------------------------------------

# PUNTUALES

puntuales_rec<-full_join(consulta3, puntuales, by="membership_id")

puntuales_rec<-filter(puntuales_rec, segmento=="Puntuales")

puntuales_rec<-puntuales_rec[,-c(4,5)]

puntuales_rec<-spread(puntuales_rec , product_category_3_name, `sum(qty)`)

# quitamos la variable unknown y membershipid

colnames(puntuales_rec)

puntuales_rec<-puntuales_rec[,-c(1,192)]

# corregir los titulos de las variables

names(puntuales_rec)<-str_replace_all(names(puntuales_rec), "�s", "s")

names(puntuales_rec)<-str_replace_all(names(puntuales_rec), "'s", "s")

names(puntuales_rec)<-str_replace_all(names(puntuales_rec), "&", "and")

names(puntuales_rec)<-str_replace_all(names(puntuales_rec), "\\.", ",")

names(puntuales_rec)<-str_remove_all(names(puntuales_rec),",$")

names(puntuales_rec)<-str_remove_all(names(puntuales_rec),",$")

names(puntuales_rec)

#quitar filas con muchos NA y poca variedad de categoria

miss_case_summary(puntuales_rec)

miss3<-miss_case_summary(puntuales_rec)

miss3<-filter(miss3, n_miss==ncol(puntuales_rec))

nrow(miss3) # hay 18 variables con mas del 100% de missings

quitar3<-miss3$case

puntuales_rec<-puntuales_rec[-quitar3,]

# convertir en matriz

puntuales_rec<-as.matrix(puntuales_rec)

puntuales_rec<-as(puntuales_rec, "realRatingMatrix")





# MUDANZAS

mudanzas_rec<-full_join(consulta3, mudanza, by="membership_id")

mudanzas_rec<-filter(mudanzas_rec, segmento=="mudanza")

mudanzas_rec<-mudanzas_rec[,-c(4,5)]

mudanzas_rec<-spread(mudanzas_rec , product_category_3_name, `sum(qty)`)

# quitamos la variable unknown y membershipid

colnames(mudanzas_rec)

mudanzas_rec<-mudanzas_rec[,-c(1,205)]

# corregir los titulos de las variables

names(mudanzas_rec)<-str_replace_all(names(mudanzas_rec), "�s", "s")

names(mudanzas_rec)<-str_replace_all(names(mudanzas_rec), "'s", "s")

names(mudanzas_rec)<-str_replace_all(names(mudanzas_rec), "&", "and")

names(mudanzas_rec)<-str_replace_all(names(mudanzas_rec), "\\.", ",")

names(mudanzas_rec)<-str_remove_all(names(mudanzas_rec),",$")

names(mudanzas_rec)<-str_remove_all(names(mudanzas_rec),",$")

names(mudanzas_rec)

#quitar filas con muchos NA y poca variedad de categoria

miss_case_summary(mudanzas_rec)

miss3<-miss_case_summary(mudanzas_rec)

#miss3<-filter(miss3, n_miss==ncol(mudanzas_rec))

#nrow(miss3) # hay 15 variables con mas del 100% de missings

#quitar3<-miss3$case

#mudanzas_rec<-mudanzas_rec[-quitar3,]

# convertir en matriz

mudanzas_rec<-as.matrix(mudanzas_rec)

mudanzas_rec<-as(mudanzas_rec, "realRatingMatrix")





#FAMILIAS

familias_rec<-full_join(consulta3, familias, by="membership_id")

familias_rec<-filter(familias_rec, segmento=="Familias")

familias_rec<-familias_rec[,-c(4,5)]

familias_rec<-spread(familias_rec , product_category_3_name, `sum(qty)`)

# quitamos la variable unknown y membershipid

colnames(familias_rec)

familias_rec<-familias_rec[,-c(1,196)]

# corregir los titulos de las variables

names(familias_rec)<-str_replace_all(names(familias_rec), "�s", "s")

names(familias_rec)<-str_replace_all(names(familias_rec), "'s", "s")

names(familias_rec)<-str_replace_all(names(familias_rec), "&", "and")

names(familias_rec)<-str_replace_all(names(familias_rec), "\\.", ",")

names(familias_rec)<-str_remove_all(names(familias_rec),",$")

names(familias_rec)<-str_remove_all(names(familias_rec),",$")

names(familias_rec)

#quitar filas con muchos NA y poca variedad de categoria

miss_case_summary(familias_rec)

miss3<-miss_case_summary(familias_rec)

miss3<-filter(miss3, n_miss==ncol(familias_rec))

nrow(miss3) # hay 3 variables con mas del 100% de missings

quitar3<-miss3$case

familias_rec<-familias_rec[-quitar3,]

# convertir en matriz

familias_rec<-as.matrix(familias_rec)

familias_rec<-as(familias_rec, "realRatingMatrix")





#EMPRESAS

empresa_rec<-full_join(consulta3, empresa, by="membership_id")

empresa_rec<-filter(empresa_rec, segmento=="empresas")

empresa_rec<-empresa_rec[,-c(4,5)]

empresa_rec<-spread(empresa_rec , product_category_3_name, `sum(qty)`)

# quitamos la variable unknown y membershipid

colnames(empresa_rec)

empresa_rec<-empresa_rec[,-c(1,201)]

# corregir los titulos de las variables

names(empresa_rec)<-str_replace_all(names(empresa_rec), "�s", "s")

names(empresa_rec)<-str_replace_all(names(empresa_rec), "'s", "s")

names(empresa_rec)<-str_replace_all(names(empresa_rec), "&", "and")

names(empresa_rec)<-str_replace_all(names(empresa_rec), "\\.", ",")

names(empresa_rec)<-str_remove_all(names(empresa_rec),",$")

names(empresa_rec)<-str_remove_all(names(empresa_rec),",$")

names(empresa_rec)

#quitar filas con muchos NA y poca variedad de categoria

miss_case_summary(empresa_rec)

miss3<-miss_case_summary(empresa_rec)

#miss3<-filter(miss3, n_miss==ncol(empresa_rec))

#nrow(miss3) # hay 6 variables con mas del 100% de missings

#quitar3<-miss3$case

#empresa_rec<-empresa_rec[-quitar3,]

# convertir en matriz

empresa_rec<-as.matrix(empresa_rec)

empresa_rec<-as(empresa_rec, "realRatingMatrix")





# RECOMENDADORES ----------------------------------------------------------


# PUNTUALES

modelo <- Recommender(puntuales_rec,
                      "POPULAR")

nuevo<-puntuales_rec

p <- predict(modelo, nuevo, type="topNList", n=1) 

predicciones<-as(p,"matrix")

predicciones<-as(p,"list")

predicciones

predicciones<-unlist(predicciones)

table(predicciones)

which.max(table(predicciones)) #TEXTILE ACCESSORIES




modelo <- Recommender(puntuales_rec,
                      "SVD",
                      param=list( k=2))

nuevo<-puntuales_rec

p <- predict(modelo, nuevo, type="topNList", n=2) 

predicciones<-as(p,"matrix")

predicciones<-as(p,"list")

predicciones

predicciones<-unlist(predicciones)

table(predicciones)

which.max(table(predicciones)) #Accessories for children



# MUDANZAS

modelo <- Recommender(mudanzas_rec,
                      "POPULAR")

nuevo<-mudanzas_rec[5000:8000]

p <- predict(modelo, nuevo, type="topNList", n=3) 

predicciones<-as(p,"matrix")

predicciones<-as(p,"list")

predicciones

predicciones<-unlist(predicciones)

table(predicciones)

which.max(table(predicciones)) # interior organizers




modelo <- Recommender(mudanzas_rec,
                      "SVD",
                      param=list( k=6))

nuevo<-mudanzas_rec

p <- predict(modelo, nuevo, type="topNList", n=3) 

predicciones<-as(p,"matrix")

predicciones<-as(p,"list")

predicciones

predicciones<-unlist(predicciones)

table(predicciones)

which.max(table(predicciones)) #  boards





# FAMILIA

modelo <- Recommender(familias_rec,
                      "POPULAR")

nuevo<-familias_rec[14000:17000,]

p <- predict(modelo, nuevo, type="topNList", n=3) 

predicciones<-as(p,"matrix")

predicciones<-as(p,"list")

predicciones

predicciones<-unlist(predicciones)

table(predicciones)

which.max(table(predicciones)) # storage boxes





modelo <- Recommender(familias_rec,
                      "SVD",
                      param=list( k=6))

nuevo<-familias_rec

p <- predict(modelo, nuevo, type="topNList", n=3) 

predicciones<-as(p,"matrix")

predicciones<-as(p,"list")

predicciones

predicciones<-unlist(predicciones)

table(predicciones)

which.max(table(predicciones)) #  boards





# EMPRESAS

modelo <- Recommender(empresa_rec,
                      "POPULAR")

nuevo<-empresa_rec

p <- predict(modelo, nuevo, type="topNList", n=3) 

predicciones<-as(p,"matrix")

predicciones<-as(p,"list")

predicciones

predicciones<-unlist(predicciones)

table(predicciones)

which.max(table(predicciones)) # System cabinets





modelo <- Recommender(empresa_rec,
                      "SVD",
                      param=list( k=6))

nuevo<-empresa_rec

p <- predict(modelo, nuevo, type="topNList", n=3) 

predicciones<-as(p,"matrix")

predicciones<-as(p,"list")

predicciones

predicciones<-unlist(predicciones)

table(predicciones)

which.max(table(predicciones)) # Skirting boards and profiles
