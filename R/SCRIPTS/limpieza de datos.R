conexionIKEA<-dbConnect(odbc(),"RETO 4")
ikea<-dbReadTable(conexionIKEA, "muebles")

# DATOS 1 

str(ikea)

#store
table(ikea$store)

#categria 1
table(ikea$product_category_1)

# categoria 1 nombre
table(ikea$product_category_1_name)
ikea$product_category_1_name<-str_replace_all(ikea$product_category_1_name, "�s", "s")
ikea$product_category_1_name<-str_replace_all(ikea$product_category_1_name, "&", "and")

# categoria 2
table(ikea$product_category_2)

# categoria 2 nombre
table(ikea$product_category_2_name)
ikea$product_category_2_name<-str_replace_all(ikea$product_category_2_name, "�s", "s")
ikea$product_category_2_name<-str_replace_all(ikea$product_category_2_name, "'s", "s")
ikea$product_category_2_name<-str_replace_all(ikea$product_category_2_name, "&", "and")
ikea$product_category_2_name<-str_replace_all(ikea$product_category_2_name, "\\.", ",")
ikea$product_category_2_name<-str_remove_all(ikea$product_category_2_name,",$")


# categoria 3
table(ikea$product_category_3)

# categoria 3 nombre
table(ikea$product_category_3_name)
ikea$product_category_3_name<-str_replace_all(ikea$product_category_3_name, "�s", "s")
ikea$product_category_3_name<-str_replace_all(ikea$product_category_3_name, "&", "and")
ikea$product_category_3_name<-str_replace_all(ikea$product_category_3_name, "'s", "s")
ikea$product_category_3_name<-str_replace_all(ikea$product_category_3_name, "\\.", ",")
ikea$product_category_3_name<-str_remove_all(ikea$product_category_3_name,",$" )
ikea$product_category_3_name<-str_remove_all(ikea$product_category_3_name,",$" )





# missings

ikea$membership_id[ikea$membership_id==""]<-NA
miss_var_summary(ikea)
anyDuplicated(ikea)
ikea<-distinct(ikea)
# socios

socios<-na.omit(ikea)
anyDuplicated(socios)
socios<-distinct(socios)

# no socios

no_socios<-filter(ikea, is.na(membership_id))
anyDuplicated(no_socios)
no_socios<-distinct(no_socios)

# Eliminamos categorias desconocidas --------------------------------------
ikea<-ikea%>%
  filter(!product_category_1_name=="UNKNOWN")

write.csv(ikea, "./DATOS TRANSFORMADOS/ikea.csv", row.names = F, dec = ".", sep = ";")
write.csv(socios, "./DATOS TRANSFORMADOS/socios.csv", row.names = F, dec = ".", sep = ";")
write.csv(no_socios, "./DATOS TRANSFORMADOS/no_socios.csv", row.names = F, dec = ".", sep = ";")



