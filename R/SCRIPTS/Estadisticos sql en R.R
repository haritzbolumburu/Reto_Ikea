
conexionIKEA<-dbConnect(odbc(),"RETO 4")

#consultas

#Consulta general
consulta<-dbGetQuery(conexionIKEA,"Select count(*) from muebles" )

consulta
class(consulta)


#Consulta 1 -- Cuantos membership tenemos en furniture?
consulta1<-dbGetQuery(conexionIKEA,"select count(membership_id) from muebles
where membership_id not like '';")
consulta1


#Consulta 2 -- Cuantos c?digos furniture tenemos <- no tenemos, solo categorias

consulta2.1 <- dbGetQuery(conexionIKEA,"select count(distinct product_category_1) from muebles;")
consulta2.1
consulta2.2 <- dbGetQuery(conexionIKEA,"select count(distinct product_category_2) from muebles;")
consulta2.2
consulta2.3 <- dbGetQuery(conexionIKEA,"select count(distinct product_category_3) from muebles;")
consulta2.3


#Consulta 3 -- Media de las ventas por Norte/Sur

consulta3 <- dbGetQuery(conexionIKEA,"select avg(sales), store from muebles
group by store;")
consulta3


#Consulta 4 -- Cuantas categorias de producto hay en categoria1,2 y3.

consulta4.1 <- dbGetQuery(conexionIKEA,"select count(distinct product_category_1) from muebles;")
consulta4.1
consulta4.2 <- dbGetQuery(conexionIKEA,"select count(distinct product_category_2) from muebles;")
consulta4.2
consulta4.3 <- dbGetQuery(conexionIKEA,"select count(distinct product_category_3) from muebles;")
consulta4.3


#Consulta 5 -- Media de las ventas por categor?a producto

consulta5.1 <- dbGetQuery(conexionIKEA,"select avg(sales), product_category_1_name from muebles
group by product_category_1_name
order by avg(sales) desc;")
consulta5.1
consulta5.2 <- dbGetQuery(conexionIKEA,"select avg(sales), product_category_2_name from muebles
group by product_category_2_name
order by avg(sales) desc;")
consulta5.2
consulta5.3 <- dbGetQuery(conexionIKEA,"select avg(sales), product_category_3_name from muebles
group by product_category_3_name
order by avg(sales) desc;")
consulta5.3


#Consulta 6 -- ?cuantas transacciones como media al dia en furniture?

consulta6.1 <- dbGetQuery(conexionIKEA,"select count(transaction_id), day(transaction_timestamp) from muebles
group by day(transaction_timestamp)
order by(count(transaction_id)) desc;")
consulta6.1

consulta6.2 <- dbGetQuery(conexionIKEA,"select count(transaction_id), hour(transaction_timestamp) from muebles
group by hour(transaction_timestamp)
order by(count(transaction_id)) desc;")
consulta6.2


#Consulta 7 -- ?cuantas transacciones como media a la semana en furniture?

consulta7 <- dbGetQuery(conexionIKEA,"select count(transaction_id), week(transaction_timestamp) from muebles
group by week(transaction_timestamp)
order by(count(transaction_id)) desc;")
consulta7


#Consulta 8 -- Numero de productos medios por transacci?n

consulta8 <- dbGetQuery(conexionIKEA,"select avg(qty), transaction_id from muebles
group by transaction_id;")
consulta8


#Consulta 9 -- Periodo de datos , fecha m?nima, fecha maxima

consulta9.1 <- dbGetQuery(conexionIKEA,"  select transaction_timestamp from muebles
order by transaction_timestamp;") #Fecha minima
consulta9.1

consulta9.2 <- dbGetQuery(conexionIKEA,"select transaction_timestamp from muebles
order by transaction_timestamp desc;")#Fecha maxima
consulta9.2


#Consulta 10 -- ?En cuantas semanas diferentes de ese mes ha comprado un membership como media en furniture?

consulta10 <- dbGetQuery(conexionIKEA,"select  distinct membership_id ,count(distinct week(transaction_timestamp))from muebles
group by membership_id 
order by count(distinct week(transaction_timestamp)) desc;")
consulta10


