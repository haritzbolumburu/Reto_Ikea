USE ikea;

select count(*) from ikea.muebles; #532277

#1. Cuantos membership tenemos, en food , en furniture,y de los que compran de ambos?
#solo se cuentan los membership de muebles.
select * from muebles;
select count(membership_id) from muebles
where membership_id not like ""; #314369 memerships en muebles

#2. Cuantos códigos furniture tenemos <- no tenemos, solo categorias
select count(distinct product_category_1) from muebles; #22
select count(distinct product_category_2_name) from muebles; #72
select count(distinct product_category_3_name) from muebles; #212

#3. Media de las ventas por Norte/Sur
select avg(sales), store from muebles
group by store;
#Media ventas en el norte 32.10
#Media ventas en el sur 28.49

#4. Cuantas categorias de producto hay en categoria1,2 y3.
select * from muebles;
select count(distinct product_category_1_name) from muebles; #22
select count(distinct product_category_2_name) from muebles; #72
select count(distinct product_category_3_name) from muebles; #212

#5. Media de las ventas por categoría producto
select avg(sales), product_category_1_name from muebles
group by product_category_1_name
order by avg(sales) desc;

select avg(sales), product_category_2_name from muebles
group by product_category_2_name
order by avg(sales) desc;

select avg(sales), product_category_3_name from muebles
group by product_category_3_name
order by avg(sales) desc;

#8. ¿cuantas transacciones como media al dia en furniture?
select * from muebles;

select count(transaction_id), day(transaction_timestamp) from muebles
group by day(transaction_timestamp)
order by(count(transaction_id)) desc;

select count(transaction_id), hour(transaction_timestamp) from muebles
group by hour(transaction_timestamp)
order by(count(transaction_id)) desc;

#9. ¿cuantas transacciones como media a la semana en furniture?
select count(transaction_id), week(transaction_timestamp) from muebles
group by week(transaction_timestamp)
order by(count(transaction_id)) desc;

#10. Numero de productos medios por transacción
select avg(qty), transaction_id from muebles
group by transaction_id;

#11. Periodo de datos , fecha mínima, fecha maxima
select transaction_timestamp from muebles
order by transaction_timestamp; #Fecha minima

select transaction_timestamp from muebles
order by transaction_timestamp desc; #Fecha maxima

#12. ¿En cuantas semanas diferentes de ese mes ha comprado un membership como media en furniture?
select  distinct membership_id ,count(distinct week(transaction_timestamp))from muebles
group by membership_id 
order by count(distinct week(transaction_timestamp)) desc;








#Extra de conexion
select membership_id, product_category_1_name, (sales)
from muebles
group by membership_id, product_category_1_name;

