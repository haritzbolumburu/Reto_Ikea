SET lc_time_names = 'en_US';
LOAD DATA  INFILE "C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/furniture - 1de2.csv" INTO TABLE muebles
FIELDS TERMINATED BY ';'
LINES TERMINATED BY '\n'
IGNORE 1 LINES
(store, transaction_id, @transaction_timestamp, membership_id, product_category_1, @product_category_1_name, product_category_2, @product_category_2_name, product_category_3, @product_category_3_name, qty, sales)
set transaction_timestamp = DATE_FORMAT(STR_TO_DATE(@transaction_timestamp,'%Y-%m-%dT%H:%i:%s.000Z'),'%Y-%m-%d %H:%i:%s'),
 product_category_2_name=substring(@product_category_2_name,1,100),
 product_category_1_name=substring(@product_category_1_name,1,100),
 product_category_3_name=substring(@product_category_3_name,1,100);
  
LOAD DATA  INFILE "C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/furniture - 2de2.csv" INTO TABLE muebles
FIELDS TERMINATED BY ';'
LINES TERMINATED BY '\n'
IGNORE 1 LINES
(store, transaction_id, @transaction_timestamp, membership_id, product_category_1, @product_category_1_name, product_category_2, @product_category_2_name, product_category_3, @product_category_3_name, qty, sales)
set transaction_timestamp = DATE_FORMAT(STR_TO_DATE(@transaction_timestamp,'%Y-%m-%dT%H:%i:%s.000Z'),'%Y-%m-%d %H:%i:%s'),
 product_category_2_name=substring(@product_category_2_name,1,100),
 product_category_1_name=substring(@product_category_1_name,1,100),
 product_category_3_name=substring(@product_category_3_name,1,100);

select * from muebles;