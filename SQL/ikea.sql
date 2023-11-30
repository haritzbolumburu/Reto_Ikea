DROP DATABASE IF EXISTS IKEA;
CREATE DATABASE IKEA;
USE IKEA;


CREATE TABLE muebles (
 store                      VARCHAR(5),
 transaction_id             VARCHAR(70),
  transaction_timestamp     TIMESTAMP,
 membership_id              VARCHAR(70),
 product_category_1         VARCHAR(100),
 product_category_1_name    VARCHAR(100),
 product_category_2         VARCHAR(100),
 product_category_2_name    VARCHAR(100),
 product_category_3         VARCHAR(100),
 product_category_3_name    VARCHAR(100),
 qty                        DECIMAL(8,4),
 sales                      DECIMAL(8,4)
);
