-- Database schema for database with counts of newly created accounts
USE s53463__actrial_p;
CREATE TABLE newaccounts (
   na_date DATE PRIMARY KEY,
   na_newusers INT,
   na_autocreate INT,
   na_byemail INT,
   na_create INT,
   na_create2 INT
);
