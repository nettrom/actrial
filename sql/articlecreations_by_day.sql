-- Database schema for database with counts of newly created accounts
USE s53463__actrial_p;
CREATE TABLE article_creations_by_day (
   na_date DATE PRIMARY KEY,
   na_num_creations INT
);
