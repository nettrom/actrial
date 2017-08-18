-- Database schema for table with counts of moves from User and Draft
-- namespaces into the article namespace.
USE s53463__actrial_p;
CREATE TABLE moves_into_main (
   mim_date DATE PRIMARY KEY,
   mim_num_moves_user INT,
   mim_num_moves_draft INT
);
