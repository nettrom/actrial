-- Database schema for database table capturing snapshots of the size
-- of the review queue.
USE s53463__actrial_p;
CREATE TABLE npp_queue_size (
    npp_timestamp DATETIME NOT NULL PRIMARY KEY,
    npp_num_articles INT UNSIGNED NOT NULL DEFAULT 0
);
