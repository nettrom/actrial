-- Database schema for table containing data on article creations,
-- enabling us to identify how long they survived.
USE staging;
CREATE TABLE nettrom_surviving_creations (
       page_id INT UNSIGNED NOT NULL,
       creation_timestamp DATETIME NOT NULL,
       creation_rev_id INT UNSIGNED NOT NULL,
       30day_rev_id INT UNSIGNED, -- most recent revision at 30 days
       -- redirect flag is 1 if the article is a redirect at day 30
       redirect_at_30 TINYINT UNSIGNED NOT NULL DEFAULT 0,
       deletion_timestamp DATETIME,
       PRIMARY KEY (page_id),
       KEY ctime_idx (creation_timestamp)
);
