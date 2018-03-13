-- Database schema for table with autoconfirmed article creations.
USE staging;
CREATE TABLE nettrom_autoconfirmed_creations (
    event_timestamp DATETIME NOT NULL,
    event_page_id INT UNSIGNED NOT NULL,
    event_user_id INT UNSIGNED NOT NULL DEFAULT 0,
    event_user_edit_count INT UNSIGNED NOT NULL DEFAULT 0,
    event_user_age INT UNSIGNED NOT NULL DEFAULT 0,
    event_deletion_time DATETIME,
    PRIMARY KEY (event_timestamp, event_page_id),
    KEY acc_page_idx (event_page_id)
);
