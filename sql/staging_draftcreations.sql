-- Database schema for data about article creations, which we'll match
-- up against a user's first edit to see if they started out by creating
-- an article.
USE staging;
CREATE TABLE nettrom_draftcreations (
    dc_page_id INT UNSIGNED NOT NULL,
    dc_timestamp BINARY(14) NOT NULL DEFAULT '',
    dc_rev_id INT UNSIGNED NOT NULL,
    KEY ac_time_idx (ac_timestamp)
);
