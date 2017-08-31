-- Database schema for data about article creations, which we'll match
-- up against a user's first edit to see if they started out by creating
-- an article.
USE staging;
CREATE TABLE nettrom_articlecreations (
    ac_page_id INT UNSIGNED NOT NULL,
    ac_timestamp BINARY(14) NOT NULL,
    PRIMARY KEY (ac_page_id, ac_timestamp),
    KEY ac_time_idx (ac_timestamp)
);
