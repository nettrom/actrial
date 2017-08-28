-- Database schema for data about article creations, which we'll match
-- up against a user's first edit to see if they started out by creating
-- an article.
USE s53463__actrial_p;
CREATE TABLE articlecreations (
    ac_rev_id INT UNSIGNED PRIMARY KEY,
    ac_timestamp DATETIME NOT NULL
);
