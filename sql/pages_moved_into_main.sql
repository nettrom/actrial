-- Database schema for table with data on page moves into the Main
-- namespace from Wikipedia talk, Draft, and User namespaces.
USE s53463__actrial_p;
CREATE TABLE pages_moved_into_main (
    pmim_page_id INT NOT NULL,
    pmim_timestamp DATETIME NOT NULL,
    pmim_source_namespace INT NOT NULL DEFAULT 118,
    PRIMARY KEY (pmim_page_id, pmim_timestamp)
);
