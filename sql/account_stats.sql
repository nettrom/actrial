-- Database schema for ACTRIAL-relevant statistics about
-- newly registered accounts.
USE s53463__actrial_p;
CREATE TABLE account_stats (
    as_userid INT UNSIGNED PRIMARY KEY,
    as_reg_timestamp DATETIME NOT NULL,
    -- How the account was created
    -- ("create", "create2", "autocreate", or "byemail")
    as_create_type VARCHAR(10),
    -- 0 if they edited an existing article, 1 if their first edit created
    -- an article, 2 if the re-created an article that had previously been
    -- deleted.
    as_created_article TINYINT DEFAULT 0,
    -- 1 if their first article (or a re-created article) survived for 30 days
    as_article_survived TINYINT DEFAULT 0,
    -- Number of edits in the first 30 days
    as_num_edits_30 INT DEFAULT 0,
    -- 1 if the account reached autoconfirmed status in 30 days,
    -- timestamp set if the account reached it
    as_autoconfirmed_30 TINYINT DEFAULT 0,
    as_autoconfirmed_30_timestamp DATETIME,
    -- Number of edits in the first week and fifth week (for survival analysis)
    as_num_edits_week_1 INT DEFAULT 0,
    as_num_edits_week_5 INT DEFAULT 0,
    -- Number of namespaces and pages edited in the first 30 days
    as_num_namespaces_30 TINYINT DEFAULT 0,
    as_num_pages_30 INT DEFAULT 0,
    KEY reg_idx (as_reg_timestamp)
);
