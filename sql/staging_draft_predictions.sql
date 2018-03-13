-- Database schema for ORES predictions for Draft pages.
USE staging;
CREATE TABLE nettrom_draft_predictions (
       rev_id INT NOT NULL PRIMARY KEY,
       rev_timestamp DATETIME NOT NULL,
       draft_prediction VARCHAR(32) NOT NULL DEFAULT 'OK',
       spam_prob FLOAT NOT NULL DEFAULT 0.0,
       vandal_prob FLOAT NOT NULL DEFAULT 0.0,
       attack_prob FLOAT NOT NULL DEFAULT 0.0,
       ok_prob FLOAT NOT NULL DEFAULT 0.0,
       wp10_prediction VARCHAR(8) NOT NULL DEFAULT 'Stub',
       stub_prob FLOAT NOT NULL DEFAULT 0.0,
       start_prob FLOAT NOT NULL DEFAULT 0.0,
       c_prob FLOAT NOT NULL DEFAULT 0.0,
       b_prob FLOAT NOT NULL DEFAULT 0.0,
       ga_prob FLOAT NOT NULL DEFAULT 0.0,
       fa_prob FLOAT NOT NULL DEFAULT 0.0
);

CREATE TABLE nettrom_drafts (
    page_id INT NOT NULL PRIMARY KEY,
    creation_timestamp DATETIME,
    publication_timestamp DATETIME, -- set if published (i.e. moved to Main)
    deletion_timestamp DATETIME -- set if deleted
);

CREATE TABLE nettrom_afc_submissions (
    page_id INT NOT NULL,
    submission_time DATETIME NOT NULL,
    review_time DATETIME, -- set if we know when it was reviewed, e.g. declined
    submission_status ENUM('', 'T', 'R', 'D') NOT NULL, -- '' means submitted
    rev_id INT NOT NULL,
    PRIMARY KEY (page_id, submission_time)
);
