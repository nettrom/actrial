-- Database schema for ORES predictions for newly created pages.
USE staging;
CREATE TABLE nettrom_articlecreation_predictions (
       rev_id INT UNSIGNED PRIMARY KEY,
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
