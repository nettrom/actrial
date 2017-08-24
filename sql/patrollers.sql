-- Database schema for statistics that allows us to determine the distribution
-- of patroller workload.

USE s53463__actrial_p;
CREATE TABLE patrollers (
  pat_date DATE NOT NULL,
  pat_userid INT UNSIGNED NOT NULL,
  pat_num_actions INT UNSIGNED DEFAULT 0,
  PRIMARY KEY (pat_date, pat_userid)
);
