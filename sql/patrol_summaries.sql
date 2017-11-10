-- Database schema defining a view that gives us the number of active
-- patrollers and the number of patrol actions they performed on a given
-- day, based on the patrol actions per patroller per day statistics.

USE s53463__actrial_p;
CREATE VIEW patrol_summaries AS
SELECT pat_date AS ps_date,
       COUNT(pat_userid) AS ps_num_patrollers,
       SUM(pat_num_actions) AS ps_num_patrol_actions
FROM patrollers
GROUP BY pat_date;
