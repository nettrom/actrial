-- Query to count the number of deletions per day in the draft namespace
-- from 2015-01-01 onwards. We can then correlate that with the historic
-- AfC backlog data.

-- Note: the log comments should identify the CSD criteria when available,
-- meaning we can use that for more refined statistics on reasons if necessary.

SELECT DATE(log_timestamp) AS log_date, count(*) AS num_draft_deletions
FROM logging
WHERE log_timestamp > '20150101000000'
AND log_namespace = 118
AND log_type = 'delete'
AND log_action = 'delete'
GROUP BY log_date;
