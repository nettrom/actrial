-- The page ID and timestamp of all main namespace deletions from
-- July 1, 2014 up to 90 days past the first two months of ACTRIAL.
-- We use the July 2014 date because we are fairly certain that at that point
-- page IDs in deletions are trustworthy.

SELECT log_page, log_timestamp
FROM logging
WHERE log_type = 'delete'
AND log_action = 'delete'
AND log_namespace = 0
AND log_timestamp >= '20140701000000'
AND log_timestamp < '20180213000000';
