-- Query to get a dataset of unreviewed articles
SELECT l1.log_page, l1.log_timestamp AS log_review_timestamp,
l2.log_timestamp AS log_unreview_timestamp, l1.log_user AS log_review_user,
l2.log_user AS log_unreview_user
FROM logging_logindex l1
JOIN logging_logindex l2
USING (log_page, log_type)
WHERE l1.log_timestamp >= "20121001000000"
AND l1.log_timestamp < "20170701000000"
AND l1.log_namespace = 0
AND l1.log_type="pagetriage-curation"
AND l1.log_action="reviewed"
AND l2.log_timestamp > l1.log_timestamp
AND l2.log_action="unreviewed";
