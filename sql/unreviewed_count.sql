-- How often is an article reviewed then unreviewed?

-- Count of reviews logged by the PageTriage extension
SELECT COUNT(*) AS num_reviews
FROM logging_logindex
WHERE log_namespace=0
AND log_timestamp >= "20121001000000"
AND log_timestamp < "20170701000000"
AND log_type="pagetriage-curation"
AND log_action="reviewed";

-- Count of unreviews to a given article that had a review
SELECT COUNT(*) AS num_rereviews
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

-- We'll later look at time to unreview to see if we need to
-- add a time constraint:
AND l2.log_timestamp < DATE_FORMAT(DATE_ADD(
    STR_TO_DATE(user_registration, "%Y%m%d%H%i%S"),
    INTERVAL 7 DAY), "%Y%m%d%H%i%S")

-- Count of review-unreview-review triplets done by the same user
-- where the time between events is less than an hour.
SELECT COUNT(*) AS num_unre_triplets
FROM logging_logindex l1
JOIN logging_logindex l2
USING (log_page, log_type, log_user)
JOIN logging_logindex l3
USING (log_page, log_type, log_user)
WHERE l1.log_timestamp >= "20121001000000"
AND l1.log_timestamp < "20170701000000"
AND l1.log_namespace = 0
AND l1.log_type="pagetriage-curation"
AND l1.log_action="reviewed"
AND l2.log_timestamp > l1.log_timestamp
AND l2.log_timestamp < DATE_FORMAT(DATE_ADD(
    STR_TO_DATE(l1.log_timestamp, "%Y%m%d%H%i%S"),
    INTERVAL 1 HOUR), "%Y%m%d%H%i%S")
AND l2.log_action="unreviewed"
AND l3.log_timestamp > l2.log_timestamp
AND l3.log_timestamp < DATE_FORMAT(DATE_ADD(
    STR_TO_DATE(l2.log_timestamp, "%Y%m%d%H%i%S"),
    INTERVAL 1 HOUR), "%Y%m%d%H%i%S")
AND l3.log_action="reviewed";

-- Count of users who make unreview of other users
SELECT user_id, user_name, count(*) AS num_unreviews
FROM logging l1
JOIN logging l2
USING (log_page)
JOIN user
ON l2.log_user=user_id
WHERE l1.log_namespace=0
AND l1.log_timestamp >= '20170101000000'
AND l1.log_timestamp < '20170108000000'
AND ((l1.log_type="patrol"
      AND l1.log_params NOT LIKE '%::auto";i:1%')
     OR 
     (l1.log_type='pagetriage-curation'
      AND l1.log_action='reviewed'))
AND l2.log_type='pagetriage-curation'
AND l2.log_action='unreviewed'
AND l1.log_user != l2.log_user
AND l2.log_timestamp > l1.log_timestamp
AND l2.log_timestamp < DATE_FORMAT(DATE_ADD(
    STR_TO_DATE(l1.log_timestamp, "%Y%m%d%H%i%S"),
    INTERVAL 1 WEEK), "%Y%m%d%H%i%S")
GROUP BY user_id;

SELECT COUNT(*) AS num_other_reviews
FROM logging_logindex l1
JOIN logging_logindex l2
USING (log_page, log_type, log_user)
JOIN logging_logindex l3
USING (log_page, log_type)
WHERE l1.log_timestamp >= "20121001000000"
AND l1.log_timestamp < "20170701000000"
AND l1.log_namespace = 0
AND l1.log_type="pagetriage-curation"
AND l1.log_action="reviewed"
AND l2.log_timestamp > l1.log_timestamp
AND l2.log_timestamp < DATE_FORMAT(DATE_ADD(
    STR_TO_DATE(l1.log_timestamp, "%Y%m%d%H%i%S"),
    INTERVAL 1 HOUR), "%Y%m%d%H%i%S")
AND l2.log_action="unreviewed"
AND l3.log_user != l2.log_user
AND l3.log_timestamp > l2.log_timestamp
AND l3.log_action="reviewed";

SELECT COUNT(*) AS num_other_reviews
FROM logging_logindex l1
JOIN logging_logindex l2
USING (log_page, log_type, log_user)
JOIN logging_logindex l3
USING (log_page, log_type)
WHERE l1.log_timestamp >= "20170201000000"
AND l1.log_timestamp < "20170701000000"
AND l1.log_namespace = 0
AND l1.log_type="pagetriage-curation"
AND l1.log_action="reviewed"
AND l2.log_timestamp > l1.log_timestamp
AND l2.log_timestamp < DATE_FORMAT(DATE_ADD(
    STR_TO_DATE(l1.log_timestamp, "%Y%m%d%H%i%S"),
    INTERVAL 1 HOUR), "%Y%m%d%H%i%S")
AND l2.log_action="unreviewed"
AND l3.log_user != l2.log_user
AND l3.log_timestamp > l2.log_timestamp
AND l3.log_action="reviewed";
