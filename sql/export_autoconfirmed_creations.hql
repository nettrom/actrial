-- Query to export data from our database table of non-autopatrolled,
-- autoconfirmed article creations to answer H14.

USE nettrom_articlecreations;
SELECT event_timestamp, page_id, event_user_id,
       event_user_revision_count, event_user_age
FROM unpatrolled_creations
WHERE event_user_revision_count >= 10
AND event_user_age >= 60*60*24*4;
