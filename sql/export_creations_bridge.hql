-- Query to export data from our database table of non-autopatrolled,
-- autoconfirmed article creations to answer H14.

USE nettrom_articlecreations;
SELECT event_timestamp, page_id, revision_id, event_user_id,
       event_user_revision_count, event_user_age
FROM creation_data_bridge;
