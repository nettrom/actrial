-- SQL statement to export all non-redirecting, non-autopatrolled article
-- creations done by accounts less than 30 days old from the page creation table.
SELECT rev_timestamp AS event_timestamp,
       rev_id AS revision_id,
       performer_user_id AS event_user_id,
       performer_user_edit_count AS event_user_revision_count,
       TIMESTAMPDIFF(SECOND, performer_user_registration_dt, rev_timestamp)
       AS event_user_age
FROM mediawiki_page_create_2
WHERE `database` = 'enwiki'
AND rev_timestamp >= '2017-07-21 00:00:00'
AND page_namespace = 0
AND page_is_redirect = 0
AND performer_user_groups NOT REGEXP 'sysop|autoreviewer|bot'
AND TIMESTAMPDIFF(SECOND, performer_user_registration_dt, rev_timestamp) < 30*24*60*60;
