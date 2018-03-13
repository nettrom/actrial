-- Query to get article/draft creations from the log database. We want to get
-- all creations in Main/Draft, that were not redirects, made after midnight
-- on 2017-07-21 and before 2017-11-01, not made by an account with autopatrol
-- rights, and where the account was less than 30 days old.
-- We skipped searching for moves because they do not appear to exist in this
-- dataset.

SELECT rev_timestamp, page_namespace, page_id, page_title, rev_id,
       performer_user_id, performer_user_groups, performer_user_edit_count,
       TIMESTAMPDIFF(SECOND, performer_user_registration_dt, rev_timestamp)
       AS performer_account_age
FROM mediawiki_page_create_2
WHERE `database`='enwiki'
AND rev_timestamp >= '2017-07-21 00:00:00'
AND rev_timestamp < '2017-11-01 00:00:00'
AND page_namespace IN (0, 118) -- Main, Draft
AND page_is_redirect = 0
AND performer_user_groups NOT REGEXP 'sysop|autoreviewer|bot'
AND TIMESTAMPDIFF(SECOND, performer_user_registration_dt, rev_timestamp)/(60*60*24) < 30.0;
