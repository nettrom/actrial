-- Query to extract all creation events of Draft pages made on
-- the English Wikipedia done by accounts that are less than
-- 30 days old. The query makes the following assumptions:

-- 1: All creations in namespace 118 are drafts
-- 2: Some of the creations are redirects or moves. These can be identified
--    from the edit comments and will be excluded from the dataset.
--    (This is most likely not important, the page creation table in the log
--     database only contains 91 pages created as redirects).
-- 3: No drafts are created by anonmyous users (this turns out to be true)
-- 4: Users with autopatrol rights (the "sysop", "autoreviewer", and "bot"
--    groups) do not create drafts we are interested in.

USE wmf;
CREATE TABLE nettrom_articlecreations.draft_data AS
SELECT event_timestamp, page_id, page_title, revision_id,
       event_user_id, event_user_groups, event_user_revision_count,
       unix_timestamp(event_timestamp) -
       unix_timestamp(coalesce(event_user_creation_timestamp,
                              '20050101000000'))
       AS event_user_age
FROM mediawiki_history
WHERE wiki_db = 'enwiki'
  AND event_entity = 'revision'
  AND event_type = 'create'
  AND page_namespace = 118
  AND page_revision_count = 1
  AND (event_comment IS NULL
       OR (event_comment IS NOT NULL
           AND LCASE(event_comment) NOT REGEXP 'redir'
           AND LCASE(event_comment) NOT REGEXP '^rd'
           AND event_comment NOT REGEXP '\\{\\{R from '
           AND event_comment NOT REGEXP '.*moved .*\\[\\[([^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\].*'))
  AND NOT (ARRAY_CONTAINS(event_user_groups, "sysop")
           OR ARRAY_CONTAINS(event_user_groups, "autoreviewer")
           OR ARRAY_CONTAINS(event_user_groups, "bot"))
  AND event_user_id > 0
  AND TO_DATE(event_timestamp) >= '2009-01-01'
  AND snapshot = '2017-11';

-- For safety's sake, do autopatrolled users create any drafts?
-- 1,905 creations since Jan 1, 2016. Compared to 42,959 creations
-- by non-autopatrolled users. In our context, though, how interested
-- are we in the survival of a user with autopatrol rights? Also, they
-- create 4.2% of all drafts over roughly 18 monts, they're not interesting.
SELECT count(*) AS num_creations
FROM mediawiki_history
WHERE wiki_db = 'enwiki'
  AND event_entity = 'revision'
  AND event_type = 'create'
  AND page_namespace = 118
  AND page_revision_count = 1
  AND (event_comment IS NULL
       OR (event_comment IS NOT NULL
           AND LCASE(event_comment) NOT REGEXP 'redir'
           AND LCASE(event_comment) NOT REGEXP '^rd'
           AND event_comment NOT REGEXP '\\{\\{R from '
           AND event_comment NOT REGEXP '.*moved .*\\[\\[([^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\].*'))
  AND NOT (ARRAY_CONTAINS(event_user_groups, "sysop")
           OR ARRAY_CONTAINS(event_user_groups, "autoreviewer")
           OR ARRAY_CONTAINS(event_user_groups, "bot"))
  AND event_user_id > 0
  AND TO_DATE(event_timestamp) >= '2016-01-01'
  AND snapshot = '2017-07_private'
  LIMIT 5;
  