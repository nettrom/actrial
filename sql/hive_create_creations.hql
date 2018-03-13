-- Query to extract all main namespace page creation events made on the
-- English Wikipedia that would require the created page to be reviewed.
-- This query is based on creation_history.hql, which contain notes on
-- some of the checks it applies. We augment that query with checks for
-- user groups that result in their creations being autopatrolled so
-- those article creations can be ignored, ref:
-- https://en.wikipedia.org/wiki/Special:ListGroupRights

-- Note: added check for event_user_id being 0 as the query otherwise
-- defines a few non-registered edits as creations, which is obviously wrong.

USE wmf;
CREATE TABLE nettrom_articlecreations.creation_data AS
SELECT event_timestamp, page_id, page_title, revision_id,
       event_user_id,  event_user_groups, event_user_revision_count,
       unix_timestamp(event_timestamp) -
       unix_timestamp(coalesce(event_user_creation_timestamp,
                              '20050101000000'))
       AS event_user_age
FROM mediawiki_history
WHERE wiki_db = 'enwiki'
  AND event_entity = 'revision'
  AND event_type = 'create'
  AND ((page_namespace = 0
        AND revision_parent_id = 0)
       OR
       (page_namespace IS NULL
        AND page_namespace_latest = 0
        AND (revision_parent_id IS NULL
             OR revision_parent_id = 0)))
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
  AND snapshot = '2017-07_private';
