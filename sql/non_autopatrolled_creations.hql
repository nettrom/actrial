-- Query to extract all main namespace page creation events made on the
-- English Wikipedia that would require the created page to be reviewed.
-- This query is based on creation_history.hql, which contain notes on
-- some of the checks it applies. We augment that query with checks of
-- user groups that result in their creations being autopatrolled,
-- ref https://en.wikipedia.org/wiki/Special:ListGroupRights

USE wmf;
SELECT event_timestamp, page_id
FROM mediawiki_history
WHERE wiki_db = 'enwiki'
  AND event_entity = 'revision'
  AND event_type = 'create'
  AND ((page_namespace = 0
        AND revision_parent_id = 0)
       OR
       (page_namespace IS NULL
        AND page_namespace_latest = 0
        AND revision_parent_id IS NULL
        AND page_revision_count = 1))
  AND (event_comment IS NULL
       OR (event_comment IS NOT NULL
           AND LCASE(event_comment) NOT REGEXP 'redir'
           AND LCASE(event_comment) NOT REGEXP '^rd'
           AND event_comment NOT REGEXP '\\{\\{R from '
           AND event_comment NOT REGEXP '.*moved .*\\[\\[([^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\].*'))
  AND TO_DATE(event_timestamp) >= '2009-01-01'
  AND NOT (ARRAY_CONTAINS(event_user_groups, "sysop")
           OR ARRAY_CONTAINS(event_user_groups, "autoreviewer")
           OR ARRAY_CONTAINS(event_user_groups, "bot"))
  AND snapshot = '2017-07_private';
