-- Query to extract all main namespace page creation events made on the
-- English Wikipedia that would require the created page to be reviewed.
-- This query is a modification of hive_create_creations.hql. We only
-- gather data for the month of July 2017 because we previously gathered
-- data up to July 1, 2017, and from July 22 onwards we can use data from
-- the page creation table in the log database. The `page_namespace_latest`
-- field has been altered in the 2017-11 snapshot, leading us to rewrite
-- that part of the query.

USE wmf;
CREATE TABLE nettrom_articlecreations.creation_data_bridge AS
SELECT event_timestamp, page_id, page_title, revision_id,
       event_user_id,  event_user_groups, event_user_revision_count,
       unix_timestamp(event_timestamp) -
       unix_timestamp(coalesce(event_user_creation_timestamp,
                              '20050101000000'))
       AS event_user_age,
       revision_deleted_timestamp
FROM mediawiki_history
WHERE wiki_db = 'enwiki'
  AND event_entity = 'revision'
  AND event_type = 'create'
  AND ((page_namespace = 0
        AND revision_parent_id = 0)
       OR
       (page_namespace_historical IS NULL
        AND page_namespace = 0
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
  AND TO_DATE(event_timestamp) >= '2010-07-01'
  AND TO_DATE(event_timestamp) < '2010-08-01'
  AND snapshot = '2017-11';

INSERT INTO nettrom_articlecreations.creation_data_bridge
SELECT event_timestamp, page_id, page_title, revision_id,
       event_user_id,  event_user_groups, event_user_revision_count,
       unix_timestamp(event_timestamp) -
       unix_timestamp(coalesce(event_user_creation_timestamp,
                              '20050101000000'))
       AS event_user_age,
       revision_deleted_timestamp
FROM mediawiki_history
WHERE wiki_db = 'enwiki'
  AND event_entity = 'revision'
  AND event_type = 'create'
  AND ((page_namespace = 0
        AND revision_parent_id = 0)
       OR
       (page_namespace_historical IS NULL
        AND page_namespace = 0
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
  AND TO_DATE(event_timestamp) >= '2015-07-01'
  AND TO_DATE(event_timestamp) < '2015-08-01'
  AND snapshot = '2017-11';
