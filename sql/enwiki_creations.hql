-- Query to extract all main namespace page creation events made on the
-- English Wikipedia.
-- This query is a modification of hive_create_creations.hql. We only
-- gather data from July 1, 2014 through July 2017. The former date is
-- chosen because we need trustworthy deletion data, and the second is
-- chosen because after July 22 we have data in the log database.
-- The `page_namespace_latest` field has been altered in the 2017-12 snapshot,
-- leading us to rewrite that part of the query.

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
  AND event_user_id > 0
  AND TO_DATE(event_timestamp) >= '2014-07-01'
  AND TO_DATE(event_timestamp) < '2017-08-01'
  AND snapshot = '2017-12';
