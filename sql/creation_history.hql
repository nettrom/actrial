-- Query to extract all main namespace page creation events made on the
-- English Wikipedia. This query is based on
-- https://phabricator.wikimedia.org/T149021#3281771
-- but adapted to remove current redirects and use improved redirect
-- detection, both mentioned later in the same Phabricator task.
-- Note: this does not identify articles published by moving them into
-- the main namespace from other namespaces.

USE wmf;

SELECT event_timestamp, revision_id, event_user_id
FROM mediawiki_history
WHERE wiki_db = 'enwiki'
  AND event_entity = 'revision'
  AND event_type = 'create'
  AND page_namespace = 0
  AND revision_parent_id = 0
  AND LCASE(event_comment) NOT REGEXP 'redir'
  AND event_comment NOT REGEXP '\\{\\{R from '
  AND event_comment NOT REGEXP '.*moved .*\\[\\[([^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\].*'
  AND TO_DATE(event_timestamp) >= '2009-01-01'
  AND event_user_id IS NOT NULL
  AND snapshot = '2017-07';
