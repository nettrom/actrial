-- Query to extract the number of main namespace page creations made on the
-- English Wikipedia per day. This query is based on
-- https://phabricator.wikimedia.org/T149021#3281771
-- but adapted to remove current redirects and use the improved redirect
-- detection, both mentioned later in the same Phabricator task.
-- Note: this does not identify articles published by moving them into
-- the main namespace from other namespaces.
USE wmf;
SELECT TO_DATE(event_timestamp) AS day,
       COUNT(*) AS num_creations
FROM mediawiki_history
WHERE wiki_db = 'enwiki'
  AND event_entity = 'revision'
  AND event_type = 'create'
  AND page_namespace = 0
  AND revision_parent_id = 0
  AND LCASE(event_comment) NOT REGEXP 'redir'
  AND event_comment NOT REGEXP '\\{\\{R from '
  AND event_comment NOT REGEXP '.*moved .*\\[\\[([^\]]+)\\]\\] to \\[\\[([^\]]+)\\]\\].*'
  AND snapshot = '2017-07'
  AND TO_DATE(event_timestamp) >= '2017-01-01'
GROUP BY TO_DATE(event_timestamp)
ORDER BY day
LIMIT 10000;
