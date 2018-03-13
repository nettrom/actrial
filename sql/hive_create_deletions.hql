USE wmf;
CREATE TABLE nettrom_articlecreations.deletion_data AS
SELECT m1.page_id, m1.event_timestamp, m1.event_type
FROM mediawiki_history m1
JOIN nettrom_articlecreations.creation_data c1
ON m1.page_id = c1.page_id
WHERE m1.wiki_db = 'enwiki'
AND m1.event_entity = 'page'
AND (m1.event_type = 'create'
     OR m1.event_type = 'delete')
AND TO_DATE(m1.event_timestamp) >= '2009-01-01'
AND m1.snapshot = '2017-07_private';
