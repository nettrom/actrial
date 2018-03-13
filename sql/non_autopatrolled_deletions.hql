-- Query to combine our data on page creations with log data of page deletions.

SELECT count(*) FROM
  (SELECT * FROM nettrom_articlecreations.creation_data) AS cdata
  JOIN enwiki.revision r
  ON cdata.revision_id=r.rev_id
  JOIN enwiki.logging l
  ON 
  