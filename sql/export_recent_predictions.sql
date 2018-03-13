-- SQL statement to export predictions for creations after July 1, 2017
-- from our article quality prediction table.

SELECT *
FROM  nettrom_articlecreation_predictions
WHERE rev_timestamp >= '2017-07-01 00:00:00';
