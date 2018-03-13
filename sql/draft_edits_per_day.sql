SELECT rev_date, count(*) AS num_edits
FROM ((SELECT DATE(rev_timestamp) AS rev_date
       FROM revision
       JOIN page
       ON rev_page=page_id
       WHERE page_namespace = 118
       AND rev_timestamp >= '20170101000000')
      UNION ALL
      (SELECT DATE(ar_timestamp) AS rev_date
       FROM archive
       WHERE ar_namespace = 118
       AND ar_timestamp >= '20170101000000')
      ) AS revisions
GROUP BY rev_date;
