-- SQL code to export a dataset of quality predictions for articles after
-- 30 days. Because H14 studies survival rate, we will interpret H21 to
-- only require articles that lasted at least 30 days.
SELECT page_id, creation_timestamp, deletion_timestamp,
       creation_rev_id, 30day_rev_id, redirect_at_30,
       p1.draft_prediction AS c_draft_prediction,
       p1.spam_prob AS c_spam_prob,
       p1.vandal_prob AS c_vandal_prob,
       p1.attack_prob AS c_attack_prob,
       p1.ok_prob AS c_ok_prob,
       p1.wp10_prediction AS c_wp10_prediction,
       p1.stub_prob AS c_stub_prob,
       p1.start_prob AS c_start_prob,
       p1.c_prob AS c_c_prob,
       p1.b_prob AS c_b_prob,
       p1.ga_prob AS c_ga_prob,
       p1.fa_prob AS c_fa_prob,
       p2.draft_prediction AS s_draft_prediction,
       p2.spam_prob AS s_spam_prob,
       p2.vandal_prob AS s_vandal_prob,
       p2.attack_prob AS s_attack_prob,
       p2.ok_prob AS s_ok_prob,
       p2.wp10_prediction AS s_wp10_prediction,
       p2.stub_prob AS s_stub_prob,
       p2.start_prob AS s_start_prob,
       p2.c_prob AS s_c_prob,
       p2.b_prob AS s_b_prob,
       p2.ga_prob AS s_ga_prob,
       p2.fa_prob AS s_fa_prob
FROM nettrom_surviving_creations c
JOIN nettrom_articlecreation_predictions p1
ON c.creation_rev_id=p1.rev_id
JOIN nettrom_articlecreation_predictions p2
ON c.30day_rev_id=p2.rev_id
AND (deletion_timestamp IS NULL
     OR TIMESTAMPDIFF(SECOND,
                      deletion_timestamp,
                      creation_timestamp) > 60*60*24*30);
