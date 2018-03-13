SELECT event_timestamp, event_user_id, event_user_revision_count, event_user_age
FROM nettrom_articlecreations.draft_data
WHERE event_user_age < 30*24*60*60;
