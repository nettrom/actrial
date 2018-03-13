-- Tables we'll have in staging to be able to gather datasets of relevant
-- features of created articles:


nettrom_articlecreations:
* page ID
* rev ID
* rev timestamp
* source ('direct', 'user', 'draft', where 'draft' means a move from either
          the Draft or Wikipedia talk namespaces)
* user ID
* user groups (as a comma-separated list?)
* user editcount (at article creation)
* user account age (at article creation, in seconds)

CREATE TABLE nettrom_creation_data (
   event_timestamp CHARACTER(19) NOT NULL,
   page_id INTEGER UNSIGNED NOT NULL,
   revision_id INTEGER UNSIGNED NOT NULL,
   user_id INTEGER UNSIGNED NOT NULL,
   user_groups VARCHAR(255) NOT NULL DEFAULT '',
   user_editcount INTEGER UNSIGNED NOT NULL DEFAULT 0,
   user_account_age INTEGER UNSIGNED NOT NULL DEFAULT 0
);

articlecreation_reviews:
* rev ID (primary key, the revision that created the article)
* reviewed rev ID (ID of the most recent revision not made by the reviewer)
* reviewer user ID
* review timestamp
* time to review (in seconds, diff between rev timestamp and review timestamp)

articlecreation_predictions:
* rev ID (either the revision that created an article,
          or its associated review rev ID)
* draft quality prediction
* four columns for the draft probabilities 
* wp10 quality prediction
* six columns for the wp10 probabilities

articlecreation_deletions:
* rev ID (revision that created the article)
* proposed deletion rev ID
* proposed deletion timestamp
* proposed delete type ('prod', 'afd', 'csd')
* proposed csd reason (code for CSD if delete type is 'csd')
* deletion rev ID (revision where it was deleted)
* deletion rev timestamp
* deletion type ('prod', 'afd', 'csd')
* deletion csd reason (code for CSD if delete type is 'csd')

CREATE TABLE nettrom_deletion_data (
    page_id INTEGER UNSIGNED NOT NULL,
    deletion_timestamp DATETIME NULL
);
