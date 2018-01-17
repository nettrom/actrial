-- Database schema for table with statistics on number of deletions
-- per day per namespace with various reasons for deletions.

USE s53463__actrial_p;
CREATE TABLE deletion_reasons (
  dr_date DATE NOT NULL,
  dr_namespace INT NOT NULL DEFAULT 0,
  num_g1 INT NOT NULL DEFAULT 0, -- patent nonsense
  num_g2 INT NOT NULL DEFAULT 0, -- test pages
  num_g3 INT NOT NULL DEFAULT 0, -- pure vandalism, blatant hoaxes
  num_g4 INT NOT NULL DEFAULT 0, -- recreation of a deleted page
  num_g5 INT NOT NULL DEFAULT 0, -- creations by banned/blocked users
  num_g6 INT NOT NULL DEFAULT 0, -- technical deletions
  num_g7 INT NOT NULL DEFAULT 0, -- author requests deletions
  num_g8 INT NOT NULL DEFAULT 0, -- pages dependent on non-existent/deleted page
  num_g9 INT NOT NULL DEFAULT 0, -- office actions
  num_g10 INT NOT NULL DEFAULT 0, -- attack pages
  num_g11 INT NOT NULL DEFAULT 0, -- unambiguous advertising/promotion
  num_g12 INT NOT NULL DEFAULT 0, -- unambiguous copyright infringement
  num_g13 INT NOT NULL DEFAULT 0, -- abandoned drafts/articles for creation
  num_a1 INT NOT NULL DEFAULT 0, -- no context
  num_a2 INT NOT NULL DEFAULT 0, -- foreign language article
  num_a3 INT NOT NULL DEFAULT 0, -- no content
  num_a5 INT NOT NULL DEFAULT 0, -- transwikied article
  num_a7 INT NOT NULL DEFAULT 0, -- no indication of importance
  num_a9 INT NOT NULL DEFAULT 0,  -- no indication of importance (music)
  num_a10 INT NOT NULL DEFAULT 0, -- duplicates existing topic
  num_a11 INT NOT NULL DEFAULT 0, -- obviously invented
  num_u1 INT NOT NULL DEFAULT 0, -- user request
  num_u2 INT NOT NULL DEFAULT 0, -- nonexistent user
  num_u3 INT NOT NULL DEFAULT 0, -- non-free galleries
  num_u5 INT NOT NULL DEFAULT 0, -- blatant misuse of Wikipedia as a web host
  num_prod INT NOT NULL DEFAULT 0, -- deletion through PROD or BLPPROD
  num_afd INT NOT NULL DEFAULT 0, -- deletion through AfD
  num_other INT NOT NULL DEFAULT 0, -- other reasons
  PRIMARY KEY(dr_date, dr_namespace)
);
