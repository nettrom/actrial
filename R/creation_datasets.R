## Combine the historic and recent datasets of non-autopatrolled article creations
## for usage in various analyses.

historic_creations = fread('bunzip2 -c datasets/creations_main_unpatrolled_historic.tsv.bz2');
recent_creations = fread('bunzip2 -c datasets/creations_main_unpatrolled_recent.tsv.bz2');
draft_creations = fread('bunzip2 -c datasets/creations_draft_complete.tsv.bz2');

join_creation_datasets = function(historic_datafile, recent_datafile) {
  ## Read in the historic and recent datasets in the given files, clean them up,
  ## and return a data.table of both sets combined (without duplicates).
  historic_creations = fread(paste('bunzip2 -c', historic_datafile),
                             na.strings = c('NULL'));
  recent_creations = fread(paste('bunzip2 -c', recent_datafile),
                           na.strings = c('NULL'));
  
  ## Remove the page namespace, page title, and user group columns from the recent
  ## dataset, they are not present in the historic dataset.
  recent_creations[, page_namespace := NULL];
  recent_creations[, page_title := NULL];
  recent_creations[, performer_user_groups := NULL];
  
  ## Parse the timestamps into creation_date and creation_time columns, then
  ## delete the original timestamp column.
  historic_creations[, c("creation_date", "creation_time") := IDateTime(
    as.POSIXct(event_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz='UTC'))];
  recent_creations[, c("creation_date", "creation_time") := IDateTime(
    as.POSIXct(rev_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];
  historic_creations[, event_timestamp := NULL];
  recent_creations[, rev_timestamp := NULL];
  
  ## Rename columns in recent_creations so that they match historic_creations
  setnames(recent_creations, 'rev_id', 'revision_id');
  setnames(recent_creations, 'performer_user_id', 'event_user_id');
  setnames(recent_creations, 'performer_user_edit_count', 'event_user_revision_count');
  setnames(recent_creations, 'performer_account_age', 'event_user_age');
  
  ## Combine the two datasets, limiting the historic dataset to creations prior to
  ## July 21, 2017, which is where the recent dataset takes over.
  rbind(
    historic_creations[creation_date < '2017-07-21'],
    recent_creations);
}

non_autopatrolled_creations_main = join_creation_datasets(
  'datasets/creations_main_unpatrolled_historic.tsv.bz2',
  'datasets/creations_main_unpatrolled_recent.tsv.bz2'
);

non_autopatrolled_creations_main = rbind(
  historic_creations[creation_date < '2017-07-21'],
  recent_creations);
rm(historic_creations, recent_creations);

## Parse the timestamp in the draft creation dataset and delete the column:
draft_creations[, c("creation_date", "creation_time") := IDateTime(
  as.POSIXct(event_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz='UTC'))];
draft_creations[, event_timestamp := NULL];

all_creations_main = join_creation_datasets(
  'datasets/creations_main_historic.tsv.bz2',
  'datasets/creations_main_recent.tsv.bz2'
);
