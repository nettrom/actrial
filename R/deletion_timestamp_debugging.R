## Debugging deletion timestamps in the Data Lake

## NOTE: I need to look more closely at this. One approach that might work is to
## create a table in the staging database and do some checks against the revision,
## logging, and archive tables to figure out where the deletion timestamps come from
## in the Data Lake.

## Read in dataset of July 2015 creations
july2015_creations = fread('datasets/2015-creations-testset.tsv',
                           na.strings=c('NULL'));

## Take out any non-autoconfirmed creations
july2015_creations = july2015_creations[
  creation_data_bridge.event_user_age >= 4 &
    creation_data_bridge.event_user_revision_count >= 10];

setkey(july2015_creations, "creation_data_bridge.page_id");
setkey(autoconfirmed_articles, "event_page_id");

july2015_creations = autoconfirmed_articles[july2015_creations];

## Remove any page that doesn't have a creation timestamp set
july2015_creations = july2015_creations[!is.na(event_timestamp)];

## Now, parse the creation_data_bridge.revision_deleted_timestamp
july2015_creations[, c("lake_deletion_date", "lake_deletion_time") := IDateTime(
  as.POSIXct(creation_data_bridge.revision_deleted_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz='UTC'))];

## How many pages have a deletion timestamp in the Data Lake, but not in our dataset?
july2015_creations[is.na(deletion_date) & !is.na(lake_deletion_date),
                   list(event_page_id, event_timestamp,
                        creation_data_bridge.page_title,
                        creation_data_bridge.revision_deleted_timestamp)];

## How many pages have a deletion timestamp in our dataset, but not in the Data Lake?
july2015_creations[!is.na(deletion_date) & is.na(lake_deletion_date)];

## How many pages have the same timestamps for both?
july2015_creations[
  as.POSIXct(event_deletion_time, format='%Y-%m-%d %H:%M:%S', tz='UTC') ==
    as.POSIXct(creation_data_bridge.revision_deleted_timestamp,
               format='%Y-%m-%d %H:%M:%S.0', tz='UTC')];

july2015_creations[
  as.POSIXct(event_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC') ==
    as.POSIXct(creation_data_bridge.event_timestamp,
               format='%Y-%m-%d %H:%M:%S.0', tz='UTC')];

## Calculate the difference
july2015_creations[,
  diff_deletion_time :=
  as.POSIXct(event_deletion_time, format='%Y-%m-%d %H:%M:%S', tz='UTC') -
    as.POSIXct(creation_data_bridge.revision_deleted_timestamp,
               format='%Y-%m-%d %H:%M:%S.0', tz='UTC')];

tail(
july2015_creations[!is.na(diff_deletion_time)][order(diff_deletion_time)][
  ,list(event_page_id, event_timestamp, creation_data_bridge.page_title,
        event_deletion_time, creation_data_bridge.revision_deleted_timestamp,
        diff_del_time = as.numeric(diff_deletion_time)/(60*60*24)
        )], 100);
