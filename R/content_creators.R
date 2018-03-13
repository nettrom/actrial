## Studying how ACTRIAL has affected survival of newly registered accounts
## that create content. This is an adaptation of our hypothesis of surviving
## editors, adapted to only examine users who create either an article or draft
## during their first week on Wikipedia.

library(ggplot2);
library(data.table);

## Load in the datasets.

## 1: Dataset of creations from July 21 to November 1, 2017:
current_creations = fread('datasets/current_creations.tsv');

## 2: Historic datasets on creations in Main and Draft from Jan 1, 2009 to
##    July 1, 2017.
draft_creations_30 = fread('datasets/draft_creations_30days.tsv');
article_creations_30 = fread('datasets/article_creations_30days.tsv');

## Add a column for namespace, join the two datasets and delete the original ones.
draft_creations_30[, event_page_namespace := 118];
article_creations_30[, event_page_namespace := 0];

page_creations_30 = rbind(draft_creations_30, article_creations_30);
rm(draft_creations_30);
rm(article_creations_30);

## 3: Dataset of user activity from Jan 1, 2009 onwards:
useractivity = data.table(read.table("datasets/activity_stats_20171101.tsv.bz2",
                                     sep='\t', stringsAsFactors=FALSE, header=TRUE));

## When we're looking at data by the type of account creation, we are
## mainly interested in whether the account was autocreated or not. Thus, merge
## the types into "autocreate" and "create".
useractivity[as_create_type %in% c('create2', 'byemail'), as_create_type := 'create'];

## Extract registration date from the registration timestamp
useractivity[, c("as_reg_date", "as_reg_time") := IDateTime(as.POSIXct(as_reg_timestamp, tz='UTC'))];

## Didn't look like NULLs were NA'ed correctly. Also, timestamps are off for
## accounts created as part of the SUL coercion.
useractivity[as_autoconfirmed_30_timestamp == "NULL", as_autoconfirmed_30_timestamp := NA];
useractivity[as_autoconfirmed_30_timestamp == "0000-00-00 00:00:00",
             as_autoconfirmed_30_timestamp := NA];

useractivity[!is.na(as_autoconfirmed_30_timestamp),
             c("as_ac_30_date", "as_ac_30_time") := IDateTime(as.POSIXct(as_autoconfirmed_30_timestamp, tz='UTC'))]

## Some accounts were created with a historic registration date due to SUL finalisation.
useractivity[as_ac_30_date < as_reg_date,
             c('as_autoconfirmed_30_timestamp', 'as_ac_30_date', 'as_ac_30_time') := list(NA, NA, NA)];

## Calculate the time between registration and autoconfirmed status
useractivity[!is.na(as_autoconfirmed_30_timestamp),
             as_ac_30_delta := as.POSIXct(as_autoconfirmed_30_timestamp, tz='UTC') - as.POSIXct(as_reg_timestamp, tz='UTC')]
## Some accounts have autoconfirmed delta < 4 because we use timestamps from
## the logging table (might be better to just add a second to every timestamp?)
useractivity[!is.na(as_autoconfirmed_30_timestamp) & as_ac_30_delta < 4,
             as_ac_30_delta := 4];

setkey(useractivity, as_reg_date, as_create_type, physical=FALSE);

## Now, we want to join our creation datasets with the user activity dataset
## in order to learn whether survival is different from users who create articles,
## drafts, or do not create anything.

## 1: Merge the historic and recent survival datasets.
## Columns in the historic dataset: event_timestamp, event_user_id,
## eventuser_revision_count, event_user_age, event_page_namespace

## Columns in the recent dataset: rev_timestamp, page_namespace, page_id,
## page_title, rev_id, performer_user_id, performer_user_groups,
## performer_user_edit_count, performer_account_age

## Note: event_timestamp in the historic dataset has a ".0" 1/10s format,
## while rev_timestamp does not. Let's split up the values in both of these.
current_creations[, c("event_date", "event_time") := IDateTime(
  as.POSIXct(rev_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];
page_creations_30[, c("event_date", "event_time") := IDateTime(
  as.POSIXct(event_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz='UTC'))];

page_creations = rbind(
  page_creations_30[, list(event_date, event_time, event_user_id,
                           event_user_revision_count, event_user_age,
                           event_page_namespace)],
  current_creations[, list(event_date, event_time, event_user_id=performer_user_id,
                           event_user_revision_count=performer_user_edit_count,
                           event_user_age=performer_account_age,
                           event_page_namespace=page_namespace)]);
rm(page_creations_30);
rm(current_creations);

## We can now restrict page creations to accounts that were less than 7 days old,
## since we will measure survival for accounts creating an article/draft in the
## first week since registration.
page_creations = page_creations[event_user_age < 60*60*24*7];

## Grab the user info from useractivty and add them to page_creations.
setkey(page_creations, event_user_id);
setkey(useractivity, as_userid);
page_creations = page_creations[useractivity, nomatch=0];

## Q1: What is the survival rate of article and draft creators over time?
prop_surviving_creators = merge(
  page_creations[as_num_edits_week_1 > 0,
                 list(n_creators=sum(.N)), by=list(as_reg_date, as_create_type,
                                                   event_page_namespace)],
  page_creations[as_num_edits_week_1 > 0
                 & as_num_edits_week_5 > 0, list(n_survivers=sum(.N)),
                 by=list(as_reg_date, as_create_type, event_page_namespace)],
  by=c('as_reg_date', 'as_create_type', 'event_page_namespace'));

## Now, plot this over time and facet it by type of account creation
## and namespace of the created article.
ggplot(prop_surviving_creators, aes(x=as_reg_date, y=100*n_survivers/n_creators)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of draft/article creators surviving in week 5") +
  facet_grid(as_create_type ~ event_page_namespace);

## This plot seems to be most interesting from 2015 onwards, and hopefully that'll
## show more of what's happened around ACTRIAL. I'll add an ACTRIAL line to
## the plots as well.
ggplot(prop_surviving_creators[as_reg_date >= '2015-01-01'],
       aes(x=as_reg_date, y=100*n_survivers/n_creators)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of draft/article creators surviving in week 5") +
  facet_grid(as_create_type ~ event_page_namespace) +
  geom_vline(xintercept=as.Date('2017-09-15'), linetype='dashed');

## What's the mean/median survival rate for article & draft creators
## from 2015-01-01 to 2017-07-01?
summary(prop_surviving_creators[as_reg_date >= '2015-01-01'
                                & as_reg_date < '2017-07-01'
                                & event_page_namespace == 0,
                                list(prop_survivors=100*n_survivers/n_creators),
                                by=as_reg_date]);
summary(prop_surviving_creators[as_reg_date >= '2015-01-01'
                                & as_reg_date < '2017-07-01'
                                & event_page_namespace == 118,
                                list(prop_survivors=100*n_survivers/n_creators),
                                by=as_reg_date]);

## There's a much larger variance in the Draft namespace, which one would expect
## since the raw number of creations is very different.
## Overall, we find:
## Main:
## * Median: 4.2%
## * IQR: 2.6-6.7%
## Draft:
## * Median: 5.7%
## * IQR: 3.3-8.9%
##
## This suggests that pre-ACTRIAL, users who created a Draft during those 2.5
## years were more likely to survive.

## Q2: Is there a significant difference in the survival rate between those who
##     create articles and those who create drafts?

## Create a 2x2 contingency matrix, split by Main and Draft creators and
## survivors/non-survivors.
draft_main_survivor_mtx = matrix(
  c(length(page_creations[as_reg_date >= '2015-01-01' & as_reg_date < '2017-07-01'
                          & event_page_namespace == 0
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 == 0]$event_user_id),
    length(page_creations[as_reg_date >= '2015-01-01' & as_reg_date < '2017-07-01'
                          & event_page_namespace == 0
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 > 0]$event_user_id),
    length(page_creations[as_reg_date >= '2015-01-01' & as_reg_date < '2017-07-01'
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 == 0]$event_user_id),
    length(page_creations[as_reg_date >= '2015-01-01' & as_reg_date < '2017-07-01'
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 > 0]$event_user_id)
  ), ncol=2, byrow=2
);
rownames(draft_main_survivor_mtx) = c('Main', 'Draft');
colnames(draft_main_survivor_mtx) = c('Non-survivor', 'Survivor')

## Proportions
100*draft_main_survivor_mtx/rowSums(draft_main_survivor_mtx);

## 3.9% for Main vs 5% for Draft, that should be significant:
chisq.test(draft_main_survivor_mtx[1,], p=c(0.95, 0.05), correct=FALSE);
chisq.test(draft_main_survivor_mtx[2,], p=c(0.961, 0.039), correct=FALSE);
## (Totally is: X^2=667.09, df=1, p ~ 0)

## Q3: Has the survival rate changed with ACTRIAL?

## 1: Yes, for main it obviously has because those who create articles there in
## their first week have reached autoconfirmed status, and that greatly boosts
## survival rate.

## 2: How about draft?
## We compare the period [2017-09-15,2017-11-01] to the same periods in the two
## preceeding years
draft_survivor_mtx = matrix(
  c(length(page_creations[((as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-01')
                          | (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-01'))
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 == 0]$event_user_id),
    length(page_creations[((as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-01')
                           | (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-01'))
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 > 0]$event_user_id),
    length(page_creations[as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-01'
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 == 0]$event_user_id),
    length(page_creations[as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-01'
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 > 0]$event_user_id)
  ), ncol=2, byrow=2
);
rownames(draft_survivor_mtx) = c('Pre-ACTRIAL', 'ACTRIAL');
colnames(draft_survivor_mtx) = c('Non-survivor', 'Survivor')

## Proportions
100*draft_survivor_mtx/rowSums(draft_survivor_mtx);

## 5.24% vs 4.57%, significant?
chisq.test(draft_survivor_mtx[1,], p=c(0.9543068, 1-0.9543068), correct=FALSE);
chisq.test(draft_survivor_mtx[2,], p=c(0.9475395, 1-0.9475395), correct=FALSE);

## Yes, that's significant:
## X^2=4.525, df=1, p = 0.033

## What does the plot for 2017 look like, with a LOESS smoother? Turns out,
## I'm mainly interested in Draft space
ggplot(prop_surviving_creators[as_reg_date >= '2017-01-01'
                               & event_page_namespace == 118],
       aes(x=as_reg_date, y=100*n_survivers/n_creators)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of Draft creators editing in week 5") +
  geom_vline(xintercept=as.Date('2017-09-15'), linetype='dashed') +
  geom_smooth(method='loess') +
  ylim(0, 25);

## We have a significant difference in survival in Draft during ACTRIAL. This can
## arguably be a result of the self-selection happening pre-ACTRIAL. To test this,
## we want to sample creators from the Main namespace in the same time periods
## in 2015 and 2016, to understand what the expected survival rate would be if
## users came in and left at the same rate as Main. This is because we suspect some
## of those users are unlikely to return regardless of what hapens to them on
## Wikipedia.

## The time period we used is 47 days. The difference in number of accounts
## pre-ACTRIAL to ACTRIAL is:
rowSums(draft_survivor_mtx)[2] - rowSums(draft_survivor_mtx)[1]
## 5978

## We're sampling across two years of data, so we get 94 days, or a total of
5978/94;
## 63.59 users per day. I'll round that up and get
64*94;
## 6016

main_creator_sample = page_creations[
  ((as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-01')
  | (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-01'))
               & event_page_namespace == 0
               & as_num_edits_week_1 > 0,
               .SD[sample(.N, 64)], by=as_reg_date]

## What's the survival rate in this sample?
length(main_creator_sample[as_num_edits_week_5 > 0]$event_user_id)/
  length(main_creator_sample$event_user_id);

## Then we combine that sample with the existing Draft creators:
draft_survivor_mtx.2 = matrix(
  c(length(page_creations[((as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-01')
                           | (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-01'))
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 == 0]$event_user_id) +
      length(main_creator_sample[as_num_edits_week_5 == 0]$event_user_id),
    length(page_creations[((as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-01')
                           | (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-01'))
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 > 0]$event_user_id) +
      length(main_creator_sample[as_num_edits_week_5 > 0]$event_user_id), 
    length(page_creations[as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-01'
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 == 0]$event_user_id),
    length(page_creations[as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-01'
                          & event_page_namespace == 118
                          & as_num_edits_week_1 > 0 & as_num_edits_week_5 > 0]$event_user_id)
  ), ncol=2, byrow=2
);
rownames(draft_survivor_mtx.2) = c('Pre-ACTRIAL', 'ACTRIAL');
colnames(draft_survivor_mtx.2) = c('Non-survivor', 'Survivor')

## Proportions
100*draft_survivor_mtx.2/rowSums(draft_survivor_mtx.2);

## The proportions are much, much closer. Goodness-of-fit test:
chisq.test(draft_survivor_mtx[2,],
           p=c(as.matrix(draft_survivor_mtx.2/rowSums(draft_survivor_mtx.2))[1,1],
               1-as.matrix(draft_survivor_mtx.2/rowSums(draft_survivor_mtx.2))[1,1]),
           correct=FALSE);


