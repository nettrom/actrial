## Analysis of H5's releated measure when it comes to the survival of
## users who did not create an article, nor a page in the Draft namespace.

library(data.table);
library(ggplot2);

## This analysis assumes that non_autopatrolled_creations_main and draft_creations,
## both found in creation_datasets.R are loaded into memory.

## Read the useractivity data back into memory:
useractivity = fread("bunzip2 -c datasets/activity_stats_20180207.tsv.bz2",
                     na.strings = c('NULL'));

## When we're looking at data by the type of account creation, we are
## mainly interested in whether the account was autocreated or not. Thus, merge
## the types into "autocreate" and "create".
useractivity[as_create_type %in% c('create2', 'byemail'), as_create_type := 'create'];

## Extract registration date from the registration timestamp
useractivity[
  , c("as_reg_date", "as_reg_time") := IDateTime(as.POSIXct(as_reg_timestamp,
                                                            tz='UTC'))];

## Timestamps are off for accounts created as part of the SUL coercion.
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

## We restricted H5r to users who created a draft or article in the first week.
## Should we restrict this to users who edited in their first week, and did not
## create an article or draft within the first five weeks? We know that accounts
## that create an article/draft do so typically within the first day. To what extent
## do we expect the results to be affeced by users who create something after the
## first week, if we do not exclude them from the analysis?
## We could just do both.

## Let's add columns to useractivty identifying if the user created an article/draft
## within the first week, or within the first five weeks.
useractivity[, c('as_creator_week_1', 'as_creator_week_5',
                 'as_ns_creator_week_1', 'as_ns_creator_week_5') := list(FALSE, FALSE, -1, -1)];

useractivity[as_userid %in% non_autopatrolled_creations_main[
  event_user_age < 60*60*24*7]$event_user_id,
  c('as_creator_week_1', 'as_ns_creator_week_1') := list(TRUE, 0)];
useractivity[as_userid %in% draft_creations[
  event_user_age < 60*60*24*7]$event_user_id,
  as_creator_week_1 := TRUE];
useractivity[as_userid %in% draft_creations[
  event_user_age < 60*60*24*7]$event_user_id,
  as_ns_creator_week_1 := as_ns_creator_week_1 + 118];
useractivity[as_userid %in% non_autopatrolled_creations_main[
  event_user_age < 60*60*24*35]$event_user_id,
  c('as_creator_week_5', 'as_ns_creator_week_5') := list(TRUE, 0)];
useractivity[as_userid %in% draft_creations[
  event_user_age < 60*60*24*35]$event_user_id,
  as_creator_week_5 := TRUE];
useractivity[as_userid %in% draft_creations[
  event_user_age < 60*60*24*35]$event_user_id,
  as_ns_creator_week_5 := as_ns_creator_week_5 + 118];

## About 1.5% appear to have created both an article and a draft in the first week,
## and 1.3% appear to have done so within the first five weeks, by the way.

## Limiting it to not having created an article/draft in the first week:
prop_surviving_noncreators_week1 = merge(
  useractivity[as_num_edits_week_1 > 0 &
                 as_creator_week_1 == FALSE,
               list(n_noncreators=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_week_1 > 0 &
                 as_num_edits_week_5 > 0 &
                 as_creator_week_1 == FALSE,
               list(n_noncreator_survivors=sum(.N)),
                 by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));
prop_surviving_noncreators_week1[
  , prop_survivors := 100*n_noncreator_survivors/n_noncreators];
                                 
## Plot over time:
ggplot(prop_surviving_noncreators_week1[as_reg_date < '2017-11-15'],
       aes(x=as_reg_date, y=prop_survivors)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,25), breaks = 5*c(0:5)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of non-creators in week 1 surviving in week 5") +
  facet_grid(as_create_type ~ .);

## Focus on the last two years:
ggplot(prop_surviving_noncreators_week1[
  as_reg_date >= '2016-01-01' & as_reg_date < '2017-11-15'],
       aes(x=as_reg_date, y=prop_survivors)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,15), breaks = 5*c(0:3)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of non-creators in week 1 surviving in week 5, 2016-2017") +
  facet_grid(as_create_type ~ .);

## Limiting it to not having created an article/draft in the first five weeks:
prop_surviving_noncreators_week5 = merge(
  useractivity[as_num_edits_week_1 > 0 &
                 as_creator_week_5 == FALSE,
               list(n_noncreators=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_week_1 > 0 &
                 as_num_edits_week_5 > 0 &
                 as_creator_week_5 == FALSE,
               list(n_noncreator_survivors=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));
prop_surviving_noncreators_week5[
  , prop_survivors := 100*n_noncreator_survivors/n_noncreators];

## Plot over time:
ggplot(prop_surviving_noncreators_week5[as_reg_date < '2017-11-15'],
       aes(x=as_reg_date, y=prop_survivors)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,25), breaks = 5*c(0:5)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of non-creators in weeks 1-5 surviving in week 5") +
  facet_grid(as_create_type ~ .);

## Focus on the last two years:
ggplot(prop_surviving_noncreators_week5[
  as_reg_date >= '2016-01-01' & as_reg_date < '2017-11-15'],
  aes(x=as_reg_date, y=prop_survivors)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,15), breaks = 5*c(0:3)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of non-creators in weeks 1-5 surviving in week 5, 2016-2017") +
  facet_grid(as_create_type ~ .);

## Let's look at the first two months of ACTRIAL as a whole, compared to
## previous five years. First looking at non-creations during the first week.
summary(prop_surviving_noncreators_week1[
  as_create_type == 'autocreate' &
  ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
    (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
    (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors);
summary(prop_surviving_noncreators_week1[
  as_create_type == 'autocreate' &
  as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors);

summary(prop_surviving_noncreators_week1[
  as_create_type == 'create' &
    ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
       (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
       (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
       (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors);
summary(prop_surviving_noncreators_week1[
  as_create_type == 'create' &
    ((as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors);
summary(prop_surviving_noncreators_week1[
  as_create_type == 'create' &
    as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15']$prop_survivors);
summary(prop_surviving_noncreators_week1[
  as_create_type == 'create' &
    as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors);

qplot(
  prop_surviving_noncreators_week1[
    as_create_type == 'autocreate' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  geom = 'histogram'
);
qplot(
  prop_surviving_noncreators_week1[
    as_create_type == 'autocreate' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors,
  geom = 'histogram'
);

qplot(
  prop_surviving_noncreators_week1[
    as_create_type == 'create' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  geom = 'histogram'
);
qplot(
  prop_surviving_noncreators_week1[
    as_create_type == 'create' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors,
  geom = 'histogram'
);

## Both plots for autocreated accounts are skewed, suggesting I should use
## Mann-Whitney for those, or log-transform. For non-autocreated accounts,
## the distributions are fairly Normal-looking.

t.test(
  log2(
    prop_surviving_noncreators_week1[
      as_create_type == 'autocreate' &
        ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
           (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
           (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
           (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
           (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors
  ),
  log2(
    prop_surviving_noncreators_week1[
      as_create_type == 'autocreate' &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors
  )
);
wilcox.test(
  prop_surviving_noncreators_week1[
    as_create_type == 'autocreate' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  prop_surviving_noncreators_week1[
    as_create_type == 'autocreate' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors
);

t.test(
    prop_surviving_noncreators_week1[
      as_create_type == 'create' &
        ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
           (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
           (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
           (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
           (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
    prop_surviving_noncreators_week1[
      as_create_type == 'create' &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors
);
wilcox.test(
  prop_surviving_noncreators_week1[
    as_create_type == 'create' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  prop_surviving_noncreators_week1[
    as_create_type == 'create' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors
);

## Now let's look at the same for those who do not create an article/draft
## in weeks 1-5.
summary(prop_surviving_noncreators_week5[
  as_create_type == 'autocreate' &
    ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
       (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
       (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
       (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors);
summary(prop_surviving_noncreators_week5[
  as_create_type == 'autocreate' &
    as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors);

summary(prop_surviving_noncreators_week5[
  as_create_type == 'create' &
    ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
       (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
       (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
       (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors);
summary(prop_surviving_noncreators_week5[
  as_create_type == 'create' &
    ((as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors);
summary(prop_surviving_noncreators_week5[
  as_create_type == 'create' &
    as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15']$prop_survivors);
summary(prop_surviving_noncreators_week5[
  as_create_type == 'create' &
    as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors);

qplot(
  prop_surviving_noncreators_week5[
    as_create_type == 'autocreate' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  geom = 'histogram'
);
qplot(
  prop_surviving_noncreators_week5[
    as_create_type == 'autocreate' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors,
  geom = 'histogram'
);

qplot(
  prop_surviving_noncreators_week5[
    as_create_type == 'create' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  geom = 'histogram'
);
qplot(
  prop_surviving_noncreators_week5[
    as_create_type == 'create' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors,
  geom = 'histogram'
);

## Both plots for autocreated accounts are skewed, suggesting I should use
## Mann-Whitney for those, or log-transform. For non-autocreated accounts,
## the distributions are fairly Normal-looking.

t.test(
  log2(
    prop_surviving_noncreators_week5[
      as_create_type == 'autocreate' &
        ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
           (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
           (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
           (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
           (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors
  ),
  log2(
    prop_surviving_noncreators_week5[
      as_create_type == 'autocreate' &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors
  )
);
wilcox.test(
  prop_surviving_noncreators_week5[
    as_create_type == 'autocreate' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  prop_surviving_noncreators_week5[
    as_create_type == 'autocreate' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors
);

t.test(
  prop_surviving_noncreators_week5[
    as_create_type == 'create' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  prop_surviving_noncreators_week5[
    as_create_type == 'create' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors
);
wilcox.test(
  prop_surviving_noncreators_week5[
    as_create_type == 'create' &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$prop_survivors,
  prop_surviving_noncreators_week5[
    as_create_type == 'create' &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$prop_survivors
);

## Need to make contigency matrices for autocreated and non-autocreated
## accounts, pre-ACTRIAL and during ACTRIAL.

## Autocreated accounts, no creations in the first week:
prop_surviving_noncreators_week1_ac_cm = matrix(
  c(length(useractivity[
    as_create_type == 'autocreate' &
    as_num_edits_week_1 > 0 & 
      as_num_edits_week_5 == 0 &
      as_creator_week_1 == FALSE &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_userid),
    length(useractivity[
      as_create_type == 'autocreate' &
      as_num_edits_week_1 > 0 &
        as_num_edits_week_5 > 0 &
        as_creator_week_1 == FALSE &
        ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
           (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
           (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
           (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
           (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_userid),
    length(useractivity[
      as_create_type == 'autocreate' &
      as_num_edits_week_1 > 0 &
        as_num_edits_week_5 == 0 &
        as_creator_week_1 == FALSE &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_userid),
    length(useractivity[
      as_create_type == 'autocreate' &
      as_num_edits_week_1 > 0 &
        as_num_edits_week_5 > 0 &
        as_creator_week_1 == FALSE &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_userid)
  ), byrow = TRUE, ncol = 2
);
colnames(prop_surviving_noncreators_week1_ac_cm) = c('non-surviving', 'surviving');
rownames(prop_surviving_noncreators_week1_ac_cm) = c('pre-ACTRIAL', 'ACTRIAL');

## Row sums and per-row proportions
rowSums(prop_surviving_noncreators_week1_ac_cm);
100*prop_surviving_noncreators_week1_ac_cm/rowSums(prop_surviving_noncreators_week1_ac_cm);

## Column sumns and per-column proportions
colSums(prop_surviving_noncreators_week1_ac_cm);
100*colSums(prop_surviving_noncreators_week1_ac_cm)/sum(prop_surviving_noncreators_week1_ac_cm);

## Total sum:
sum(prop_surviving_noncreators_week1_ac_cm);

## Non-autocreated accounts, no creations in the first week:
prop_surviving_noncreators_week1_nac_cm = matrix(
  c(length(useractivity[
    as_create_type == 'create' &
      as_num_edits_week_1 > 0 & 
      as_num_edits_week_5 == 0 &
      as_creator_week_1 == FALSE &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_userid),
    length(useractivity[
      as_create_type == 'create' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 > 0 &
        as_creator_week_1 == FALSE &
        ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
           (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
           (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
           (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
           (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_userid),
    length(useractivity[
      as_create_type == 'create' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 == 0 &
        as_creator_week_1 == FALSE &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_userid),
    length(useractivity[
      as_create_type == 'create' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 > 0 &
        as_creator_week_1 == FALSE &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_userid)
  ), byrow = TRUE, ncol = 2
);
colnames(prop_surviving_noncreators_week1_nac_cm) = c('non-surviving', 'surviving');
rownames(prop_surviving_noncreators_week1_nac_cm) = c('pre-ACTRIAL', 'ACTRIAL');

## Overview:
prop_surviving_noncreators_week1_nac_cm;

## Row sums and per-row proportions
rowSums(prop_surviving_noncreators_week1_nac_cm);
100*prop_surviving_noncreators_week1_nac_cm/rowSums(prop_surviving_noncreators_week1_nac_cm);

## Column sumns and per-column proportions
colSums(prop_surviving_noncreators_week1_nac_cm);
100*colSums(prop_surviving_noncreators_week1_nac_cm)/sum(prop_surviving_noncreators_week1_nac_cm);

## Total sum:
sum(prop_surviving_noncreators_week1_nac_cm);

## Autocreated accounts, no creations in the first five weeks:
prop_surviving_noncreators_week5_ac_cm = matrix(
  c(length(useractivity[
    as_create_type == 'autocreate' &
      as_num_edits_week_1 > 0 & 
      as_num_edits_week_5 == 0 &
      as_creator_week_5 == FALSE &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_userid),
    length(useractivity[
      as_create_type == 'autocreate' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 > 0 &
        as_creator_week_5 == FALSE &
        ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
           (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
           (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
           (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
           (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_userid),
    length(useractivity[
      as_create_type == 'autocreate' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 == 0 &
        as_creator_week_5 == FALSE &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_userid),
    length(useractivity[
      as_create_type == 'autocreate' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 > 0 &
        as_creator_week_5 == FALSE &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_userid)
  ), byrow = TRUE, ncol = 2
);
colnames(prop_surviving_noncreators_week5_ac_cm) = c('non-surviving', 'surviving');
rownames(prop_surviving_noncreators_week5_ac_cm) = c('pre-ACTRIAL', 'ACTRIAL');

## Overview:
prop_surviving_noncreators_week5_ac_cm;

## Row sums and per-row proportions
rowSums(prop_surviving_noncreators_week5_ac_cm);
100*prop_surviving_noncreators_week5_ac_cm/rowSums(prop_surviving_noncreators_week5_ac_cm);

## Column sumns and per-column proportions
colSums(prop_surviving_noncreators_week5_ac_cm);
100*colSums(prop_surviving_noncreators_week5_ac_cm)/sum(prop_surviving_noncreators_week5_ac_cm);

## Total sum:
sum(prop_surviving_noncreators_week5_ac_cm);

## Non-autocreated accounts, no creations in the first five weeks:
prop_surviving_noncreators_week5_nac_cm = matrix(
  c(length(useractivity[
    as_create_type == 'create' &
      as_num_edits_week_1 > 0 & 
      as_num_edits_week_5 == 0 &
      as_creator_week_5 == FALSE &
      ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
         (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
         (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_userid),
    length(useractivity[
      as_create_type == 'create' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 > 0 &
        as_creator_week_5 == FALSE &
        ((as_reg_date >= '2012-09-15' & as_reg_date < '2012-11-15') |
           (as_reg_date >= '2013-09-15' & as_reg_date < '2013-11-15') |
           (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
           (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
           (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_userid),
    length(useractivity[
      as_create_type == 'create' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 == 0 &
        as_creator_week_5 == FALSE &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_userid),
    length(useractivity[
      as_create_type == 'create' &
        as_num_edits_week_1 > 0 &
        as_num_edits_week_5 > 0 &
        as_creator_week_5 == FALSE &
        as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_userid)
  ), byrow = TRUE, ncol = 2
);
colnames(prop_surviving_noncreators_week5_nac_cm) = c('non-surviving', 'surviving');
rownames(prop_surviving_noncreators_week5_nac_cm) = c('pre-ACTRIAL', 'ACTRIAL');

## Overview:
prop_surviving_noncreators_week5_nac_cm;

## Row sums and per-row proportions
rowSums(prop_surviving_noncreators_week5_nac_cm);
100*prop_surviving_noncreators_week5_nac_cm/rowSums(prop_surviving_noncreators_week5_nac_cm);

## Column sumns and per-column proportions
colSums(prop_surviving_noncreators_week5_nac_cm);
100*colSums(prop_surviving_noncreators_week5_nac_cm)/sum(prop_surviving_noncreators_week5_nac_cm);

## Total sum:
sum(prop_surviving_noncreators_week5_nac_cm);

## Goodness-of-fit tests for each matrix:
chisq.test(prop_surviving_noncreators_week1_ac_cm[2,],
           p=c(as.matrix(prop_surviving_noncreators_week1_ac_cm/rowSums(prop_surviving_noncreators_week1_ac_cm))[1,1],
               1-as.matrix(prop_surviving_noncreators_week1_ac_cm/rowSums(prop_surviving_noncreators_week1_ac_cm))[1,1]),
           correct=FALSE);
chisq.test(prop_surviving_noncreators_week1_nac_cm[2,],
           p=c(as.matrix(prop_surviving_noncreators_week1_nac_cm/rowSums(prop_surviving_noncreators_week1_nac_cm))[1,1],
               1-as.matrix(prop_surviving_noncreators_week1_nac_cm/rowSums(prop_surviving_noncreators_week1_nac_cm))[1,1]),
           correct=FALSE);

chisq.test(prop_surviving_noncreators_week5_ac_cm[2,],
           p=c(as.matrix(prop_surviving_noncreators_week5_ac_cm/rowSums(prop_surviving_noncreators_week5_ac_cm))[1,1],
               1-as.matrix(prop_surviving_noncreators_week5_ac_cm/rowSums(prop_surviving_noncreators_week5_ac_cm))[1,1]),
           correct=FALSE);
chisq.test(prop_surviving_noncreators_week5_nac_cm[2,],
           p=c(as.matrix(prop_surviving_noncreators_week5_nac_cm/rowSums(prop_surviving_noncreators_week5_nac_cm))[1,1],
               1-as.matrix(prop_surviving_noncreators_week5_nac_cm/rowSums(prop_surviving_noncreators_week5_nac_cm))[1,1]),
           correct=FALSE);
