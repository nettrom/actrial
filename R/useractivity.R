## (Preliminary) analysis of user activity

library(data.table);
library(ggplot2);

## Colourblind-friendly palette with black, from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## FIXME: remember to only extract data up to 2017-07-01 as we don't have
## 30-day activity data for users after that.

## Read in the activity dataset
useractivity = data.table(read.table("datasets/activity_stats_20170831.tsv.bz2",
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

## Proportion of users by date and type that have > 0 edits in the first 30 days
prop_nonzero_30 = merge(useractivity[, list(n_registrations=sum(.N)),
                                     by=list(as_reg_date, as_create_type)],
                        useractivity[as_num_edits_30 > 0, list(n_editors=sum(.N)),
                                     by=list(as_reg_date, as_create_type)],
                        by=c('as_reg_date', 'as_create_type'));

## Make me plots:
ggplot(prop_nonzero_30[, list(prop_nonzero = sum(n_editors)/sum(n_registrations)),
                       by=as_reg_date],
       aes(x=as_reg_date, y=100*prop_nonzero)) + geom_line() +
  ggtitle('Proportion of accounts with non-zero edits') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  xlab('Year') +
  ylab('Proportion in %');

ggplot(prop_nonzero_30[, list(prop_nonzero = 100*sum(n_editors)/sum(n_registrations)),
                       by=list(as_reg_date, type=as_create_type)],
       aes(x=as_reg_date, y=prop_nonzero,
           group=type, colour=type)) + geom_line() +
  ggtitle('Proportion of accounts with non-zero edits by type') +
  xlab('Year') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate'));

ggplot(prop_nonzero_30[as_create_type == 'autocreate',
                       list(prop_nonzero = sum(n_editors)/sum(n_registrations)),
                       by=list(as_reg_date)],
       aes(x=as_reg_date, y=prop_nonzero*100)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12), breaks=seq(0,12,2)) +
  xlab('Year') +
  ylab('Proportion in %') +
  ggtitle('Proportion of autocreated accounts with non-zero edits');

## Based on the trends in number of autocreated accounts and the proportion
## of accounts making edits, is the number of accounts making edits fairly stable?
ggplot(prop_nonzero_30[as_create_type == 'autocreate'],
       aes(x=as_reg_date, y=n_editors)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 500)) +
  xlab('Year') +
  ylab('Number of editors') +
  ggtitle('Autocreated accounts making edits in the first 30 days') +
  geom_smooth(method='loess', span=0.25);

## Proportion of users by date and type that reach autoconfirmed status in 30 days,
## also measuring number of editors because that's perhaps more interesting.
prop_autoconfirmed_30 = merge(merge(
  useractivity[, list(n_registrations=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 > 0, list(n_editors=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')),
  useractivity[as_num_edits_30 >= 10, list(n_autoconfirmed=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));

ggplot(prop_autoconfirmed_30[
  , list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_registrations)),
  by=as_reg_date], aes(x=as_reg_date, y=prop_autoconfirmed)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5), breaks=seq(0, 5, 1)) +
  
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts reaching autoconfirmed status in 30 days");

ggplot(prop_autoconfirmed_30[
  , list(n_autoconfirmed = sum(n_autoconfirmed)),
  by=as_reg_date], aes(x=as_reg_date, y=n_autoconfirmed)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 350), breaks=seq(0, 350, 50)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Number of accounts reaching autoconfirmed status in 30 days");

ggplot(prop_autoconfirmed_30[
  , list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_editors)),
  by=as_reg_date], aes(x=as_reg_date, y=prop_autoconfirmed)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(5, 16), breaks=seq(5, 15, 5)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts with non-zero edits autoconfirmed in 30 days");

ggplot(prop_autoconfirmed_30[
  as_create_type %in% c('create', 'autocreate'),
  list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_editors)),
  by=list(as_reg_date, type=as_create_type)],
  aes(x=as_reg_date, y=prop_autoconfirmed,
      group=type, colour=type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks=seq(0, 50, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts with non-zero edits autoconfirmed in 30 days");

## Average number of edits in the first 30 days (both with and without) 0-edits.
avg_edits_30_by_date = merge(
  useractivity[, list(avg_edits_0=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_edits_1=geo.mean(as_num_edits_30)),
               by=list(as_reg_date)],
  by='as_reg_date'
);

avg_edits_30_by_type = merge(
  useractivity[, list(avg_edits_0=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_edits_1=geo.mean(as_num_edits_30)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')
);

## The first plot looks very much like the proportion of accounts making at
## least one edit, just with a different Y-axis. The second plot looks a lot
## like the proportion of accounts making at least one edit that reaches
## autoconfirmed status, except with a different Y-axis.
ggplot(avg_edits_30_by_date, aes(x=as_reg_date, y=avg_edits_0)) + geom_line();
ggplot(avg_edits_30_by_date, aes(x=as_reg_date, y=avg_edits_1)) + geom_line();

## Same for these two, they look similar to some of the other plots.
ggplot(avg_edits_30_by_type[as_create_type %in% c('create', 'autocreate')],
       aes(x=as_reg_date, y=avg_edits_0,
           group=as_create_type, colour=as_create_type)) +
  geom_line();

ggplot(avg_edits_30_by_type[as_create_type %in% c('create', 'autocreate')],
       aes(x=as_reg_date, y=avg_edits_1,
           group=as_create_type, colour=as_create_type)) +
  geom_line();

## Median time to reach autoconfirmed status
autoconfirmed_30_delta = useractivity[!is.na(as_autoconfirmed_30_timestamp),
                                      list(median_delta=median(as_ac_30_delta)),
                                      by=as_reg_date];

autoconfirmed_30_delta_by_type = useractivity[!is.na(as_autoconfirmed_30_timestamp),
                                      list(median_delta=median(as_ac_30_delta)),
                                      by=list(as_reg_date, as_create_type)];

ggplot(autoconfirmed_30_delta, aes(x=as_reg_date, y=median_delta)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks=seq(0, 8, 1)) +
  xlab('Year') + ylab('Time in days') +
  ggtitle("Median time to autoconfirmed status in 30 days");

ggplot(autoconfirmed_30_delta_by_type, aes(x=as_reg_date, y=median_delta,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), breaks=seq(0, 30, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Time in days') +
  ggtitle("Median time to autoconfirmed status in 30 days");

## These look different, are they consistently different? We could just check
## for each month, or something…
t.test(as.numeric(useractivity[as_reg_date >= as.Date('2017-06-01')
                    & as_reg_date < as.Date('2017-07-01')
                    & as_create_type == 'create']$as_ac_30_delta),
       as.numeric(useractivity[as_reg_date >= as.Date('2017-06-01')
                    & as_reg_date < as.Date('2017-07-01')
                    & as_create_type == 'autocreate']$as_ac_30_delta));

## Proportion of new accounts that survive (>= 1 edits in both week 1 & 5)
prop_survived_week_5 = merge(useractivity[, list(n_registrations=sum(.N)),
                                     by=list(as_reg_date, as_create_type)],
                        useractivity[as_num_edits_week_1 > 0
                                     & as_num_edits_week_5 > 0,
                                     list(n_editors=sum(.N)),
                                     by=list(as_reg_date, as_create_type)],
                        by=c('as_reg_date', 'as_create_type'));
prop_survived_week_5 = merge(
  prop_survived_week_5,
  useractivity[as_num_edits_week_1 > 0, list(n_nonzero=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));

ggplot(prop_survived_week_5[, list(prop_survived = 100*sum(n_editors)/sum(n_registrations)),
                       by=as_reg_date],
       aes(x=as_reg_date, y=prop_survived)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2), breaks=seq(0, 2, 0.5)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of surviving editors in week 5");

## This plot deliberately looks in favour of the non-autocreated accounts,
## but we know that a much lower proportion of these made any edits.
ggplot(prop_survived_week_5[, list(prop_survived = 100*sum(n_editors)/sum(n_registrations)),
                       by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_survived,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.75), breaks=seq(0, 2.5, 0.5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of surviving editors in week 5");

## So, let's fix that by only looking at proportion of surviving users based on
## making at least one edit in the first week.
ggplot(prop_survived_week_5[, list(prop_survived = 100*sum(n_editors)/sum(n_nonzero)),
                            by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_survived,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0, 20, 5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of surviving editors in week 5");

## What's the proportion of newly registered accounts that make edits in the
## first 30 days, but don't make it to autoconfirmed status?
edited_not_autoconfirmed = merge(
  useractivity[as_num_edits_30 > 0, list(n_edited=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 10, list(n_autoconfirmed=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));

ggplot(edited_not_autoconfirmed[, list(prop_not_autoconfirmed = 1 - (sum(n_autoconfirmed)/sum(n_edited))),
                                by=list(as_reg_date)], aes(x=as_reg_date, y=prop_not_autoconfirmed)) +
         geom_line();
ggplot(edited_not_autoconfirmed[, list(prop_not_autoconfirmed = 1 - (sum(n_autoconfirmed)/sum(n_edited))),
                                by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_not_autoconfirmed,
           group=as_create_type, colour=as_create_type)) +
  geom_line();

## Let's do 2016 onwards
ggplot(edited_not_autoconfirmed[as_reg_date >= as.Date('2016-01-01'),
                                list(prop_not_autoconfirmed = 1 - (sum(n_autoconfirmed)/sum(n_edited))),
                                by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_not_autoconfirmed,
           group=as_create_type, colour=as_create_type)) +
  geom_line() + geom_smooth();

## H6: Diversity of participation
num_namespaces_30 = merge(
  useractivity[, list(avg_namespaces_0=mean(as_num_namespaces_30)),
               by=list(as_reg_date)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_namespaces_nonzero=mean(as_num_namespaces_30)),
               by=list(as_reg_date)],
  by=c('as_reg_date')
);
num_namespaces_30_by_type = merge(
  useractivity[, list(avg_namespaces_0=mean(as_num_namespaces_30)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_namespaces_nonzero=mean(as_num_namespaces_30)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')
);

num_pages_30 = merge(
  useractivity[, list(avg_pages_0=geo.mean.plus.one(as_num_pages_30)),
               by=list(as_reg_date)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_pages_nonzero=geo.mean.plus.one(as_num_pages_30)),
               by=list(as_reg_date)],
  by=c('as_reg_date')
);
num_pages_30_by_type = merge(
  useractivity[, list(avg_pages_0=geo.mean.plus.one(as_num_pages_30)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_pages_nonzero=geo.mean.plus.one(as_num_pages_30)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')
);

ggplot(num_namespaces_30, aes(x=as_reg_date, y=avg_namespaces_0)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days");

ggplot(num_namespaces_30, aes(x=as_reg_date, y=avg_namespaces_nonzero)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days");

ggplot(num_pages_30, aes(x=as_reg_date, y=avg_pages_0)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days");

ggplot(num_pages_30, aes(x=as_reg_date, y=avg_pages_nonzero)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days");

## Split by type of account creation
ggplot(num_namespaces_30_by_type,
       aes(x=as_reg_date, y=avg_namespaces_0,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days");

ggplot(num_namespaces_30_by_type,
       aes(x=as_reg_date, y=avg_namespaces_nonzero,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days");

ggplot(num_pages_30_by_type,
       aes(x=as_reg_date, y=avg_pages_0,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days");

ggplot(num_pages_30_by_type,
       aes(x=as_reg_date, y=avg_pages_nonzero,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days");

## H7: Average number of edits
num_edits_30 = merge(
  useractivity[, list(avg_edits_0=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_edits_nonzero=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date)],
  by=c('as_reg_date')
);
num_edits_30_by_type = merge(
  useractivity[, list(avg_edits_0=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_edits_nonzero=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')
);

ggplot(num_edits_30,
       aes(x=as_reg_date, y=avg_edits_0)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of edits') +
  ggtitle("Average number of edits in first 30 days");

ggplot(num_edits_30,
       aes(x=as_reg_date, y=avg_edits_nonzero)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of edits') +
  ggtitle("Average number of edits in first 30 days");

ggplot(num_edits_30_by_type,
       aes(x=as_reg_date, y=avg_edits_0,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of edits in first 30 days");

ggplot(num_edits_30_by_type,
       aes(x=as_reg_date, y=avg_edits_nonzero,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of edits in first 30 days");
