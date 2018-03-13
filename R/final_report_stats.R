## Various statistics for the final report.

library(ggplot2);
library(lubridate);

## 1: Avg number of registrations Sept-Nov:
mean(newaccounts_by_month[type == 'create' &
                            na_month %in% c('201709', '201710', '201711'),
                          list(num_regs=sum(n_accounts)), by=na_month]$num_regs);
mean(newaccounts_by_month[type == 'create' &
                            na_month %in% c('201609', '201610', '201611'),
                          list(num_regs=sum(n_accounts)), by=na_month]$num_regs);
100*(153662.7-150989.7)/153662.7;

## Reduction in the NPP backlog
(14353-12371)/14353;

## Graph of retention by month, non-autocreated accounts:
ggplot(prop_survived_week_5_monthly[type == 'create' &
                                      reg_date < '2017-12-01'],
       aes(x=reg_date, y=prop_surviving)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha = 0.75) +
  geom_line() +
  ggtitle('Proportion of surviving editors in week 5') +
  xlab('Registration month') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  geom_smooth(method='loess', span=0.2);

## Graph of number of non-autocreated accounts created per month
ggplot(newaccounts_by_month[type == 'create'],
       aes(x=na_date, y=n_accounts)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha = 0.75) +
  geom_line() +
  xlab('Year') + ylab('Number of accounts') +
  ggtitle('Accounts registered per month') +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 3.5e+05),
                     breaks=c(0,1e+05,2e+05, 3e+05),
                     labels=c("0", "100k", "200k", "300k"));

## Average number of non-autoconfirmed main namespace creations per day
## prior to ACTRIAL.
summary(all_creations_main[
  ((creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
     (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
     (creation_date >= '2016-09-15' & creation_date < '2016-11-15')) &
    event_user_revision_count < 10 & event_user_age < 60*60*24*4,
  list(num_creations=sum(.N)), by=creation_date]$num_creations);

summary(all_creations_main[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15') &
    event_user_revision_count < 10 & event_user_age < 60*60*24*4,
  list(num_creations=sum(.N)), by=creation_date]$num_creations);

## Average numer of Draft creations prior to and during ACTRIAL.
summary(draft_creations[
  (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15'),
  list(num_creations=sum(.N)), by=creation_date]$num_creations);

summary(draft_creations[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15'),
  list(num_creations=sum(.N)), by=creation_date]$num_creations);

main_draft_creations = rbind(
  all_creations_main[
    creation_date >= '2014-07-01' & creation_date < '2017-09-15' &
      event_user_revision_count < 10 & event_user_age < 60*60*24*4,
    list(num_creations=sum(.N), namespace='Main'), by=creation_date],
  draft_creations[
    creation_date >= '2014-07-01' & creation_date < '2017-12-01',
    list(num_creations=sum(.N), namespace='Draft'), by=creation_date]
);
main_draft_creations[, creation_month := strftime(creation_date, "%Y%m")];

main_draft_creations_monthly = main_draft_creations[
  , list(num_creations=sum(num_creations)), by=c('creation_month', 'namespace')];

main_draft_creations_monthly[
  , creation_date := as.Date(strptime(paste0(creation_month, "01"),
                                         format="%Y%m%d", tz='UTC'))];

## Graph of non-autoconfirmed creations per day and number of Draft creations
## per day from July 1, 2014 onwards:
ggplot(main_draft_creations_monthly,
       aes(x=creation_date, y=num_creations, group=namespace, colour=namespace)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha = 0.75) +
  geom_line() +
  scale_x_date("Date", date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous('Number of creations', limits = c(0,10000)) +
  scale_colour_manual(values=cbbPalette, breaks=c('Main', 'Draft')) +
  ggtitle('Non-autoconfirmed article creations and all Draft creations');


