## H14: survival rate of articles created by autoconfirmed users

library(data.table);
library(ggplot2);
library(forecast);

autoconfirmed_articles = data.table(read.table(
  "datasets/autoconfirmed_deletions.tsv.bz2", sep='\t',
  quote='', na.strings=c('NULL'), stringsAsFactors=FALSE,
  header=TRUE
));

## Turn creation and deletion timestamps into dates.
autoconfirmed_articles[, c("creation_date", "creation_time") := IDateTime(
  as.POSIXct(event_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];
autoconfirmed_articles[, c("deletion_date", "deletion_time") := IDateTime(
  as.POSIXct(event_deletion_time, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];

## Calculate time difference (in seconds) between creation and deletion time
autoconfirmed_articles[,
                       survival_time := as.POSIXct(event_deletion_time,
                                                   format='%Y-%m-%d %H:%M:%S',
                                                   tz='UTC') -
                         as.POSIXct(event_timestamp, format='%Y-%m-%d %H:%M:%S',
                                    tz='UTC')];

## Total number of created articles is 1,601,920, total number of deletions
## is:
length(autoconfirmed_articles[!is.na(deletion_date)]$event_timestamp);
## 108,237

## Overall survival rate is:
(length(autoconfirmed_articles$event_timestamp)-length(autoconfirmed_articles[!is.na(deletion_date)]$event_timestamp))/length(autoconfirmed_articles$event_timestamp);

## How many were created before Dec 1, 2017, and how many of those survived 30 days?
length(autoconfirmed_articles[creation_date < '2017-12-01']$event_timestamp);
length(autoconfirmed_articles[creation_date < '2017-12-01' &
                                !is.na(deletion_date) &
                                survival_time >= 60*60*24*30]$event_timestamp)

## Now we can do some arithmetic, check number of articles per day that survived
## 30, 60, and 90 days.

## For 30 days, limit it to before Nov 15, 2017 (end of second month of ACTRIAL).
## Combine the two into a single dataset so we can calculate the survival rate:
autoconfirmed_articles_30days = merge(
  autoconfirmed_articles[creation_date >= '2014-07-01' &
                           creation_date < '2017-11-15',
                       list(n_creations=sum(.N)), by=creation_date],
  autoconfirmed_articles[creation_date >= '2014-07-01' &
                           creation_date < '2017-11-15' &
                         (is.na(deletion_date) |
                         survival_time >= 60*60*24*30),
                       list(n_survivors=sum(.N)),
                       by=creation_date],
  by='creation_date');
autoconfirmed_articles_30days[, prop_survived := n_survivors/n_creations];

ggplot(autoconfirmed_articles_30days,
       aes(x=creation_date, y=100*prop_survived)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Article creation date') + ylab('Proportion in %') +
  ggtitle("Autoconfirmed article creation survival") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 100);

## Digging into the increasing trend of deletions, what about plotting
## median time to deletion. Did that go down as well?
autoconfirmed_median_survival_time_30days = autoconfirmed_articles[
  creation_date < '2017-12-01' &
    !is.na(deletion_date),
  list(min_survival_time=min(as.numeric(survival_time)),
       median_survival_time=median(as.numeric(survival_time))/(60*60*24)),
  by=creation_date];

ggplot(autoconfirmed_median_survival_time_30days,
       aes(x=creation_date, y=median_survival_time)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Median survival time (in days)') +
  ggtitle("Median survival time for autoconfirmed article creations") +
  geom_smooth(method='loess', span=0.2);

ggplot(autoconfirmed_median_survival_time_30days,
       aes(x=creation_date, y=min_survival_time)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Minimum survival time (in seconds)') +
  ggtitle("Minimum survival time for autoconfirmed article creations") +
  geom_smooth(method='loess', span=0.2);

## OK, the min deletion time reveals the problem, our data is not good
## before early June, 2014. I'll follow up to see if we can get better data.

## Let's look at the data from July 1, 2014 onwards
ggplot(autoconfirmed_articles_30days[creation_date >= '2014-07-01'],
       aes(x=creation_date, y=100*n_deleted/n_creations)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of autoconfirmed article creations deleted in 30 days") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 20);


## 60 days
autoconfirmed_articles_60days = merge(
  autoconfirmed_articles[creation_date < '2017-11-01',
                         list(n_creations=sum(.N)), by=creation_date],
  autoconfirmed_articles[creation_date < '2017-11-01' &
                           !is.na(deletion_date) &
                           survival_time >= 60*60*24*60,
                         list(n_deleted=sum(.N)),
                         by=creation_date],
  by='creation_date');
autoconfirmed_articles_60days[, prop_survived := (n_creations-n_deleted)/n_creations];

ggplot(autoconfirmed_articles_60days,
       aes(x=creation_date, y=100*n_deleted/n_creations)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of autoconfirmed article creations deleted in 60 days") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 20);

ggplot(autoconfirmed_articles_60days[creation_date >= '2016-01-01'],
       aes(x=creation_date, y=100*n_deleted/n_creations)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of autoconfirmed article creations deleted in 60 days") +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  geom_smooth(method='loess', span=0.2) + ylim(0, 20);

## 90 days
autoconfirmed_articles_90days = merge(
  autoconfirmed_articles[creation_date < '2017-10-01',
                         list(n_creations=sum(.N)), by=creation_date],
  autoconfirmed_articles[creation_date < '2017-10-01' &
                           !is.na(deletion_date) &
                           survival_time >= 60*60*24*90,
                         list(n_deleted=sum(.N)),
                         by=creation_date],
  by='creation_date');
autoconfirmed_articles_90days[, prop_survived := (n_creations-n_deleted)/n_creations];

ggplot(autoconfirmed_articles_90days,
       aes(x=creation_date, y=100*n_deleted/n_creations)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of autoconfirmed article creations deleted in 90 days") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 20);

ggplot(autoconfirmed_articles_90days[creation_date >= '2016-01-01'],
       aes(x=creation_date, y=100*n_deleted/n_creations)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of autoconfirmed article creations deleted in 90 days") +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  geom_smooth(method='loess', span=0.2) + ylim(0, 20);


## Can we do a logistic regression on survival based on number of edits and
## account age?
autoconfirmed_articles[, survived := 1];
autoconfirmed_articles[!is.na(deletion_date), survived := 0];

## Let's control for the age of Wikipedia (in days).
autoconfirmed_articles[, wp_age := creation_date - as.IDate('2001-01-15')]

autoconfirmed_survival_model = glm(
  formula = survived ~ wp_age + event_user_age + event_user_edit_count,
  family=binomial(link='logit'),
  data = autoconfirmed_articles
  );

autoconfirmed_articles[, user_age_days := event_user_age / (60*60*24)];
autoconfirmed_articles[
  , log_days_since_autoconfirmed := log2(1 + (event_user_age / (60*60*24) - 4))];
autoconfirmed_articles[is.na(log_days_since_autoconfirmed),
                       log_days_since_autoconfirmed := 0]
autoconfirmed_articles[, log_edit_count := log2(1 + event_user_edit_count)];

autoconfirmed_survival_model = glm(
  formula = survived ~ wp_age + user_age_days + log_edit_count,
  family=binomial(link='logit'),
  data = autoconfirmed_articles[creation_date >= '2014-07-01']
);

## Can we calculate survival rates for different user ages?
max(autoconfirmed_articles$log_days_since_autoconfirmed);

autoconfirmed_articles$bucket = cut(
  autoconfirmed_articles$log_days_since_autoconfirmed,
    breaks=c(0:ceiling(max(autoconfirmed_articles$log_days_since_autoconfirmed))),
    right=FALSE);

## Overall survival by bucket
autoconfirmed_survival_by_bucket = merge(
  autoconfirmed_articles[
    creation_date >= '2014-07-01' &
    creation_date < '2017-12-01',
    list(n_creations=sum(.N)), by=bucket],
  autoconfirmed_articles[
    creation_date >= '2014-07-01' &
      creation_date < '2017-12-01' &
      !is.na(deletion_date) &
      survival_time >= 60*60*24*30,
    list(n_deleted=sum(.N)),
    by=bucket],
  by='bucket');

autoconfirmed_survival_by_bucket[, prop := (n_creations - n_deleted)/n_creations];
autoconfirmed_survival_by_bucket;

## What's average/median/IQR time to deletion per bucket, for pages that
## get deleted?
autoconfirmed_articles_time_to_deletion = autoconfirmed_articles[
  creation_date >= '2014-07-01' &
  creation_date < '2017-12-01'
  & !is.na(deletion_date),
  list(avg_survival_time=mean(as.numeric(survival_time))/(60*60*24),
       median_survival_time=median(as.numeric(survival_time))/(60*60*24),
       q25_survival_time=quantile(as.numeric(survival_time), 0.25)/(60*60*24),
       q75_survival_time=quantile(as.numeric(survival_time), 0.75)/(60*60*24)),
  by=bucket][order(bucket)];

ggplot(autoconfirmed_articles[!is.na(deletion_date)],
       aes(x=bucket, y=as.numeric(survival_time)/(60*60*24))) +
  geom_boxplot() +
  scale_y_log10(
    expand = c(0,0),
    limits = c(2e-05, 1.25e+04),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + 
  annotation_logticks(sides='l') +
  theme(panel.grid.minor = element_blank()) +
  xlab("Days since autoconfirmed (log2-scale)") +
  ylab("Days to deletion") +
  ggtitle("Time to deletion by account age");
  
## The overall plots of survival don't really tell us much. How about we instead
## plot them by bucket? And plot the deletion rate instead?
autoconfirmed_survival_by_bucket_date = merge(
  autoconfirmed_articles[
    creation_date >= '2014-07-01' &
      creation_date < '2017-12-01',
    list(n_creations=sum(.N)),
    by=list(bucket, creation_date)],
  autoconfirmed_articles[
    creation_date >= '2014-07-01' &
      creation_date < '2017-12-01' &
      !is.na(deletion_date) &
      survival_time >= 60*60*24*30,
    list(n_deleted=sum(.N)),
    by=list(bucket, creation_date)],
  by=c('bucket', 'creation_date'));

autoconfirmed_survival_by_bucket_date[, deleted_prop := n_deleted/n_creations];

ggplot(autoconfirmed_survival_by_bucket_date,
       aes(x=creation_date, y=100*deleted_prop, color=bucket)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of autoconfirmed article creations surviving 30 days");
##  geom_smooth(method='loess', span=0.2) + ylim(0, 100);

## Note, H14 requires us to measure survival by looking at whether an article
## is deleted within the first 30 days or not.

## Let's run a similar analysis as before, comparing the first two months
## of ACTRIAL with the same time period during 2014, 2015, and 2016?

## First, historical data
autoconfirmed_deleted_30 = merge(
  autoconfirmed_articles[creation_date >= '2014-07-01' &
                           creation_date < '2017-11-15',
                         list(n_creations=sum(.N)), by=creation_date],
  autoconfirmed_articles[creation_date >= '2014-07-01' &
                           creation_date < '2017-11-15' &
                           !is.na(deletion_date) &
                           survival_time < 60*60*24*30,
                         list(n_deletions=sum(.N)),
                         by=creation_date],
  by='creation_date');
autoconfirmed_deleted_30[, prop_deleted := 100*n_deletions/n_creations];

ggplot(autoconfirmed_deleted_30,
       aes(x=creation_date, y=prop_deleted)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits=c(0,50)) +
  xlab('Article creation date') + ylab('Proportion of articles deleted (%)') +
  ggtitle("Autoconfirmed article creations deleted within 30 days") +
  geom_smooth(method='loess', span=0.2);

autoconfirmed_articles_recent_accounts = merge(
  autoconfirmed_articles[creation_date >= '2014-07-01' &
                           creation_date < '2017-11-15' & user_age_days < 30,
                           list(n_creations=sum(.N)), by=creation_date],
  autoconfirmed_articles[creation_date >= '2014-07-01' &
                           creation_date < '2017-11-15' & user_age_days < 30 &
                           !is.na(deletion_date) &
                           survival_time < 60*60*24*30,
                         list(n_deleted=sum(.N)),
                         by=creation_date],
  by='creation_date');
autoconfirmed_articles_older_accounts = merge(
  autoconfirmed_articles[creation_date >= '2014-07-01' &
                           creation_date < '2017-11-15' &
                           user_age_days > 30,
                         list(n_creations=sum(.N)), by=creation_date],
  autoconfirmed_articles[creation_date >= '2014-07-01' &
                           creation_date < '2017-11-15' &
                           user_age_days > 30 &
                           !is.na(deletion_date) &
                           survival_time < 60*60*24*30,
                         list(n_deleted=sum(.N)),
                         by=creation_date],
  by='creation_date');

autoconfirmed_articles_older_accounts[, prop_deleted := 100*n_deleted/n_creations];
autoconfirmed_articles_recent_accounts[, prop_deleted := 100*n_deleted/n_creations];

ggplot() +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line(data=autoconfirmed_articles_recent_accounts,
            aes(x=creation_date, y=prop_deleted, colour='< 30 days')) +
  geom_line(data=autoconfirmed_articles_older_accounts,
            aes(x=creation_date, y=prop_deleted, colour='> 30 days')) +
  scale_color_manual(values=c(
    `< 30 days`=cbPalette[2], `> 30 days`=cbPalette[3])) +
  guides(colour=guide_legend(title='Account age')) +
  scale_y_continuous(limits=c(0,80)) +
  xlab('Article creation date') + ylab('Proportion of articles deleted (%)') +
  ggtitle('Autoconfirmed article creations deleted within 30 days');

## Now, let's compare pre-ACTRIAL to ACTRIAL for accounts younger than 30 days.

autoconfirmed_articles_recent_accounts[
  (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15'),
  list(total_creations=sum(n_creations), total_deletions=sum(n_deleted),
       prop_deleted=100*sum(n_deleted)/sum(n_creations))
  ];
autoconfirmed_articles_recent_accounts[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15'),
  list(total_creations=sum(n_creations), total_deletions=sum(n_deleted),
       prop_deleted=100*sum(n_deleted)/sum(n_creations))
  ];

## And for older than 30 days…

autoconfirmed_articles_older_accounts[
  (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15'),
  list(total_creations=sum(n_creations), total_deletions=sum(n_deleted),
       prop_deleted=100*sum(n_deleted)/sum(n_creations))
  ];
autoconfirmed_articles_older_accounts[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15'),
  list(total_creations=sum(n_creations), total_deletions=sum(n_deleted),
       prop_deleted=100*sum(n_deleted)/sum(n_creations))
  ];

## Curious to learn what the deletion rate is for each bucket.

autoconfirmed_deletion_by_bucket_date = merge(
  autoconfirmed_articles[
    creation_date >= '2014-07-01' &
      creation_date < '2017-11-15',
    list(n_creations=sum(.N)),
    by=list(bucket, creation_date)],
  autoconfirmed_articles[
    creation_date >= '2014-07-01' &
      creation_date < '2017-11-15' &
      !is.na(deletion_date) &
      survival_time < 60*60*24*30,
    list(n_deleted=sum(.N)),
    by=list(bucket, creation_date)],
  by=c('bucket', 'creation_date'));
autoconfirmed_deletion_by_bucket_date[, deleted_prop := n_deleted/n_creations];

autoconfirmed_deletion_by_bucket_date[
  , list(total_creations=sum(n_creations), total_deletions=sum(n_deleted),
         deletion_prop=100*sum(n_deleted)/sum(n_creations)), by=bucket];

## Compare pre-ACTRIAL to ACTRIAL:
autoconfirmed_deletion_by_bucket_date[
  (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15')
  , list(total_creations=sum(n_creations), total_deletions=sum(n_deleted),
         deletion_prop=100*sum(n_deleted)/sum(n_creations)), by=bucket];
autoconfirmed_deletion_by_bucket_date[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15')
  , list(total_creations=sum(n_creations), total_deletions=sum(n_deleted),
         deletion_prop=100*sum(n_deleted)/sum(n_creations)), by=bucket];

## Calculate proportion per month and forecast. Arguably, we'd want to do this
## for various levels of account age, but that will have to wait for later.

autoconfirmed_deleted_30[
  , log_month := format(creation_date, "%Y%m")];
autoconfirmed_deleted_30_by_month = autoconfirmed_deleted_30[
  , list(n_creations = sum(n_creations), n_deletions = sum(n_deletions)),
  by=c('log_month')];
autoconfirmed_deleted_30_by_month[
  , log_date := as.Date(paste0(log_month, "01"), format="%Y%m%d")];
autoconfirmed_deleted_30_by_month[
  ,prop_deleted := 100*n_deletions / n_creations];

ggplot(autoconfirmed_deleted_30_by_month,
       aes(x=log_date, y=prop_deleted)) +
  geom_line() +
  geom_smooth(method='loess', span=0.5) +
  xlab('Year') + ylab('Proportion of articles deleted (%)') +
  scale_y_continuous(limits=c(0, 25)) +
  ggtitle("Autoconfirmed article creations deleted within 30 days");

autoconfirmed_deleted_30_by_month_ts = ts(
  autoconfirmed_deleted_30_by_month[log_date < '2017-09-01']$prop_deleted,
                             start=c(2014,7), end=c(2017,8), frequency=12);
autoplot(autoconfirmed_deleted_30_by_month_ts);

## Let's investigate seasonality:
tsdisplay(diff(autoconfirmed_deleted_30_by_month_ts, 12));
## That's also seasonal, let's diff it again:
tsdisplay(diff(diff(autoconfirmed_deleted_30_by_month_ts, 12)));

## The lack of lags suggests ARIMA(0,1,0)(0,1,0)[12]. Let's check more closely.

ggAcf(diff(diff(autoconfirmed_deleted_30_by_month_ts, 12)), lag.max=36) +
  ggtitle('ACF for autoconfirmed creations deleted in 30 days');
ggPacf(diff(diff(autoconfirmed_deleted_30_by_month_ts, 12)), lag.max=36) +
  ggtitle('PACF for autoconfirmed creations deleted in 30 days');

ac30.model.1 = Arima(autoconfirmed_deleted_30_by_month_ts,
                     order = c(0,1,0), seasonal = c(0,1,0));
ac30.model.2 = Arima(autoconfirmed_deleted_30_by_month_ts,
                     order = c(1,1,0), seasonal = c(0,1,0));

summary(ac30.model.1);
summary(ac30.model.2);

Box.test(residuals(ac30.model.1), lag=36, fitdf=1, type="Ljung");
Box.test(residuals(ac30.model.2), lag=36, fitdf=2, type="Ljung");

ac30.model.3 = auto.arima(autoconfirmed_deleted_30_by_month_ts,
           stepwise = FALSE, approximation = FALSE, parallel = TRUE);
Box.test(residuals(ac30.model.3), lag=36, fitdf=2, type="Ljung");

ggAcf(residuals(ac30.model.3));
ggPacf(residuals(ac30.model.3));

## Let's use the simplest model for now.
autoconfirmed_deleted_30_by_month_fc = forecast(ac30.model.1, h=3);
autoconfirmed_deleted_30_by_month_fc.2 = forecast(ac30.model.3, h=3);

autoplot(autoconfirmed_deleted_30_by_month_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=autoconfirmed_deleted_30_by_month[log_month %in% c('201709', '201710', '201711')]$prop_deleted),
            aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 25)) +
  xlab('Year') + ylab('Proportion of articles deleted (%)') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("Prop autoconfirmed deletion forecast for ACTRIAL");

## Let's do the straightforward comparison of pre-ACTRIAL (2014, 2015, and 2016)
## to ACTRIAL as well.

autoconfirmed_deleted_30[
  (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15'),
  list(n_creations=sum(n_creations), n_deletions=sum(n_deletions))
]
autoconfirmed_deleted_30[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15'),
  list(n_creations=sum(n_creations), n_deletions=sum(n_deletions))
  ]

chisq.test(c(15658, 3775), p=c(0.8675472, 1-0.8675472));
