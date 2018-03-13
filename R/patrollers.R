## Investigation of patroller activity

library(ggplot2);
library(data.table);
library(reldist);
library(forecast);
library(tseries);
library(zoo);
library(lubridate);

## Load in the datasets
# patrolactions = fread('datasets/patrolactions_by_day.tsv');
# patrollers = fread('datasets/patrollers_by_day.tsv');
# patrol_summary = fread('datasets/patrol_summaries.tsv');
# patroller_distribution = fread('datasets/patrollers.tsv');
patrol_summary = fread('https://tools.wmflabs.org/actrial/datasets/patrol_summaries.tsv');
patroller_distribution = fread('https://tools.wmflabs.org/actrial/datasets/patrollers.tsv');

## Fix dates
# patrolactions[, log_date := as.IDate(date)];
# patrollers[, log_date := as.IDate(date)];
patrol_summary[, log_date := as.Date(ps_date)];
patroller_distribution[, log_date := as.Date(pat_date)];

## Calculate Gini coefficient over each day
patroller_gini = patroller_distribution[, list(pat_gini=gini(pat_num_actions)),
                                        by=log_date];

## Also the 75th percentile
patroller_75th = patroller_distribution[
  , list(pat_75th=as.numeric(quantile(pat_num_actions, probs=c(0.75)))),
  by=log_date];

## Problem with the 75th percentile is that it can stay constant as the size
## of the population grows. How about we instead calculate the number of review
## actions performed by the top 25% of patrollers?
top_quantile_reviews = function(review_actions, prob=0.25) {
  ## Calculate the number of review actions done by the "prob" quantile of top
  ## reviewers.
  idx = round(length(review_actions) * prob);
  sum(sort(review_actions, decreasing = TRUE)[1:idx]);
}

top_quantile_reviews(patroller_distribution[log_date == '2017-01-01']$pat_num_actions)

top_patrollers = patroller_distribution[
  ,list(total_actions=sum(pat_num_actions),
        top10_actions=top_quantile_reviews(pat_num_actions, prob=0.10),
        top25_actions=top_quantile_reviews(pat_num_actions)),
  by=log_date
  ];

## H9: Number of patrol actions will decrease.

## patrol_actions_per_day_2012-2017.png
## We'll restrict the plot to the first two months of ACTRIAL.
ggplot(patrol_summary[log_date < '2017-11-15'],
       aes(x=log_date, y=ps_num_patrol_actions)) +
  geom_line() +
  geom_smooth(method = 'loess', span = 0.2) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 3000)) +
  ggtitle('Patrol actions per day') +
  xlab('Year') + ylab('Number of patrol actions');
  
## Particular focus on 2016 and 2017:
ggplot(patrol_summary[log_date >= '2016-01-01' &
                        log_date < '2017-11-15'],
       aes(x=log_date, y=ps_num_patrol_actions)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  geom_smooth(method = 'loess', span = 0.2) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(limits = c(0, 2000)) +
  ggtitle('Patrol actions per day 2016-2017') +
  xlab('Month') + ylab('Number of patrol actions');
  
## TODO: determine the relationship between number of moves and number
## of creations. We might want to disregard moves when measuring this ratio,
## beause some, but not all, moves have to be patrolled.
creation_and_move_counts = merge(
  moves_into_main[
    event_date < '2017-11-15'
    ],
  non_autopatrolled_creations_main[
    creation_date < '2017-11-15',
    list(num_creations=sum(.N)), by=creation_date
  ],
  by.x='event_date', by.y='creation_date'
);

creation_and_move_counts[
  , num_creations_and_moves := mim_num_moves_user + mim_num_moves_draft + num_creations];
creation_and_move_counts[
  , prop_moves := 100*(mim_num_moves_user+mim_num_moves_draft)/num_creations_and_moves
];

ggplot(creation_and_move_counts,
       aes(x=event_date, y=prop_moves)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,35)) +
  xlab('Month') + ylab('Proportion in %') +
  ggtitle('Proportion of moves to article creations per day');

ggplot(creation_and_move_counts[event_date >= '2016-01-01'],
       aes(x=event_date, y=prop_moves)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(limits = c(0,35)) +
  xlab('Month') + ylab('Proportion in %') +
  ggtitle('Proportion of moves to article creations per day 2016-2017');

ggplot(creation_and_move_counts,
       aes(x=event_date, y=num_creations)) +
  geom_line();

## The graph suggests that the moves has become a much larger proportion recently,
## and also during ACTRIAL. Given that not all AfC participants have the reviewer
## right, and most likely not all have autopatrol rights, and previous to 2016
## anyone could patrol, I propose we include _all_  moves, thereby asserting that
## they would all have to be reviewed. A more fine-grained analysis is possible,
## but outside the scope at the moment.

## Related measure: Ratio of patrol actions to created articles
patrolactions_related = merge(
  creation_and_move_counts[, list(event_date, num_creations_and_moves)],
  patrol_summary,
  by.x='event_date', by.y='log_date');
patrolactions_related[
  , prop_actions_articles := 100*ps_num_patrol_actions / num_creations_and_moves];

ggplot(patrolactions_related,
       aes(x=event_date, y=prop_actions_articles)) + 
  geom_line() +
  ggtitle('Patrol actions relative to creations and moves') +
  xlab('Year') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  geom_smooth(method='loess', span=0.2);

## Plot since 2016 with a trend line.
ggplot(patrolactions_related[event_date >= '2016-01-01'],
       aes(x=event_date, y=prop_actions_articles)) + 
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(limits = c(0,225)) +
  ggtitle('Patrol actions relative to creations and moves 2016-2017') +
  xlab('Month') + ylab('Prop. of patrol actions to articles in %');

## H10: Number of active patrollers
ggplot(patrol_summary[log_date < '2017-11-15'],
       aes(x=log_date, y=ps_num_patrollers)) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  ggtitle('Active patrollers per day') +
  xlab('Year') + ylab('Number of patrollers') +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 175)) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## 2016 onwards:
ggplot(patrol_summary[log_date >= '2016-01-01' &
                    log_date < '2017-11-15'],
       aes(x=log_date, y=ps_num_patrollers)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 175)) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_smooth(method='loess', span=0.2) +
  ggtitle('Active patrollers per day 2016-2017') +
  xlab('Month') + ylab('Number of patrollers');

ggplot(patrol_summary,
       aes(x=log_date, y=ps_num_patrollers)) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  ggtitle('Active patrollers per day') +
  xlab('Year') + ylab('Number of patrollers') +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 175)) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

ggplot(patrol_summary[log_date >= '2016-01-01'],
       aes(x=log_date, y=ps_num_patrollers)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 175)) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_smooth(method='loess', span=0.2) +
  ggtitle('Active patrollers per day 2016-2017') +
  xlab('Month') + ylab('Number of patrollers');

## Related measure: Ratio of active patrollers to created articles
## I'll switch this one around, I think the ratio of created articles to active
## patrollers is more interesting, as it indicates the number of reviews each
## patroller would have to do in order to keep up with the influx.
patrolactions_related[
  , creations_to_patrollers := num_creations_and_moves / ps_num_patrollers];

ggplot(patrolactions_related,
       aes(x=event_date, y=creations_to_patrollers)) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,20)) +
  ggtitle('Ratio of creations and moves to active patrollers') +
  xlab('Year') + ylab('N creations and moves / N patrollers');
  
ggplot(patrolactions_related[event_date >= '2016-01-01'],
       aes(x=event_date, y=creations_to_patrollers)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(limits = c(0,20)) +
  ggtitle('Ratio of creations and moves to active patrollers') +
  xlab('Month') + ylab('N creations and moves / N patrollers');


## Patroller distribution as measured by the Gini coefficient
ggplot(patroller_gini, aes(x=log_date, y=pat_gini)) + geom_line() +
  ggtitle('Gini coefficient of patroller actions') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  xlab('Date') + ylab('Gini coefficient');

## 75th percentile
ggplot(patroller_75th, aes(x=log_date, y=pat_75th)) + geom_line() +
  ggtitle('75th percentile of patroller actions') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Date') + ylab('Number of patrol actions');

## How much of the patrolling is done by the top 25% of patrollers?
ggplot(top_patrollers[log_date < '2017-11-15',
                      list(log_date, prop_top=100*top25_actions/total_actions)],
       aes(x=log_date, y=prop_top)) + 
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=seq(0,100,10)) +
  ggtitle('Proportion of total patrol actions done by most active quartile') +
  xlab('Year') + ylab('Proportion in %');

## For 2016-2017:
ggplot(top_patrollers[log_date >= '2016-01-01' &
                        log_date < '2017-11-15',
                      list(log_date, prop_top=100*top25_actions/total_actions)],
       aes(x=log_date, y=prop_top)) + 
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=seq(0,100,10)) +
  ggtitle('Proportion of total patrol actions done by most active quartile') +
  xlab('Month') + ylab('Proportion in %');

## Top 10%?
ggplot(top_patrollers[, list(log_date, prop_top=100*top10_actions/total_actions)],
       aes(x=log_date, y=prop_top)) + geom_line() +
  ggtitle('Proportion of total patrol actions done by 90th percentile') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=seq(0,100,10)) +
  xlab('Date') + ylab('Proportion in percent');

## For H9, we should be able to compare ACTRIAL to the same months in 2013-2016.
## Let's make an overall comparison first. What do the distributions look like?

qplot(patrol_summary[
  (log_date >= '2013-09-15' & log_date < '2013-11-15') |
    (log_date >= '2014-09-15' & log_date < '2014-11-15') |
    (log_date >= '2015-09-15' & log_date < '2015-11-15') |
    (log_date >= '2016-09-15' & log_date < '2016-11-15')]$ps_num_patrol_actions,
  geom = 'histogram');
qplot(patrol_summary[
  (log_date >= '2017-09-15' & log_date < '2017-11-15')]$ps_num_patrol_actions,
  geom = 'histogram');

## There are some outliers, but it doesn't appear to be skewed enough for a
## log-transformation to be necessary. We'll use both t-test and wilcox.test, though.
t.test(
  patrol_summary[
    (log_date >= '2013-09-15' & log_date < '2013-11-15') |
      (log_date >= '2014-09-15' & log_date < '2014-11-15') |
      (log_date >= '2015-09-15' & log_date < '2015-11-15') |
      (log_date >= '2016-09-15' & log_date < '2016-11-15')]$ps_num_patrol_actions,
  patrol_summary[
    (log_date >= '2017-09-15' & log_date < '2017-11-15')]$ps_num_patrol_actions
);
wilcox.test(
  patrol_summary[
    (log_date >= '2013-09-15' & log_date < '2013-11-15') |
      (log_date >= '2014-09-15' & log_date < '2014-11-15') |
      (log_date >= '2015-09-15' & log_date < '2015-11-15') |
      (log_date >= '2016-09-15' & log_date < '2016-11-15')]$ps_num_patrol_actions,
  patrol_summary[
    (log_date >= '2017-09-15' & log_date < '2017-11-15')]$ps_num_patrol_actions
);

## Both find that number of patrol actions per day is down. What about the ratio
## of patrol actions to creations?

qplot(patrolactions_related[
  (event_date >= '2013-09-15' & event_date < '2013-11-15') |
    (event_date >= '2014-09-15' & event_date < '2014-11-15') |
    (event_date >= '2015-09-15' & event_date < '2015-11-15') |
    (event_date >= '2016-09-15' & event_date < '2016-11-15')]$prop_actions_articles,
  geom = 'histogram');
qplot(patrolactions_related[
  (event_date >= '2017-09-15' & event_date < '2017-11-15')]$prop_actions_articles,
  geom = 'histogram');

## Some skewness, so let's put more weight on the Mann-Whitney test.
t.test(
  patrolactions_related[
    (event_date >= '2013-09-15' & event_date < '2013-11-15') |
      (event_date >= '2014-09-15' & event_date < '2014-11-15') |
      (event_date >= '2015-09-15' & event_date < '2015-11-15') |
      (event_date >= '2016-09-15' & event_date < '2016-11-15')]$prop_actions_articles,
  patrolactions_related[
    (event_date >= '2017-09-15' & event_date < '2017-11-15')]$prop_actions_articles
);
wilcox.test(
  patrolactions_related[
    (event_date >= '2013-09-15' & event_date < '2013-11-15') |
      (event_date >= '2014-09-15' & event_date < '2014-11-15') |
      (event_date >= '2015-09-15' & event_date < '2015-11-15') |
      (event_date >= '2016-09-15' & event_date < '2016-11-15')]$prop_actions_articles,
  patrolactions_related[
    (event_date >= '2017-09-15' & event_date < '2017-11-15')]$prop_actions_articles
);

## For H10, we need a different approach. It appears that we can compare the first
## two months of ACTRIAL against the first half of 2017. Let's look at the
## distributions:

qplot(patrol_summary[
    (log_date >= '2017-01-01' & log_date < '2017-07-01')]$ps_num_patrollers,
  geom = 'histogram');
qplot(patrol_summary[
  (log_date >= '2017-09-15' & log_date < '2017-11-15')]$ps_num_patrollers,
  geom = 'histogram');

## These are not particularly skewed, a t-test should be fine.
t.test(
  patrol_summary[
    (log_date >= '2017-01-01' & log_date < '2017-07-01')]$ps_num_patrollers,
  patrol_summary[
    (log_date >= '2017-09-15' & log_date < '2017-11-15')]$ps_num_patrollers
);
wilcox.test(
  patrol_summary[
    (log_date >= '2017-01-01' & log_date < '2017-07-01')]$ps_num_patrollers,
  patrol_summary[
    (log_date >= '2017-09-15' & log_date < '2017-11-15')]$ps_num_patrollers
);

## Related measure, again comparing Q1+Q2 2017 against ACTRIAL:
qplot(patrolactions_related[
  (event_date >= '2017-01-01' & event_date < '2017-07-01')]$creations_to_patrollers,
  geom = 'histogram');
qplot(patrolactions_related[
  (event_date >= '2017-09-15' & event_date < '2017-11-15')]$creations_to_patrollers,
  geom = 'histogram');

## Not particularly skewed either, a t-test should be fine:
t.test(
  patrolactions_related[
    (event_date >= '2017-01-01' & event_date < '2017-07-01')]$creations_to_patrollers,
  patrolactions_related[
    (event_date >= '2017-09-15' & event_date < '2017-11-15')]$creations_to_patrollers
);
wilcox.test(
  patrolactions_related[
    (event_date >= '2017-01-01' & event_date < '2017-07-01')]$creations_to_patrollers,
  patrolactions_related[
    (event_date >= '2017-09-15' & event_date < '2017-11-15')]$creations_to_patrollers
);

## H11: Based on historical data, we should be able to compare ACTRIAL to the
## same months in 2013-2016.

qplot(
  top_patrollers[(log_date >= '2013-09-15' & log_date < '2013-11-15') |
                   (log_date >= '2014-09-15' & log_date < '2014-11-15') |
                   (log_date >= '2015-09-15' & log_date < '2015-11-15') |
                   (log_date >= '2016-09-15' & log_date < '2016-11-15'),
                 list(prop_top=100*top25_actions/total_actions)]$prop_top,
  geom = 'histogram'
);
qplot(
  top_patrollers[(log_date >= '2017-09-15' & log_date < '2017-11-15'),
                 list(prop_top=100*top25_actions/total_actions)]$prop_top,
  geom = 'histogram'
);

## It's a bit skewed due to it being so close to 100%, so we'll weigh the
## non-parametric test more.
t.test(
  top_patrollers[(log_date >= '2013-09-15' & log_date < '2013-11-15') |
                   (log_date >= '2014-09-15' & log_date < '2014-11-15') |
                   (log_date >= '2015-09-15' & log_date < '2015-11-15') |
                   (log_date >= '2016-09-15' & log_date < '2016-11-15'),
                 list(prop_top=100*top25_actions/total_actions)]$prop_top,
  top_patrollers[(log_date >= '2017-09-15' & log_date < '2017-11-15'),
                 list(prop_top=100*top25_actions/total_actions)]$prop_top
);
wilcox.test(
  top_patrollers[(log_date >= '2013-09-15' & log_date < '2013-11-15') |
                   (log_date >= '2014-09-15' & log_date < '2014-11-15') |
                   (log_date >= '2015-09-15' & log_date < '2015-11-15') |
                   (log_date >= '2016-09-15' & log_date < '2016-11-15'),
                 list(prop_top=100*top25_actions/total_actions)]$prop_top,
  top_patrollers[(log_date >= '2017-09-15' & log_date < '2017-11-15'),
                 list(prop_top=100*top25_actions/total_actions)]$prop_top
);

## Forecasting models for H9, H10, and H11. Here we'll aim to look at the
## data two weeks at a time, as that coincides with ACTRIAL. It also gives
## us more data points than doing monthly data, and we've seen that some of
## the datasets have shifts in the means.

## H9: Number of patrol actions.
patrol_summary[day(log_date) < 15,
               reg_bimonth := format(log_date, "%Y%m01")];
patrol_summary[day(log_date) >= 15,
               reg_bimonth := format(log_date, "%Y%m15")];

patrol_summary_bimonthly = patrol_summary[
  , list(num_patrol_actions=sum(ps_num_patrol_actions)),
  by=c('reg_bimonth')];
patrol_summary_bimonthly[
  , reg_date := as.Date(reg_bimonth, format="%Y%m%d")];

ggplot(patrol_summary_bimonthly[reg_date < '2017-12-01'],
       aes(x=reg_date, y=num_patrol_actions)) +
  geom_line() +
  ggtitle('Patrol actions measured bimonthly') +
  xlab('Date') + ylab('Number of patrol actions') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20000)) +
  geom_smooth(method='loess', span=0.2);

## Let's try to build a 24-cycle forecasting model.

patrol_actions_bimonthly_ts = ts(
  patrol_summary_bimonthly[reg_date < '2017-09-15']$num_patrol_actions,
  frequency=24);
autoplot(patrol_actions_bimonthly_ts);

## Let's investigate seasonality:
tsdisplay(patrol_actions_bimonthly_ts);
tsdisplay(diff(patrol_actions_bimonthly_ts));
tsdisplay(diff(patrol_actions_bimonthly_ts/1000, differences = 24));

## Does not appear to be seasonal, and appears non-stationary.
adf.test(patrol_actions_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(patrol_actions_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(patrol_actions_bimonthly_ts, differences = 12),
         alternative = 'stationary');

## Looks like an ARIMA(2,1,0)(0,1,1)[12] model could work for this.

patrol_actions_bimonthly_model.auto = auto.arima(
  patrol_actions_bimonthly_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(patrol_actions_bimonthly_model.auto);

ggAcf(residuals(patrol_actions_bimonthly_model.auto));
ggPacf(residuals(patrol_actions_bimonthly_model.auto));
Box.test(residuals(patrol_actions_bimonthly_model.auto),
         lag=72, fitdf=8, type="Ljung");

## Model looks reasonably good, although somewhat complex compared to what
## we've seen previously. Let's try manual approaches.

patrol_actions_bimonthly_model = Arima(
  patrol_actions_bimonthly_ts,
  order = c(3,1,0),
  seasonal = list(order = c(0,1,1),
                  frequency = 24)
);
summary(patrol_actions_bimonthly_model);

ggAcf(residuals(patrol_actions_bimonthly_model));
ggPacf(residuals(patrol_actions_bimonthly_model));
Box.test(residuals(patrol_actions_bimonthly_model),
         lag=72, fitdf=5, type="Ljung");

## This model looks much better.

patrol_actions_bimonthly_fc = forecast(
  patrol_actions_bimonthly_model, h=4);

autoplot(patrol_actions_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=5 + 23/24 + (c(0:3)/24),
      y=patrol_summary_bimonthly[
          reg_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
          ]$num_patrol_actions),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 20000)) +
  scale_x_continuous(breaks = 1 + c(0:21)/4, minor_breaks = NULL,
                     labels = c('Oct', 'Jan 2013', 'Apr', 'Jul', 'Oct',
                                'Jan 2014', 'Apr', 'Jul', 'Oct',
                                'Jan 2015', 'Apr', 'Jul', 'Oct',
                                'Jan 2016', 'Apr', 'Jul', 'Oct',
                                'Jan 2017', 'Apr', 'Jul', 'Oct', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Number of patrol actions') +
  ggtitle("ACTRIAL forecast for bimonthly number of patrol actions");

## H9's related measure, calculated over a two week period. Note that this will
## likely be higher than our daily calculation, but that's expected due to the
## uneven distribution of patrolling activity.

creation_and_move_counts[day(event_date) < 15,
                         reg_bimonth := format(event_date, "%Y%m01")];
creation_and_move_counts[day(event_date) >= 15,
                         reg_bimonth := format(event_date, "%Y%m15")];

patrol_h9_related = merge(
  patrol_summary_bimonthly,
  creation_and_move_counts[
    , list(num_creations=sum(num_creations_and_moves)),
    by='reg_bimonth'
    ],
  by='reg_bimonth'
);
patrol_h9_related[
  , prop_actions_articles := 100*num_patrol_actions / num_creations];

ggplot(patrol_h9_related,
       aes(x=reg_date, y=prop_actions_articles)) +
  geom_line() +
  ggtitle('Proportion of patrol actions to creations and moves measured bimonthly') +
  xlab('Date') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  geom_smooth(method='loess', span=0.2);

patrol_h9_related_ts = ts(
  patrol_h9_related[reg_date < '2017-09-15']$prop_actions_articles,
  frequency=24);
autoplot(patrol_h9_related_ts);

## Let's investigate stationarity and seasonality:
tsdisplay(patrol_h9_related_ts);
tsdisplay(diff(patrol_h9_related_ts));
tsdisplay(diff(patrol_h9_related_ts, differences = 24));

## Does not appear to be seasonal, and appears non-stationary.
adf.test(patrol_actions_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(patrol_actions_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(patrol_actions_bimonthly_ts, differences = 24),
         alternative = 'stationary');

## Looks like an ARIMA(2,1,0) model could work for this.

patrol_h9_related_ts_model.auto = auto.arima(
  patrol_h9_related_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(patrol_h9_related_ts_model.auto);

ggAcf(residuals(patrol_h9_related_ts_model.auto));
ggPacf(residuals(patrol_h9_related_ts_model.auto));
Box.test(residuals(patrol_h9_related_ts_model.auto),
         lag=72, fitdf=8, type="Ljung");

## Model looks reasonably good, but it's not obvious that the seasonal
## component is needed. Let's check.

# patrol_h9_related_ts_model = Arima(
#   patrol_h9_related_ts,
#   order = c(0,1,3)
# );
# summary(patrol_h9_related_ts_model);
# 
# ggAcf(residuals(patrol_h9_related_ts_model));
# ggPacf(residuals(patrol_h9_related_ts_model));
# Box.test(residuals(patrol_h9_related_ts_model),
#          lag=72, fitdf=5, type="Ljung");

## The models are comparable. The question is whether we see a need to include
## the seasonal component or not. Based on the training data, there doesn't appear
## to be a strong reason, but the ACF/PACF does suggest a seasonal trend. I'll
## choose to go with the seasonal model, as the models are comparable in complexity.

patrol_h9_related_fc = forecast(
  patrol_h9_related_ts_model.auto, h=4);

autoplot(patrol_h9_related_fc) +
  geom_line(
    data=data.frame(
      x=5 + 23/24 + (c(0:3)/24),
      y=patrol_h9_related[
        reg_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$prop_actions_articles),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(breaks = 1 + c(0:21)/4, minor_breaks = NULL,
                     labels = c('Oct', 'Jan 2013', 'Apr', 'Jul', 'Oct',
                                'Jan 2014', 'Apr', 'Jul', 'Oct',
                                'Jan 2015', 'Apr', 'Jul', 'Oct',
                                'Jan 2016', 'Apr', 'Jul', 'Oct',
                                'Jan 2017', 'Apr', 'Jul', 'Oct', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Number of patrol actions') +
  ggtitle("ACTRIAL forecast for bimonthly proportion of patrol actions to creations and moves");

## H10: Number of active patrollers
patrollers_active_bimonthly = patroller_distribution[
  pat_date < '2017-11-15',
  list(num_patrollers = length(unique(pat_userid))),
  by='reg_bimonth'];
patrollers_active_bimonthly[
  , reg_date := as.Date(reg_bimonth, format="%Y%m%d")];

ggplot(patrollers_active_bimonthly[reg_date < '2017-12-01'],
       aes(x=reg_date, y=num_patrollers)) +
  geom_line() +
  ggtitle('Active patrollers measured bimonthly') +
  xlab('Date') + ylab('Number of active patrollers') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 800),
                     breaks = c(0:8)*100) +
  geom_smooth(method='loess', span=0.2);

## Let's try to build a 24-cycle forecasting model.

patrollers_active_bimonthly_ts = ts(
  patrollers_active_bimonthly[reg_date < '2017-09-15']$num_patrollers,
  frequency=24);
autoplot(patrollers_active_bimonthly_ts);

## Let's investigate stationarity and seasonality:
tsdisplay(patrollers_active_bimonthly_ts);
tsdisplay(diff(patrollers_active_bimonthly_ts));
tsdisplay(diff(patrollers_active_bimonthly_ts, differences = 24));

## Not clear that it is seasonal, but it's definitely not stationary.
adf.test(patrollers_active_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(patrollers_active_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(patrollers_active_bimonthly_ts, differences = 12),
         alternative = 'stationary');

## Looks like an ARIMA(3,1,0) model could work for this.

patrollers_active_bimonthly_model.auto = auto.arima(
  patrollers_active_bimonthly_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(patrollers_active_bimonthly_model.auto);

ggAcf(residuals(patrollers_active_bimonthly_model.auto));
ggPacf(residuals(patrollers_active_bimonthly_model.auto));
Box.test(residuals(patrollers_active_bimonthly_model.auto),
         lag=72, fitdf=8, type="Ljung");

## Model looks very good. Do not know if we need the seasonal component,
## though. Let's investigate.
## ARIMA(3,1,0) has strong yearly autocorrelation (lags 24 and 48).

## Note that including drift in the model improves fitness, but does not lead
## to a forecast with a much narrower confidence interval. In this case, we might
## need more advanced forecasting techniques to account for the shifts.
patrollers_active_bimonthly_model = Arima(
  patrollers_active_bimonthly_ts,
  order = c(1,1,1),
  seasonal = list(order = c(0,0,1),
                  frequency = 24)
);
summary(patrollers_active_bimonthly_model);

ggAcf(residuals(patrollers_active_bimonthly_model));
ggPacf(residuals(patrollers_active_bimonthly_model));
Box.test(residuals(patrollers_active_bimonthly_model),
         lag=72, fitdf=5, type="Ljung");

## This model has a much lower AICc and BIC than the auto-generated model.
## I'll choose this one, then.

patrollers_active_bimonthly_fc = forecast(
  patrollers_active_bimonthly_model, h=4);

autoplot(patrollers_active_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=5 + 23/24 + (c(0:3)/24),
      y=patrollers_active_bimonthly[
        reg_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$num_patrollers),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 800),
                     breaks = c(0:8)*100) +
  scale_x_continuous(breaks = 1 + c(0:21)/4, minor_breaks = NULL,
                     labels = c('Oct', 'Jan 2013', 'Apr', 'Jul', 'Oct',
                                'Jan 2014', 'Apr', 'Jul', 'Oct',
                                'Jan 2015', 'Apr', 'Jul', 'Oct',
                                'Jan 2016', 'Apr', 'Jul', 'Oct',
                                'Jan 2017', 'Apr', 'Jul', 'Oct', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Number of patrol actions') +
  ggtitle("ACTRIAL forecast for bimonthly number of active patrollers");

## H10's related measure, calculated over a two week period. Note that this will
## likely be higher than our daily calculation, but that's expected due to the
## uneven distribution of patrolling activity.

patroller_distribution[day(log_date) < 15,
               reg_bimonth := format(log_date, "%Y%m01")];
patroller_distribution[day(log_date) >= 15,
               reg_bimonth := format(log_date, "%Y%m15")];
creation_and_move_counts[day(event_date) < 15,
                         reg_bimonth := format(event_date, "%Y%m01")];
creation_and_move_counts[day(event_date) >= 15,
                        reg_bimonth := format(event_date, "%Y%m15")];

patrol_h10_related = merge(
  patroller_distribution[
    pat_date < '2017-11-15',
    list(num_patrollers = length(unique(pat_userid))),
    by='reg_bimonth'],
  creation_and_move_counts[
    , list(num_creations=sum(num_creations_and_moves)),
    by='reg_bimonth'
  ],
  by='reg_bimonth'
);
patrol_h10_related[
  , creations_to_patrollers := num_creations / num_patrollers];
patrol_h10_related[, reg_date := as.Date(reg_bimonth, format="%Y%m%d")];

ggplot(patrol_h10_related[reg_date < '2017-12-01'],
       aes(x=reg_date, y=creations_to_patrollers)) +
  geom_line() +
  ggtitle('Ratio of creations and moves to active patrollers measured bimonthly') +
  xlab('Date') + ylab('N creations and moves / N active patrollers') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60)) +
  geom_smooth(method='loess', span=0.2);

patrol_h10_related_ts = ts(
  patrol_h10_related[reg_date < '2017-09-15']$creations_to_patrollers,
  frequency=24);
autoplot(patrol_h10_related_ts);

## Let's investigate stationarity and seasonality:
tsdisplay(patrol_h10_related_ts);
tsdisplay(diff(patrol_h10_related_ts));
tsdisplay(diff(patrol_h10_related_ts, differences = 24));

## Is not stationary, and appears that a seasonal model greatly improves
## the ACF and PACF.
adf.test(patrol_h10_related_ts,
         alternative = 'stationary');
adf.test(diff(patrol_h10_related_ts),
         alternative = 'stationary');
adf.test(diff(patrol_h10_related_ts, differences = 24),
         alternative = 'stationary');

## Based on the seasonal ACF and PACF, an ARIMA(0,1,2)(0,1,1)[24] model
## is a good candidate.

patrol_h10_related_model.auto = auto.arima(
  patrol_h10_related_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(patrol_h10_related_model.auto);

ggAcf(residuals(patrol_h10_related_model.auto));
ggPacf(residuals(patrol_h10_related_model.auto));
Box.test(residuals(patrol_h10_related_model.auto),
         lag=72, fitdf=8, type="Ljung");

## Model looks reasonably good. Can we improve on it?

# patrol_h10_related_model = Arima(
#   patrol_h10_related_ts,
#   order = c(3,1,0),
#   include.drift = TRUE
# );
# summary(patrol_h10_related_model);
# 
# ggAcf(residuals(patrol_h10_related_model));
# ggPacf(residuals(patrol_h10_related_model));
# Box.test(residuals(patrol_h10_related_model),
#          lag=72, fitdf=5, type="Ljung");

## Not clear that we can improve upon the auto-generated model.
## Occam's razoer applies, choose the simpler model.

patrol_h10_related_fc = forecast(
  patrol_h10_related_model.auto, h=4);

autoplot(patrol_h10_related_fc) +
  geom_line(
    data=data.frame(
      x=5 + 23/24 + (c(0:3)/24),
      y=patrol_h10_related[
        reg_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$creations_to_patrollers),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_continuous(breaks = 1 + c(0:21)/4, minor_breaks = NULL,
                     labels = c('Oct', 'Jan 2013', 'Apr', 'Jul', 'Oct',
                                'Jan 2014', 'Apr', 'Jul', 'Oct',
                                'Jan 2015', 'Apr', 'Jul', 'Oct',
                                'Jan 2016', 'Apr', 'Jul', 'Oct',
                                'Jan 2017', 'Apr', 'Jul', 'Oct', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Number of patrol actions') +
  ggtitle("ACTRIAL forecast for ratio of creations and moves to patrollers");

## H11: Distribution of activity.
top_patrollers_bimonthly = patroller_distribution[
  ,list(total_actions=sum(pat_num_actions),
        top10_actions=top_quantile_reviews(pat_num_actions, prob=0.10),
        top25_actions=top_quantile_reviews(pat_num_actions)),
  by=reg_bimonth
  ];

top_patrollers_bimonthly[
  , prop_top_25 := 100*top25_actions / total_actions];
top_patrollers_bimonthly[
  , reg_date := as.Date(reg_bimonth, format='%Y%m%d')];

ggplot(top_patrollers_bimonthly[reg_date < '2017-12-01'],
       aes(x=reg_date, y=prop_top_25)) +
  geom_line() +
  ggtitle('Proportion of patrol actions by top quartile measured bimonthly') +
  xlab('Date') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  geom_smooth(method='loess', span=0.2);

top_patrollers_bimonthly_ts = ts(
  top_patrollers_bimonthly[reg_date < '2017-09-15']$prop_top_25,
  frequency=24);
autoplot(top_patrollers_bimonthly_ts);

## Let's investigate stationarity and seasonality:
tsdisplay(top_patrollers_bimonthly_ts);
tsdisplay(diff(top_patrollers_bimonthly_ts));
tsdisplay(diff(top_patrollers_bimonthly_ts, differences = 24));

## Is not stationary, and appears that a seasonal model greatly simplifies
## the ACF and PACF.
adf.test(top_patrollers_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(top_patrollers_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(top_patrollers_bimonthly_ts, differences = 24),
         alternative = 'stationary');

## Based on the seasonal ACF and PACF, an ARIMA(0,1,3)(0,1,1)[24] model
## is a likely candidate. Other models might be good too.

top_patrollers_bimonthly_model.auto = auto.arima(
  top_patrollers_bimonthly_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(top_patrollers_bimonthly_model.auto);

ggAcf(residuals(top_patrollers_bimonthly_model.auto));
ggPacf(residuals(top_patrollers_bimonthly_model.auto));
Box.test(residuals(top_patrollers_bimonthly_model.auto),
         lag=72, fitdf=8, type="Ljung");

## Model looks reasonably good. Can we improve on it?

# top_patrollers_bimonthly_model = Arima(
#   top_patrollers_bimonthly_ts,
#   order = c(1,0,1),
#   seasonal = list(order = c(1,0,1),
#                   frequency = 24),
#   include.mean = TRUE
# );
# summary(top_patrollers_bimonthly_model);
# 
# ggAcf(residuals(top_patrollers_bimonthly_model));
# ggPacf(residuals(top_patrollers_bimonthly_model));
# Box.test(residuals(top_patrollers_bimonthly_model),
#          lag=72, fitdf=5, type="Ljung");

## Not clear that we can improve upon the auto-generated model.
## Occam's razoer applies, choose the simpler model.

top_patrollers_bimonthly_fc = forecast(
  top_patrollers_bimonthly_model.auto, h=4);

autoplot(top_patrollers_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=5 + 23/24 + (c(0:3)/24),
      y=top_patrollers_bimonthly[
        reg_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$prop_top_25),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = 1 + c(0:21)/4, minor_breaks = NULL,
                     labels = c('Oct', 'Jan 2013', 'Apr', 'Jul', 'Oct',
                                'Jan 2014', 'Apr', 'Jul', 'Oct',
                                'Jan 2015', 'Apr', 'Jul', 'Oct',
                                'Jan 2016', 'Apr', 'Jul', 'Oct',
                                'Jan 2017', 'Apr', 'Jul', 'Oct', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("ACTRIAL forecast for proportion of patrol actions by top quartile");
