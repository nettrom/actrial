## Investigation of patroller activity

library(ggplot);
library(data.table);
library(reldist);

## Load in the datasets
patrolactions = fread('datasets/patrolactions_by_day.tsv');
patrollers = fread('datasets/patrollers_by_day.tsv');
patroller_distribution = fread('datasets/patroller_distribution.tsv');

## Fix dates
patrolactions[, log_date := as.IDate(date)];
patrollers[, log_date := as.IDate(date)];
patroller_distribution[, log_date := as.IDate(pat_date)];

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

## H9: Number of patrol actions
ggplot(patrolactions, aes(x=log_date, y=num_patrol_actions)) + geom_line() +
  ggtitle('Patrol actions per day') +
  xlab('Year') + ylab('Number of patrol actions') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## Related measure: Ratio of patrol actions to created articles
patrolactions_related = merge(
  creations_and_moves[, list(n_articles=sum(n_articles)), by=c_date],
  patrolactions,
  by.x='c_date', by.y='log_date')
patrolactions_related[, prop_acts_arts := num_patrol_actions / n_articles];

ggplot(patrolactions_related, aes(x=c_date, y=100*prop_acts_arts)) + geom_line() +
  ggtitle('Proportion of patrol actions to created articles') +
  xlab('Year') + ylab('Prop. of patrol actions to articles in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## I'm curious to see how that proportion has developed since 2015,
## or maybe later.
ggplot(patrolactions_related[c_date >= as.IDate('2016-01-01')],
       aes(x=c_date, y=100*prop_acts_arts)) + geom_line() +
  ggtitle('Proportion of patrol actions to created articles since 2016') +
  xlab('Year') + ylab('Prop. of patrol actions to articles in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## H10: Number of active patrollers
ggplot(patrollers, aes(x=log_date, y=num_patrollers)) + geom_line() +
  ggtitle('Active patrollers per day') +
  xlab('Year') + ylab('Number of active patrollers') +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 175)) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## Let's add a smoothed line to the plot:
ggplot(patrollers, aes(x=log_date, y=num_patrollers)) + geom_line() +
  ggtitle('Active patrollers per day') +
  xlab('Year') + ylab('Number of active patrollers') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  geom_smooth(method='loess', span=0.1);

## Related measure: Ratio of active patrollers to created articles
## I'll switch this one around, I think the ratio of created articles to active
## patrollers is more interesting, as it indicates the number of reviews each
## patroller would have to do in order to keep up with the influx.
patrollers_related = merge(
  creations_and_moves[, list(n_articles=sum(n_articles)), by=c_date],
  patrollers,
  by.x='c_date', by.y='log_date')
patrollers_related[, prop_arts_pats := n_articles / num_patrollers];

ggplot(patrollers_related, aes(x=c_date, y=prop_arts_pats)) + geom_line() +
  ggtitle('Ratio of patrollers to created articles') +
  xlab('Year') + ylab('N articles / N patrollers') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## Total number of created articles with Loess-smoothed line
ggplot(creations_and_moves[, list(n_articles=sum(n_articles)), by=c_date],
       aes(x=c_date, y=n_articles)) + geom_line() +
  ggtitle('Number of articles created per day (including moves into main)') +
  geom_smooth(method='loess', span=0.1) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0)) +
  xlab('Date') + ylab('Number of articles')

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
ggplot(top_patrollers[, list(log_date, prop_top=100*top25_actions/total_actions)],
       aes(x=log_date, y=prop_top)) + geom_line() +
  ggtitle('Proportion of total patrol actions done by most active quartile') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=seq(0,100,10)) +
  xlab('Date') + ylab('Proportion in percent');

## Top 10%?
ggplot(top_patrollers[, list(log_date, prop_top=100*top10_actions/total_actions)],
       aes(x=log_date, y=prop_top)) + geom_line() +
  ggtitle('Proportion of total patrol actions done by 90th percentile') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=seq(0,100,10)) +
  xlab('Date') + ylab('Proportion in percent');
