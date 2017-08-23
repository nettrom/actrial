## Investigation of patroller activity

library(ggplot);
library(data.frame);

## Load in the datasets
patrolactions = fread('datasets/patrolactions_by_day.tsv');
patrollers = fread('datasets/patrollers_by_day.tsv');

## Fix dates
patrolactions[, log_date := as.IDate(date)];
patrollers[, log_date := as.IDate(date)];

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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70)) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## Let's add a smoothed line to the plot:
ggplot(patrollers, aes(x=log_date, y=num_patrollers)) + geom_line() +
  ggtitle('Active patrollers per day') +
  xlab('Year') + ylab('Number of active patrollers') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70)) +
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2750)) +
  xlab('Date') + ylab('Number of articles')
