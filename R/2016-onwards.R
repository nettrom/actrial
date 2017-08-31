## Graphs from 2016 onwards with a smoothed trend line added.
## Expects the datasets from articlecreations.R and patrollers.R
## to be loaded into memory.

## Article creations
ggplot(creations_and_moves[c_date >= as.Date('2016-01-01'),
                           list(n_articles=sum(n_articles)),
                           by=c_date], aes(x=c_date, y=n_articles)) + geom_line() +
  ggtitle('Articles created per day since 2016') +
  xlab('Year') + ylab('Number of articles') +
  scale_x_date(date_breaks='1 month', date_labels = '%Y-%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2250)) +
  geom_smooth(method='loess', span=0.2);

ggplot(creations_and_moves[c_date >= as.Date('2016-01-01')],
       aes(x=c_date, y=n_articles, group=type, colour=type)) + geom_line() +
  ggtitle('Articles created per day since 2016') +
  xlab('Year') + ylab('Number of articles') +
  scale_x_date(date_breaks='1 month', date_labels = '%Y-%m') +
  scale_colour_manual(values=cbbPalette) +
  geom_smooth(method='loess', span=0.2);

## Proportion of patrol actions to articles
ggplot(patrolactions_related[c_date >= as.IDate('2016-01-01')],
       aes(x=c_date, y=100*prop_acts_arts)) + geom_line() +
  ggtitle('Proportion of patrol actions to created articles since 2016') +
  xlab('Year') + ylab('Prop. of patrol actions to articles in %') +
  scale_x_date(date_breaks='1 month', date_labels = '%Y-%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  geom_smooth(method='loess', span=0.2);

## Number of active patrollers
ggplot(patrollers[log_date >= as.IDate('2016-01-01')
                  & log_date < as.IDate('2017-07-01')],
       aes(x=log_date, y=num_patrollers)) + geom_line() +
  ggtitle('Active patrollers per day since 2016') +
  xlab('Year') + ylab('Number of active patrollers') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 175)) +
  scale_x_date(date_breaks='1 month', date_labels = '%Y-%m') +
  geom_smooth(method='loess', span=0.2);

## How much of the patrolling is done by the top 25% of patrollers?
ggplot(top_patrollers[log_date >= as.IDate('2016-01-01'),
                      list(log_date, prop_top=100*top25_actions/total_actions)],
       aes(x=log_date, y=prop_top)) + geom_line() +
  ggtitle('Prop. of all patrol actions done by most active quartile since 2016') +
  scale_x_date(date_breaks='1 month', date_labels = '%Y-%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=seq(0,100,10)) +
  xlab('Year') + ylab('Proportion in percent') + 
  geom_smooth(method='loess', span=0.2);

## Patrol actions per day
ggplot(patrolactions[log_date >= as.IDate('2016-01-01')],
       aes(x=log_date, y=num_patrol_actions)) + geom_line() +
  ggtitle('Patrol actions per day since 2016') +
  xlab('Date') + ylab('Number of patrol actions') +
  scale_x_date(date_breaks='1 month', date_labels = '%Y-%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2550)) +
  geom_smooth(method='loess', span=0.2);
