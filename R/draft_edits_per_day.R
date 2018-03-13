## Quick plot of the number of edits per day in the Draft namespace:

library(data.table);
library(ggplot2);

draft_edits_per_day = fread('datasets/draft_edits_per_day.tsv');
draft_edits_per_day[, rev_date := as.Date(rev_date)];

draft_edits_per_day[
  , num_edits.ma7 := rollapply(num_edits, width=7,
                               FUN=mean, na.rm=TRUE,
                               fill=0, align='right')];
draft_edits_per_day[
  , num_edits.md7 := rollapply(num_edits, width=7,
                               FUN=median, na.rm=TRUE,
                               fill=0, align='right')];

ggplot(draft_edits_per_day,
       aes(x=rev_date, y=num_edits)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of edits') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  geom_smooth(method='loess', span=0.1) +
  ggtitle('Draft namespace edits since Jan 1 2017');

ggplot(draft_edits_per_day,
       aes(x=rev_date, y=num_edits.ma7)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of edits') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  ggtitle('Draft namespace edits, 7-day moving average');

ggplot(draft_edits_per_day,
       aes(x=rev_date, y=num_edits.md7)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of edits') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  ggtitle('Draft namespace edits, 7-day moving median');
