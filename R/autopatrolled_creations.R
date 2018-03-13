## Estimating the proportion of articles created by autopatrolled users.
## This does not consider articles that are published through moves from
## User or Draft namespaces.

library(data.table);
library(ggplot2);

## Read in the article creation dataset:
ac_dataset = data.table(read.table('datasets/enwiki_article_creations.tsv.bz2',
                                   header=TRUE, stringsAsFactors=FALSE, sep='\t'));
ac_dataset[, c("as_reg_date", "as_reg_time") := IDateTime(strptime(event_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz="UTC"))];

## Plot to check:
ggplot(ac_dataset[as_reg_date < as.Date('2017-07-01'),
                  list(n_creations=sum(.N)),
                  by=as_reg_date],
       aes(x=as_reg_date, y=n_creations)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of articles created') +
  ggtitle('Number of articles created by day') +
  geom_smooth(method='loess', span=0.25);

## Calculate number of creations per day:
articlecreations_per_day = ac_dataset[as_reg_date < as.Date('2017-07-01'),
                                      list(n_articles=sum(.N)),
                                      by=as_reg_date];

## Read in the non-autopatrolled article creation dataset:
nap_ac_dataset = data.table(read.table('datasets/enwiki_non-autopatrolled_creations.tsv.bz2',
                                       header=TRUE, stringsAsFactors=FALSE, sep='\t'));
nap_ac_dataset[, c("as_reg_date", "as_reg_time") := IDateTime(strptime(event_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz="UTC"))];

## Plot to check:
## Plot to check:
ggplot(nap_ac_dataset[as_reg_date < as.Date('2017-07-01'),
                      list(n_creations=sum(.N)),
                      by=as_reg_date],
       aes(x=as_reg_date, y=n_creations)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of articles created') +
  ggtitle('Number of articles created by day') +
  geom_smooth(method='loess', span=0.25);

## Calculate number of articles created per day:
nap_articlecreations_per_day = nap_ac_dataset[as_reg_date < as.Date('2017-07-01'),
                                              list(n_nonap_articles=sum(.N)),
                                              by=as_reg_date];

## Merge the two datasets and calculate proportions:
nap_articlecreations_per_day = merge(articlecreations_per_day,
                                     nap_articlecreations_per_day,
                                     by='as_reg_date');
nap_articlecreations_per_day[, prop_non_autopatrolled := n_nonap_articles / n_articles];

## Plot the proportions:
ggplot(nap_articlecreations_per_day,
       aes(x=as_reg_date, y=100*(1-prop_non_autopatrolled))) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks=seq(0,80,10)) +
  xlab('Year') + ylab('Proportion of autopatrolled articles (in %)') +
  ggtitle('Proportion of autopatrolled articles created per day');
