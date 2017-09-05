## Checking if my HQL query to get article creation events has improved
## by not ignoring deleted articles between early 2012 and mid 2014.

library(data.table);
library(ggplot2);

## Read in the dataset
ac_dataset = data.table(read.table('datasets/enwiki_article_creations.tsv.bz2',
                                   header=TRUE, stringsAsFactors=FALSE, sep='\t'));
ac_dataset[, c("as_reg_date", "as_reg_time") := IDateTime(strptime(event_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz="UTC"))];
ggplot(ac_dataset[as_reg_date < as.Date('2017-07-01'),
                  list(n_creations=sum(.N)),
                  by=as_reg_date],
       aes(x=as_reg_date, y=n_creations)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of articles created') +
  ggtitle('Number of articles created by day') +
  geom_smooth(method='loess', span=0.25);

## There are some dates in 2009 with a huge number of creation events,
## what dates are those?
ac_dataset[as_reg_date < as.Date('2009-07-01'),
           list(n_creations=sum(.N)),
           by=as_reg_date][n_creations > 3000]

## Key dates, Feb 21 with 6,501 creations, Feb 23 with 5,199, March 5 with 5,566,
## April 2 with 5,647, and April 16 with 8,208.