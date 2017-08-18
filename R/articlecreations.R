## Preliminary analysis of article creations

library(ggplot2);
library(data.table);

## Read in the dataset
articlecreations = fread('datasets/enwiki_article_creations.tsv');

articlecreations[, c("as_reg_date", "as_reg_time") := IDateTime(strptime(event_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz="UTC"))];

ggplot(articlecreations[as_reg_date < as.Date('2017-07-01'),
                        list(n_creations=sum(.N)), by=as_reg_date],
       aes(x=as_reg_date, y=n_creations)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  geom_smooth() +
  xlab('Year') + ylab('Number of articles created') +
  ggtitle('Number of articles created by day');

write.table(articlecreations[as_reg_date < as.Date('2017-07-01'),
                 list(n_creations=sum(.N)), by=as_reg_date],
            file='datasets/article_creations_by_day.tsv',
            quote=FALSE, sep='\t', row.names=FALSE);
