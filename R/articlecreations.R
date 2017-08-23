## Preliminary analysis of article creations

library(ggplot2);
library(data.table);

## Read in the dataset
articlecreations = fread('datasets/article_creations_by_day.tsv');
moves = fread('datasets/moves_by_day.tsv');

creations_and_moves = rbind(articlecreations[as_reg_date >= "2011-01-01",
                                             list(c_date=as_reg_date,
                                                  n_articles=n_creations,
                                                  type="direct")],
                            moves[, list(c_date=mim_date,
                                         n_articles=mim_num_moves_user,
                                         type="from_user")],
                            moves[, list(c_date=mim_date,
                                         n_articles=mim_num_moves_draft,
                                         type="from_draft")]);

creations_and_moves[, c_date := as.IDate(c_date)];

# articlecreations[, c("as_reg_date", "as_reg_time") := IDateTime(strptime(event_timestamp, format='%Y-%m-%d %H:%M:%S.0', tz="UTC"))];
# ggplot(articlecreations[as_reg_date < as.Date('2017-07-01'),
#                         list(n_creations=sum(.N)), by=as_reg_date],
#        aes(x=as_reg_date, y=n_creations)) + geom_line() +
#   scale_x_date(date_breaks='1 years', date_labels = '%Y') +
#   geom_smooth() +
#   xlab('Year') + ylab('Number of articles created') +
#   ggtitle('Number of articles created by day');
# 
# write.table(articlecreations[as_reg_date < as.Date('2017-07-01'),
#                  list(n_creations=sum(.N)), by=as_reg_date],
#             file='datasets/article_creations_by_day.tsv',
#             quote=FALSE, sep='\t', row.names=FALSE);

ggplot(creations_and_moves, aes(x=c_date, y=n_articles,
                                group=type, colour=type)) + geom_line() +
  ggtitle('Articles created per day') +
  xlab('Year') + ylab('Number of articles') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette);

## Let's focus on drafts
ggplot(creations_and_moves[type != 'direct'], aes(x=c_date, y=n_articles,
                                group=type, colour=type)) + geom_line() +
  ggtitle('Articles created per day') +
  xlab('Year') + ylab('Number of articles') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette[2:length(cbbPalette)]);

## What's going on with the draft space in 2017?
ggplot(creations_and_moves[type == 'from_draft' & c_date >= as.IDate('2017-01-01')],
       aes(x=c_date, y=n_articles)) + geom_line(colour=cbbPalette[2]) +
  ggtitle('Moves from draft space into main per day') +
  xlab('Year') + ylab('Number of articles') +
  scale_x_date(date_breaks='1 month', date_labels="%Y-%m");


## What's the average per day since the start of 2017?
creations_and_moves[, list(avg_per_day=mean(n_articles)), by=type];
