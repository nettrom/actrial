## Investigation into the quality of articles created by newly created accounts
## (within the first 30 days after registration).

library(data.table);
library(ggplot2);

creations_30days = fread('datasets/article_creations_30days.tsv');
qual_preds = fread('datasets/article_quality_predictions_2009-2017.tsv');

creations_30days_recent = fread('datasets/article_creations_30days_recent.tsv');
qual_preds_recent = fread('datasets/article_quality_predictions_2017.tsv');

## Set keys to revision ID and do a left join.
setkey(creations_30days, revision_id);
setkey(qual_preds, rev_id);

creations_30days = creations_30days[qual_preds, nomatch = 0];

setkey(creations_30days_recent, revision_id);
setkey(qual_preds_recent, rev_id);

creations_30days_recent = creations_30days_recent[qual_preds_recent, nomatch = 0];

## Total creations: 1,353,535 (wc -l)
## Creations that match a quality prediction: 524,271
## That suggests that 829,264 articles (61.27%) were deleted in such a way that
## we no longer can retrieve the revision and make a prediction (copyright, attack)

## Total recent creations: 18662
## Creations that match a quality prediction: 8011
## That suggests that 57.1% were deleted in such a way that we no longer
## can retrieve them.

## That's concerning. I should plot that over time. Also, how does it develop
## since the start of ACTRIAL?

all_30day_creations = fread('datasets/article_creations_30days.tsv');

## Parse the timestamps and split into date/time.
all_30day_creations[, c("event_date", "event_time") := IDateTime(as.POSIXct(event_timestamp,
                                                                            format='%Y-%m-%d %H:%M:%S.0',
                                                                            tz='UTC'))];
creations_30days[, c("event_date", "event_time") := IDateTime(
  as.POSIXct(rev_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];

surviving_article_data = merge(
  all_30day_creations[, list(num_creations=sum(.N)), by=event_date],
  creations_30days[, list(num_surviving_articles=sum(.N)), by=event_date],
  by='event_date');
surviving_article_data[, prop_surviving := 100*num_surviving_articles/num_creations]

ggplot(surviving_article_data,
       aes(x=event_date, y=prop_surviving)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of created articles retrievable in late 2017") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 65)

all_recent_30day_creations = fread('datasets/article_creations_30days_recent.tsv');
all_recent_30day_creations[, c("event_date", "event_time") := IDateTime(as.POSIXct(event_timestamp,
                                                                            format='%Y-%m-%d %H:%M:%S',
                                                                            tz='UTC'))];
creations_30days_recent[, c("event_date", "event_time") := IDateTime(
  as.POSIXct(event_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];
surviving_article_data_recent = merge(
  all_recent_30day_creations[, list(num_creations=sum(.N)), by=event_date],
  creations_30days_recent[, list(num_surviving_articles=sum(.N)), by=event_date],
  by='event_date');
surviving_article_data_recent[, prop_surviving := 100*num_surviving_articles/num_creations]

ggplot(surviving_article_data_recent,
       aes(x=event_date, y=prop_surviving)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of articles created in 2017 retrievable in early 2018") +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  geom_smooth(method='loess', span=0.2) + ylim(0, 100)

## Q1: What is the average quality score per day across time?
## We use Halfaker's measure: sum(I(c)*P(c)) where 'c' is the class,
## I(c) is 0 for "Stub", "1" for Start, and so on up to "5" for FA.
creations_30days[, qual_sum := start_prob + 2*c_prob + 3*b_prob +
                   4*ga_prob + 5*fa_prob];

ggplot(creations_30days[, list(avg_weigh_sum=mean(qual_sum)), by=event_date],
       aes(x=event_date, y=avg_weigh_sum)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle("Average quality weighed sum per day, all retrievable revisions") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 1.5);

creations_30days_recent[, qual_sum := start_prob + 2*c_prob + 3*b_prob +
                          4*ga_prob + 5*fa_prob];
ggplot(creations_30days_recent[, list(avg_weigh_sum=mean(qual_sum)), by=event_date],
       aes(x=event_date, y=avg_weigh_sum)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle("Average quality weighed sum per day, all retrievable revisions") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 1.5);

## Q2: What proportion of articles are flagged as not-OK by the draft quality model?
draft_pred_ok = merge(
  creations_30days[, list(num_creations=sum(.N)), by=event_date],
  creations_30days[draft_prediction == 'OK' & ok_prob >= 0.664,
                   list(num_ok=sum(.N)), by=event_date],
  by='event_date');

ggplot(draft_pred_ok,
       aes(x=event_date, y=100*num_ok/num_creations)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion (%)') +
  ggtitle("Proportion of articles not flagged by the draft quality model") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 90);

draft_pred_ok_recent = merge(
  creations_30days_recent[, list(num_creations=sum(.N)), by=event_date],
  creations_30days_recent[draft_prediction == 'OK' & ok_prob >= 0.664,
                   list(num_ok=sum(.N)), by=event_date],
  by='event_date');

ggplot(draft_pred_ok_recent,
       aes(x=event_date, y=100*num_ok/num_creations)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Proportion (%)') +
  ggtitle("Proportion of non-flagged articles created in 2017") +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  geom_smooth(method='loess', span=0.2) + ylim(0, 100);

## Q3: What proportion of articles are flagged as spam/vandalism/attack.
draft_pred_split = merge(
  creations_30days[, list(num_creations=sum(.N)), by=event_date],
  merge(
    creations_30days[draft_prediction == 'spam', list(num_spam=sum(.N)), by=event_date],
    merge(
      creations_30days[draft_prediction == 'vandalism', list(num_vandalism=sum(.N)), by=event_date],
      creations_30days[draft_prediction == 'attack', list(num_attack=sum(.N)), by=event_date],
      by='event_date'),
    by='event_date'),
  by='event_date');

ggplot() +
  geom_line(data=draft_pred_split,
            aes(x=event_date, y=100*num_spam/num_creations, colour='Spam')) +
  geom_line(data=draft_pred_split,
            aes(x=event_date, y=100*num_vandalism/num_creations, colour='Vandalism')) +
  geom_line(data=draft_pred_split,
            aes(x=event_date, y=100*num_attack/num_creations, colour='Attack')) +
  geom_smooth(data=draft_pred_split,
              aes(x=event_date, y=100*num_spam/num_creations, colour='Spam'),
              method='loess', span=0.2) +
  geom_smooth(data=draft_pred_split,
              aes(x=event_date, y=100*num_vandalism/num_creations, colour='Vandalism'),
              method='loess', span=0.2) +
  geom_smooth(data=draft_pred_split,
              aes(x=event_date, y=100*num_attack/num_creations, colour='Attack'),
              method='loess', span=0.2) +
  ylim(0,45) + xlab('Date') + ylab('Proportion (%)') +
  ggtitle("Proportion of new articles flagged by the draft quality model") +
  scale_color_manual(values=c(
    Attack=cbbPalette[1], Spam=cbbPalette[2], Vandalism=cbbPalette[3])) +
  guides(colour=guide_legend(title='Flagged as'))

## There's a lot of variation in this data, can we do it by month instead?

## Revisiting Q1, isn't it more interesting to look at average quality for articles
## that were _not_ flagged by the draft quality model? Arguably, if the article
## is flagged, we suspect it'll get deleted or otherwise moved out of Main?

ggplot(creations_30days[draft_prediction == 'OK' & ok_prob >= 0.664,
                        list(avg_weigh_sum=mean(qual_sum)), by=event_date],
       aes(x=event_date, y=avg_weigh_sum)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle("Average quality weighed sum per day, non-flagged articles") +
  geom_smooth(method='loess', span=0.2) + ylim(0, 1.5);

ggplot(creations_30days_recent[draft_prediction == 'OK' & ok_prob >= 0.664,
                        list(avg_weigh_sum=mean(qual_sum)), by=event_date],
       aes(x=event_date, y=avg_weigh_sum)) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle("Avg quality weighed sum per day, non-flagged 2017 articles") +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  geom_smooth(method='loess', span=0.2) + ylim(0, 1.5);

## Question: is the quality of content created by newcomers slowly increasing,
## or is the bar for not getting your article deleted increasing?

## Question: should we investigate average probability of an article not being
## flagged, given that the flags are not exclusive (e.g. an article can be 25%
## spam and 28% vandalism)?

## Q4: linear model for weighed sum of quality predicted by age and number of edits:

qualmodel = lm(qual_sum ~ event_user_age + event_user_revision_count,
               data=creations_30days[draft_prediction == 'OK' & ok_prob >= 0.664]);
summary(qualmodel);

qualmodel.log = lm(qual_sum ~ log2(1 + event_user_age)
                   + log2(1 + event_user_revision_count),
                   data=creations_30days[draft_prediction == 'OK' & ok_prob >= 0.664]);
summary(qualmodel.log);

## Let's redo the same analysis for more recent creations!

