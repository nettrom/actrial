## Preliminary analysis of unreviewed articles.

library(data.table);
library(ggplot2);

## Read in the dataset
unreviewed_articles = data.table(
  read.table("datasets/unreviewed_articles.tsv",
  header=TRUE, stringsAsFactors=FALSE, sep='\t',
  colClasses = c('numeric', 'character', 'character', 'numeric', 'numeric')));

unreviewed_articles[, log_review_time := as.POSIXct(log_review_timestamp, format='%Y%m%d%H%M%S', tz="UTC")];
unreviewed_articles[, log_unreview_time := as.POSIXct(log_unreview_timestamp, format='%Y%m%d%H%M%S', tz="UTC")];
unreviewed_articles[, log_review_date := as.Date(log_review_time)];

## Calculate unreview delta
unreviewed_articles[, log_unreview_delta := log_unreview_time - log_review_time];

## What proportion of unreviews are self-unreviews:
length(unreviewed_articles[log_review_user == unreview_user]$log_page);
length(unreviewed_articles[log_review_user == unreview_user]$log_page)/length(unreviewed_articles$log_page);
## 91.35%

## What proportion of the self-unreviews happen within 15 minutes?
length(unreviewed_articles[
  log_review_user == unreview_user & log_unreview_delta < 15*60]$log_page)/length(unreviewed_articles[log_review_user == unreview_user]$log_page);
## 89.6%

## What proportion of unreviews by others happens in less than a day, or a week?
length(unreviewed_articles[
  log_review_user != unreview_user & log_unreview_delta < 60*60*24]$log_page)/length(unreviewed_articles[log_review_user != unreview_user]$log_page);
length(unreviewed_articles[
  log_review_user != unreview_user & log_unreview_delta < 60*60*24*7]$log_page)/length(unreviewed_articles[log_review_user != unreview_user]$log_page);

## Time to unreview for all articles
ggplot(unreviewed_articles, aes(as.numeric(log_unreview_delta))) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Distribution of time between review and unreview") +
  scale_x_log10(
    "Time to unreview",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60, 356*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month", "year"));

## Time to unreview where the unreview is done by the same user
ggplot(unreviewed_articles[log_review_user == unreview_user],
       aes(as.numeric(log_unreview_delta))) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Distribution of time between review and unreview") +
  scale_x_log10(
    "Time to unreview",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60, 356*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month", "year"));

## Time to unreview where the unreview is _not_ done by the same user
ggplot(unreviewed_articles[log_review_user != unreview_user],
       aes(as.numeric(log_unreview_delta))) + 
  geom_histogram(binwidth = 0.10, colour="black", fill='white') +
  ggtitle("Distribution of time between review and unreview") +
  scale_x_log10(
    "Time to unreview",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60, 356*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month", "year"));

## Number of unreviews by date
num_unreviews = unreviewed_articles[
  , list(n_unreviews = sum(.N)), by=log_review_date];

num_unreviews_other = unreviewed_articles[log_review_user != unreview_user,
                                          list(n_unreviews = sum(.N)), by=log_review_date];


ggplot(num_unreviews, aes(x=log_review_date, y=n_unreviews)) + geom_line();

## How many are there on average in a day?
sum(num_unreviews$n_unreviews)/length(num_unreviews$log_review_date);

## To what extent does a user unreview then re-review the same page in succession?
## 1,509 review->unreview->review triplets, where there's less than an hour
## between them.
## There's 11,015 self-unreviews, so 13.7% of these result in a "self-re-review".

## Generate a date sequence and join with num_unreviews_other so that we can
## fill in the missing data:
num_unreviews_other_dates = data.table(
  log_review_date=seq.Date(min(num_unreviews_other$log_review_date),
                           to=max(num_unreviews_other$log_review_date),
                           by=1));
num_unreviews_other = merge(num_unreviews_other_dates,
                            num_unreviews_other, 
                            by='log_review_date', all.x=TRUE);
num_unreviews_other[is.na(n_unreviews), n_unreviews := 0];

## Now this plot should have 0s where appropriate:
ggplot(num_unreviews_other, aes(x=log_review_date, y=n_unreviews)) + geom_line() +
  ggtitle("Unreviews by other users per day") +
  xlab('Date of review') + ylab('Number of unreviews by other users') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 10.5), breaks=seq(0,11, 1));

