## Investigation into time to review for articles that were created by
## accounts that did not have autopatrolled rights.

library(data.table);
library(ggplot2);

## Load in the dataset
review_data = data.table(
  read.table('datasets/enwiki_creations_non_autop_reviewed.tsv.bz2',
             stringsAsFactors = FALSE, sep='\t', header = TRUE,
             colClasses = c('numeric', 'character', 'character', 'numeric')));

## Convert creation timestamp to a date and time, and clean up the review
## timestamp for all articles that were not reviewed.
review_data[time_to_review == 0, review_timestamp := ''];
review_data[
  , c("creation_date", "creation_time") := IDateTime(strptime(ac_timestamp, format='%Y%m%d%H%M%S', tz="UTC"))];

## How many reviews were done each day in this dataset? I'm concerned about
## us not having much data in the earlier years.
ggplot(review_data[time_to_review > 0, list(num_reviews=sum(.N)), by=creation_date],
       aes(x=creation_date, y=num_reviews)) + geom_line();

## Looks like pre-2010 is not useful at all. After performing some of the other
## analysis in the dataset, I find that our typical cutoff of Oct 1, 2012, based
## on the PageTriage extension being deployed, is most useful.
review_data = review_data[creation_date >= as.IDate('2012-10-01')];

## What proportion of articles do we have logged reviews for?
prop_reviewed = merge(
  review_data[time_to_review > 0, list(num_reviews=sum(.N)), by=creation_date],
  review_data[, list(num_created_articles=sum(.N)), by=creation_date],
  by='creation_date');
prop_reviewed[, prop_reviewed := num_reviews / num_created_articles];

ggplot(prop_reviewed, aes(x=creation_date, y=100*prop_reviewed)) + geom_line() +
  ggtitle("Proportion of articles that have been reviewed") +
  xlab('Date of article creation') + ylab('Proportion of created articles (in %)') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## The proportion is never 100%, could that be because we cannot filter out all
## redirect creations? And perhaps also that articles will get deleted without
## being reviewed? It suggests that we want more data cleaning if we want to use
## this for estimating the percentage of articles that get reviewed.

## How many articles in total have never been reviewed?
100*length(review_data[time_to_review == 0]$ac_page_id)/length(review_data$ac_page_id);

## On average, 20% of the articles don't get reviewed. It would be interesting
## to know if these are all articles that have been deleted.

## What's the overall distribution of time to review? (Ignoring articles
## that haven't been reviewed).
ggplot(review_data[time_to_review > 0], aes(1 + time_to_review)) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Distribution of time between creation and review") +
  scale_x_log10(
    "Time to review",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60, 356*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month", "year"));

## What proportion of articles are reviewed within the first week?
100*length(review_data[time_to_review < 7*24*60*60]$ac_page_id)/length(review_data$ac_page_id)

## How median time to review has developed over time (we ignore all non-reviewed
## pages for simplicity)
median_time_to_review = review_data[
  time_to_review > 0,
  list(median_time_to_review=median(time_to_review),
       perc_75=as.numeric(quantile(time_to_review, probs=c(0.75)))),
  by=creation_date];

ggplot(median_time_to_review, aes(x=creation_date, y=median_time_to_review)) +
  geom_line() +
  ggtitle("Median time between creation and review") +
  scale_y_log10(
    "Median time to review",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60, 356*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month", "year")) +
  xlab("Date of article creation");

ggplot(median_time_to_review, aes(x=creation_date, y=perc_75)) +
  geom_line() +
  scale_y_log10(
    "Time to review",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60, 356*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month", "year"));

## Median time to review for the first half of 2017? Let's take the median of the
## medians.
median(median_time_to_review[creation_date >= '2017-01-01']$median_time_to_review)/(60*60);

## The median time to review has been much faster in the more recent years,
## what does the histogram look for those years? 2015 onwards looks sensible.
ggplot(review_data[time_to_review > 0 & creation_date >= '2015-01-01'],
       aes(1 + time_to_review)) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Distribution of time between creation and review") +
  scale_x_log10(
    "Time to review",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60, 356*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month", "year"));

## What proportion of articles created since 2015 are reviewed within
## the first week?
100*length(review_data[creation_date >= '2015-01-01'
                       & time_to_review < 7*24*60*60]$ac_page_id)/length(review_data[creation_date >= '2015-01-01']$ac_page_id)
## 85.6%