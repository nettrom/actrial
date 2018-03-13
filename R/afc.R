## Investigating AfC submissions, how drafts fare, quality at AfC, and such.
## (H16, H17, H22)

library(data.table);
library(ggplot2);
library(forecast);
library(tseries);
library(zoo);
library(lubridate);

## Read in the datasets
drafts = fread('datasets/drafts_afc/drafts.tsv', na.strings = c('NULL'));
drafts_afc = fread('datasets/drafts_afc/drafts_afc.tsv', na.strings = c('NULL'));
drafts_predictions = fread('datasets/drafts_afc/drafts_predictions.tsv', na.strings = c('NULL'));

## Parse timestamps in drafts dataset
drafts[, c("creation_date", "creation_time") := IDateTime(
  as.POSIXct(creation_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];
drafts[, c("publication_date", "publication_time") := IDateTime(
  as.POSIXct(publication_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];
drafts[, c("deletion_date", "deletion_time") := IDateTime(
  as.POSIXct(deletion_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];

## Limit the dataset to 2017-12-01, two-ish months into ACTRIAL.
## The dataset is currently limited to starting on 2014-07-01, as that is when
## we know we have trustworthy deletion data.
drafts = drafts[creation_date < '2017-12-01'];

## Some overviews.
## 1: Number of drafts created per day.
ggplot(drafts[, list(n_drafts=sum(.N)), by=creation_date],
       aes(x=creation_date, y=n_drafts)) +
  geom_line() +
  xlab('Year') + ylab('Number of drafts') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 350)) +
  geom_smooth(method='loess', span=0.2) +
  ggtitle("Drafts created per day, 2009-2017");

## 2: Based on the graph, we are mainly interested in data from mid-2014 onwards.
## Partly because it starts being used (the RfC passed in Nov 2013), and partly
## because we can then trust deletion by page ID to work.
ggplot(drafts[creation_date >= '2014-07-01',
              list(n_drafts=sum(.N)), by=creation_date],
       aes(x=creation_date, y=n_drafts)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  xlab('Year') + ylab('Number of drafts') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 350)) +
  geom_smooth(method='loess', span=0.2) +
  ggtitle("Drafts created per day, mid-2014 to 2017");

## Things to note: the steady usage through 2015, then increased usage in
## spring and fall of 2016 and spring of 2017. Did Education Program switch
## to mainly creating in Draft namespace? Lastly, the kick when ACTRIAL started.

## 3: How many drafts get published?
## Overall:
length(drafts[creation_date >= '2014-07-01' & creation_date < '2017-12-01']$page_id);

length(drafts[creation_date >= '2014-07-01' & creation_date < '2017-12-01' &
                !is.na(publication_date)]$page_id);

## 1436 of 123243 moved into Main. That's a publication rate of 1.17%. Is that right?

## I can believe that to be right. The following SQL statement found 964 pages
## as of Feb 1, 2018:
## SELECT count(*) FROM nettrom_drafts JOIN enwiki.page USING (page_id)
## WHERE creation_timestamp >= '2014-07-01 00:00:00' AND page_namespace=0;

## Do a similar analysis of the rate of submissions to AfC from July 2014 onwards.
## Note that this does to some extent contain duplicates as some pages will contain
## an AfC template that's not submitted.
drafts_afc[, submission_timestamp := submission_time];
drafts_afc[, review_timestamp := review_time];

## Moved up from the analysis further down.
## Four pages with negative review times (for five reviews):
## (43265175, 47583016, 51010783, 53385611);

## Revision IDs: (725853418, 686508251, 728756022, 773755924, 773756609)
## They're all unique, so we can fix it that way. 686508251 gets year set to
## 2015, the others have submission time moved by 12 hours.
drafts_afc[rev_id == 686508251, submission_timestamp := '2015-10-19 03:12:20'];
drafts_afc[rev_id == 725853418, submission_timestamp := '2016-06-18 06:00:00'];
drafts_afc[rev_id == 728756022, submission_timestamp := '2016-07-07 10:00:00'];
drafts_afc[rev_id == 773755924, submission_timestamp := '2017-04-04 05:02:53'];
drafts_afc[rev_id == 773756609, submission_timestamp := '2017-04-04 05:09:53'];

drafts_afc[, c("submission_date", "submission_time") := IDateTime(
  as.POSIXct(submission_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];
drafts_afc[, c("review_date", "review_time") := IDateTime(
  as.POSIXct(review_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];

ggplot(drafts_afc[submission_status != "T" &
  submission_date >= '2014-07-01' & submission_date < '2017-12-01',
              list(n_afcs=sum(.N)), by=submission_date],
       aes(x=submission_date, y=n_afcs)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  xlab('Year') + ylab('Number of submissions') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 350)) +
  geom_smooth(method='loess', span=0.2) +
  ggtitle("AfC submissions per day, mid-2014 to 2017");

## NOTE: I disregard the daily forecast, that is outside the scope of the project.
## If we want higher granularity, we'll go for a bimonthly forecast.
## Make a timeseries on the number of submissions and forecast for the first four
## weeks of ACTRIAL.
# drafts_afc_per_day_ts = ts(
#   drafts_afc_per_day[submission_date < '2017-09-15']$n_afcs, frequency=7);
# 
# decomp_add = stl(drafts_afc_per_day_ts, s.window='periodic');
# autoplot(decomp_add);

## Note that this suggests the time series is stationary.
## Augmented Dickey-Fuller test for stationarity:
# adf.test(drafts_afc_per_day_ts, alternative='stationary')
## This suggests that it's stationary, since the null-hypothesis (non-stationary)
## is rejected. Question is then what kind of ARMA model is appropriate.

# ggAcf(drafts_afc_per_day_ts) +
#   ggtitle('ACF for AfCs per day');
# ggPacf(drafts_afc_per_day_ts) +
#   ggtitle('PACF for AfCs per day');

## The ACF suggests testing MA 1, 6, 7, and 8.
## The PACF suggests testing AR 1, 6, and 7.

# auto.arima(drafts_afc_per_day_ts, stationary=FALSE, max.p=9,  max.q=9);
# auto.arima(drafts_afc_per_day_ts, stationary=TRUE, max.p=9,  max.q=9);
# auto.arima(drafts_afc_per_day_ts, stationary=TRUE, seasonal=TRUE, max.p=9,  max.q=9);
# auto.arima(drafts_afc_per_day_ts, stationary=TRUE, seasonal=FALSE, max.p=9,  max.q=9);
# 
# auto.arima(drafts_afc_per_day_ts, stationary=TRUE);
# auto.arima(drafts_afc_per_day_ts, stationary=TRUE, ic='bic');
# 
# auto.arima(drafts_afc_per_day_ts, stationary = TRUE,
#            stepwise = FALSE, approximation = FALSE, parallel = TRUE);
# auto.arima(drafts_afc_per_day_ts, stationary = TRUE, ic='bic',
#            stepwise = FALSE, approximation = FALSE, parallel = TRUE);
# auto.arima(drafts_afc_per_day_ts, stationary=TRUE, seasonal = FALSE,
#            stepwise = FALSE, approximation = FALSE, parallel = TRUE);

## AICc and BIC appear to agree here. The seasonal model is simpler and has
## much lower AIcc and BIC, although we sacrifice some log-likelihood. Let's go
## with that.

# afc_by_day_model = auto.arima(drafts_afc_per_day_ts,
#                               stepwise = FALSE, approximation = FALSE, parallel = TRUE);
# afc_by_day_fc = forecast(afc_by_day_model, h=28);
# 
# autoplot(afc_by_day_fc) +
#   geom_line(data=data.frame(x=168.5+c(1:30)/7,
#                             y=drafts_afc_per_day[submission_date >= '2017-09-15' & submission_date < '2017-10-15']$n_afcs),
#             aes(x=x, y=y, colour='actual')) +
#   xlab('Weeks since July 1, 2014') + ylab('Number of AfCs submitted') +
#   scale_color_manual(values=c(actual="red")) +
#   guides(level=guide_legend(title="CI"),
#          colour=guide_legend(title="")) +
#   ggtitle("AfC submission forecast for ACTRIAL");

## Some of the traffic is within the forecast. Looking more closely at the data,
## it seems clear that weekend traffic is down towards previous levels, meaning
## it ends up being close to expected. Secondly, there's an upwards trend in traffic
## in August/September, a seasonal trend that the model captures. In combination
## with the expanding confidence intervals, it makes the result less clear.
## It appears clearer that ACTRIAL has had an effect if we look from early
## October onwards.

## H17: The AfC backlog will increase faster than expected.

## First, give me an idea about how quickly things that have been reviewed have
## been reviewed, in general.
drafts_afc[
  , time_to_review := as.POSIXct(review_timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC") -
    as.POSIXct(submission_timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")];

## How many submissions have a status other than "T" and have been reviewed?
length(drafts_afc[submission_date >= '2014-07-01' & 
                    submission_date < '2017-11-15' &
                    submission_status != "T"]$page_id);
length(drafts_afc[submission_date >= '2014-07-01' & 
                    submission_date < '2017-11-15' &
                    submission_status != "T" & !is.na(review_timestamp)]$page_id);

## Percentage:
100*length(drafts_afc[submission_date >= '2014-07-01' & 
                        submission_date < '2017-11-15' &
                        submission_status != "T" & !is.na(review_timestamp)]$page_id)/
  length(drafts_afc[submission_date >= '2014-07-01' & 
                      submission_date < '2017-11-15' &
                      submission_status != "T"]$page_id);

## There are two paths to "review":
## 1: The submission is declined, meaning it has a review timestamp.
## 2: The page gets published. 

## This means I'll need to join drafts_afc and drafts so that I get publication
## timestamps added for published pages.

drafts_afc_merged = merge(drafts_afc, drafts, by='page_id', all.x=TRUE);

## We've already calculated review time where there is a declined timestamp.
## Set review time to publication time where there isn't a review timestamp.
drafts_afc_merged[
  is.na(review_time) & !is.na(publication_time),
  review_timestamp := publication_timestamp]
drafts_afc_merged[, c("review_date", "review_time") := IDateTime(
  as.POSIXct(review_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];
drafts_afc_merged[
  , time_to_review := as.POSIXct(review_timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC") -
    as.POSIXct(submission_timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")];

## Also calculate time to review in days, because that's a useful meetric:
drafts_afc_merged[, time_to_review_days := as.numeric(time_to_review)/(60*60*24)];

## Histogram of time to review:
ggplot(drafts_afc_merged[submission_date >= '2014-07-01' & 
                    submission_date < '2017-11-15' &
                    submission_status != "T" & !is.na(review_timestamp) &
                    as.numeric(time_to_review) > 0],
       aes(1 + as.numeric(time_to_review)/60)) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Distribution of time to review at AfC") +
  scale_x_log10(
    "Time to review",
    breaks=c(1, 60, 24*60, 7*24*60, 30*24*60, 180*24*30, 356*24*60),
    labels=c("minute", "hour", "day", "week", "month", "6 months", "year"));

# ## Using week-long buckets
# ggplot(drafts_afc_merged[submission_date >= '2014-07-01' & 
#                     submission_date < '2017-11-15' &
#                     submission_status != "T" & !is.na(review_timestamp)],
#        aes(time_to_review_days)) + 
#   geom_histogram(binwidth = 7, colour="black", fill='white') +
#   ggtitle("Distribution of time to review at AfC");
# 
# ## Buckets of 30-day months
# ggplot(drafts_afc_merged[submission_date >= '2014-07-01' & 
#                     submission_date < '2017-11-15' &
#                     submission_status != "T" & !is.na(review_timestamp)],
#        aes(time_to_review_days)) + 
#   geom_histogram(binwidth = 30, colour="black", fill='white') +
#   scale_x_continuous(breaks=c(180,360,540,720,900,1080,1260)) +
#   xlab('Time to review in days') + ylab('Number of submissions') +
#   ggtitle("Distribution of time to review Drafts at AfC, 30-day buckets");

## The question is, has this significantly changed during ACTRIAL?
ggplot(drafts_afc_merged[submission_date >= '2017-09-15' &
                           submission_date < '2017-11-15' &
                           submission_status != "T" & !is.na(review_timestamp) &
                           as.numeric(time_to_review) > 0],
       aes(1 + as.numeric(time_to_review)/60)) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Distribution of time to review at AfC during ACTRIAL") +
  scale_x_log10(
    "Time to review",
    breaks=c(1, 60, 24*60, 7*24*60, 30*24*60, 180*24*30, 356*24*60),
    labels=c("minute", "hour", "day", "week", "month", "6 months", "year"));

## Of submissions during the same period of the year in 2014-2016, what proportion
## got reviewed within 7 days, and how does that compare to ACTRIAL?

100*length(drafts_afc_merged[
    ((submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
       (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
       (submission_date >= '2016-09-15' & submission_date < '2016-11-15')) &
      submission_status != 'T' &
      !is.na(review_timestamp) &
      time_to_review_days < 7]$rev_id)/
  length(drafts_afc_merged[
    ((submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
       (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
       (submission_date >= '2016-09-15' & submission_date < '2016-11-15')) &
      submission_status != 'T']$rev_id);

100*length(drafts_afc_merged[
  (submission_date >= '2017-09-15' & submission_date < '2017-11-15') &
    submission_status != 'T' &
    !is.na(review_timestamp) &
    time_to_review_days < 7]$rev_id)/
  length(drafts_afc_merged[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15') &
      submission_status != 'T']$rev_id);

## H17: the AfC backlog…
## Calculate number of submissions and reviews per day, then diff them.

# drafts_backlog_data = merge(
#   drafts_afc_merged[
#     creation_date >= '2014-07-01' & creation_date < '2017-11-15' &
#       submission_date >= '2014-07-01' & submission_date < '2017-11-15' &
#       review_date >= '2014-07-01' & review_date < '2017-11-15',
#     list(n_reviews=sum(.N)), by=review_date],
#   drafts_afc_merged[
#     creation_date >= '2014-07-01' & creation_date < '2017-11-15' &
#       submission_date >= '2014-07-01' & submission_date < '2017-11-15' &
#       submission_status != "T",
#     list(n_submissions=sum(.N)), by=submission_date],
#   by.x='review_date', by.y='submission_date');

## Deleted pages only reduce the backlog if they're unreviewed.
# drafts_backlog_data = merge(
#   drafts_backlog_data,
#   drafts_afc_merged[
#     creation_date >= '2014-07-01' & creation_date < '2017-11-15' &
#     deletion_date >= '2014-07-01' & deletion_date < '2017-11-15' &
#                       submission_status != 'T' & is.na(review_time),
#                     list(n_deletions = length(unique(page_id))), by=deletion_date],
#   by.x='review_date', by.y='deletion_date'
# );
# 
# drafts_backlog_data[, delta := n_submissions - n_reviews - n_deletions];
# drafts_backlog_data[
#   , backlog := cumsum(delta)];
# 
# ggplot(drafts_backlog_data, aes(x=review_date, y=backlog)) +
#   geom_line();
# 
# drafts_afc_merged[deletion_date >= '2014-07-01' & deletion_date < '2017-11-15' &
#                     submission_status != 'T' & is.na(review_time)]
# 
# drafts_afc_merged[deletion_date >= '2014-07-01' & deletion_date < '2017-11-15' &
#                     submission_status != 'T' & is.na(review_time),
#                   list(n_deletions = length(unique(page_id))), by=deletion_date]
# 
# head(drafts_afc_merged[
#   review_date >= '2014-07-01' & review_date < '2017-11-15',
#   list(n_reviews=sum(.N)), by=review_date][order(review_date)][review_date >= '2017-01-01'], n=25)
# 
# drafts_afc_merged[creation_date >= '2014-07-01' & creation_date < '2017-11-15' &
#                     review_date == '2017-01-14']
# 
# head(drafts_backlog_data[review_date >= '2017-01-01'], 31);

## Proposed improvement:
## We measure the size of the backlog at midnight each day. Each submission
## contributes one unit to the backlog every day after it is submitted until
## it is reviewed. This means we can create a one-column data.table of dates where
## the pages contributed, and then collapse and count each day.

toy_dataset = head(drafts_afc_merged, 100);

# A submission either has a review timestamp, a publication timestamp,
## or a deletion timestamp. Choose the earliest of those.

toy_dataset[, list(submission_timestamp, review_timestamp, publication_timestamp,
                   deletion_timestamp)];

process_submissions = function(afc_dataset) {
  ## For each page in the dataset, process a review queue that any AfC submission
  ## adds to, and that any AfC review clears. If the review happens 
  
  ## Q: How do we handle duplicate submissions of the same page? Note, it's
  ## the submission that gets reviewed, but the page that gets deleted. A page
  ## can't contribute to the backlog if it's already in the backlog. Can't we
  ## solve this by adding dates for every submission of a page, then removing
  ## duplicates? That means we're interating per page, per row... uffda!
  
  total_pages = length(unique(afc_dataset$page_id));
  num_pages_processed = 0;

  ## page_id -> data.table with a page's AfC submissions
  all_pages = vector('list', total_pages);
  
  for(page in unique(afc_dataset$page_id)) {
    ## Data we've discovered for this page
    submission_timestamps = c();
    review_timestamps = c();
    review_actions = c();
    
    ## This page's queued submissions
    page_queue = list();
    
    ## Order the rows by submission timestamp.
    for(i in 1:nrow(afc_dataset[page_id == page][order(submission_timestamp)])) {
      if(afc_dataset[page_id == page][i,]$submission_status == 'T') {
        next; ## not submitted for review
      }

      ## A submission without review pushes onto the stack.
      if(afc_dataset[page_id == page][i,]$submission_status != 'D') {
        page_queue[[length(page_queue) +1]] = afc_dataset[page_id == page][i,]$submission_timestamp;
      } else if(!is.na(afc_dataset[page_id == page][i,]$review_timestamp)) {
        ## A decline empties the stack. Use the stack's first date if set,
        ## otherwise use the submission date in the row. Checking for review date
        ## being NA due to problematic data.
        if(length(page_queue) > 0) {
          submission_timestamp = page_queue[[1]];
        } else {
          submission_timestamp = afc_dataset[page_id == page][i,]$submission_timestamp;
        }
        # print(paste('page', page, 'submitted on', submission_date, 'reviewed on',
        #             afc_dataset[page_id == page][i,]$review_date));
        submission_timestamps = append(submission_timestamps, submission_timestamp);
        review_timestamps = append(review_timestamps,
                                   afc_dataset[page_id == page][i,]$review_timestamp);
        review_actions = append(review_actions, 'declined');
        page_queue = list(); # empty the stack
      }
    }

    ## If the page was published, that empties the stack and adds dates up
    ## until publication. If the page was published but the stack is empty,
    ## print a warning (might just be pages that were ready for publication).
    if(!is.na(afc_dataset[page_id == page][i,]$publication_timestamp)) {
      if(length(page_queue) == 0) {
        print(paste('page', page, 'published with an empty review queue'));
      } else {
        submission_timestamps = append(submission_timestamps, page_queue[[1]]);
        review_timestamps = append(review_timestamps,
                                   afc_dataset[page_id == page][i,]$publication_timestamp);
        review_actions = append(review_actions, 'published');
        page_queue = list();
      }
    }
    
    ## If the page queue is non-empty at the end, print a message and use
    ## the deletion date as the date of review.
    if(length(page_queue) > 0) {
      # This is so common that it's not useful to print them.
      # print(paste('page', page, 'completed with non-empty review queue'));
      submission_timestamps = append(submission_timestamps, page_queue[[1]]);
      review_timestamps = append(review_timestamps,
                                 afc_dataset[page_id == page][i,]$deletion_timestamp);
      review_actions = append(review_actions, 'deleted');
    }
    
    ## Add the dates this page contributed to the backlog:
    if(length(submission_timestamps) > 0) {
      all_pages[[as.character(page)]] = data.table(
        page_id=rep(page, length(submission_timestamps)),
        submission_timestamp=submission_timestamps,
        review_timestamp=review_timestamps,
        review_action=review_actions
      );
    }
    num_pages_processed = num_pages_processed + 1;
    prop_processed = 100*num_pages_processed/total_pages;
    if(num_pages_processed %% 1000 == 0) {
      print(paste0("progress: ", round(prop_processed, 1), "%"));
    }
  }
  rbindlist(all_pages);
}

## Limiting the backlog analysis to pages created from July 1, 2014 onwards.
drafts_afc_reviews = process_submissions(drafts_afc_merged[
  creation_date >= '2014-07-01']);

## H16: The rate of new submissions at AfC will increase.
## We limit this to counting the number of declined submissions per day, two
## months through ACTRIAL.

## Parse the timestamps, turn them into POSIXct
drafts_afc_reviews[
  , rtimestamp := as.POSIXct(review_timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")];
drafts_afc_reviews[
  , stimestamp := as.POSIXct(submission_timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")];
drafts_afc_reviews[, time_to_review := rtimestamp - stimestamp];
drafts_afc_reviews[, submission_date := date(stimestamp)];

drafts_afc_per_day = drafts_afc_reviews[
  submission_date >= '2014-07-01' & submission_date < '2017-12-01' &
    time_to_review > 0,
  list(n_afcs=sum(.N)), by=submission_date][order(submission_date)];

## afc_submissions_per_day_2014-2017.png
ggplot(drafts_afc_per_day[submission_date < '2017-12-01'],
       aes(x=submission_date, y=n_afcs)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  xlab('Date') + ylab('Number of submissions') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 250)) +
  geom_smooth(method='loess', span=0.2) +
  ggtitle("AfC submissions per day, mid-2014 to 2017");

# drafts_afc_pre_avg = mean(drafts_afc_per_day[
#   year(submission_date) %in% c(2014,2015,2016) &
#     strftime(submission_date, "%m%d") >= '0915' &
#     strftime(submission_date, "%m%d") < '1115']$n_afcs);

## Let's examine the jump in submissions two ways. First, a t-test on the
## number of submissions per day. Then we'll throw an ARIMA model at it.

## Let's look at the distributions, are they roughly Normal?
qplot(
  drafts_afc_per_day[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$n_afcs,
  geom='histogram', binwidth=5);

qplot(
  drafts_afc_per_day[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$n_afcs,
  geom='histogram', binwidth=20);

summary(
  drafts_afc_per_day[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$n_afcs);
summary(
  drafts_afc_per_day[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$n_afcs);

## There's a bit of skewness. Since we're approaching this from multiple angles,
## we should be okay.
t.test(
  drafts_afc_per_day[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$n_afcs,
  drafts_afc_per_day[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$n_afcs
);

## Unsurprisingly, a mean of 66.2 pre-ACTRIAL vs 146.2 during ACTRIAL is
## statistically significant.

## Can also run a Mann-Whitney test:
wilcox.test(
  drafts_afc_per_day[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$n_afcs,
  drafts_afc_per_day[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$n_afcs
);

## Let's also investigate monthly
drafts_afc_per_day[
  , log_month := format(submission_date, "%Y%m")];
drafts_afc_per_month = drafts_afc_per_day[
  , list(n_afcs = sum(n_afcs)), by=c('log_month')];
drafts_afc_per_month[
  , log_date := as.Date(paste0(log_month, "01"), format="%Y%m%d")];

## AfC_submissions_per_month.png
ggplot(drafts_afc_per_month,
       aes(x=log_date, y=n_afcs)) +
  geom_line() +
  geom_smooth(method='loess', span=0.5) +
  xlab('Year') + ylab('Number of submissions') +
  scale_y_continuous(limits=c(0, 5000)) +
  ggtitle("AfC submissions per month");

drafts_afc_per_month_ts = ts(drafts_afc_per_month[log_date < '2017-09-01']$n_afcs,
                             start=c(2014,7), end=c(2017,8), frequency=12);
autoplot(drafts_afc_per_month_ts);

ggAcf(drafts_afc_per_month_ts) +
  ggtitle('ACF for AfCs per month');
ggPacf(drafts_afc_per_month_ts) +
  ggtitle('PACF for AfCs per month');

## Both suggest an ARIMA(1,0,1) model would be our first attempt. 6-month
## peak in the ACF, though, could point to seasonality. Let's look at the
## plot with and without seasonality:
tsdisplay(drafts_afc_per_month_ts);
tsdisplay(diff(drafts_afc_per_month_ts));
tsdisplay(diff(drafts_afc_per_month_ts, differences = 12));

## The time series is definitely not stationary:
adf.test(drafts_afc_per_month_ts, alternative='stationary')

## Are both seasonal and non-seasonal diffs stationary?
adf.test(diff(drafts_afc_per_month_ts));
adf.test(diff(drafts_afc_per_month_ts, differences = 12));

## No, only the seasonal one is stationary. Suggests a seasonal model.

drafts_afc_per_month_model.auto = auto.arima(
  drafts_afc_per_month_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(drafts_afc_per_month_model.auto);
summary(auto.arima(drafts_afc_per_month_ts, ic='bic',
                   stepwise = FALSE, approximation = FALSE, parallel = TRUE));

## Auto-selected model is an ARIMA(1,0,0)(1,1,0)[12] with drift, and BIC
## suggests the same model.

ggAcf(residuals(drafts_afc_per_month_model.auto)) +
  ggtitle('ACF for residuals AfCs per month, auto.arima model');
ggPacf(residuals(drafts_afc_per_month_model.auto)) +
  ggtitle('PACF for residuals AfCs per month, auto.arima model');
Box.test(residuals(drafts_afc_per_month_model.auto),
         lag=36, fitdf=2, type="Ljung");

## Passes the tests. Can we build an improved model?
drafts_afc_per_month_model = Arima(
  drafts_afc_per_month_ts,
  order = c(0,1,1),
  seasonal = list(order = c(0,1,1),
                  frequency = 12)
);
summary(drafts_afc_per_month_model)

ggAcf(residuals(drafts_afc_per_month_model)) +
  ggtitle('ACF for residuals AfCs per month, manual arima model');
ggPacf(residuals(drafts_afc_per_month_model)) +
  ggtitle('PACF for residuals AfCs per month, manual arima model');
Box.test(residuals(drafts_afc_per_month_model),
         lag=36, fitdf=3, type="Ljung");

## Integrating the non-seasonal component appears to make much more sense
## than allowing drift. Is there a preference for MA vs AR?
## ARIMA(1,1,0)(0,1,1)[12]: AIC=326.29   AICc=327.43   BIC=329.95
## ARIMA(0,1,1)(0,1,1)[12]: AIC=326.27   AICc=327.42   BIC=329.93
## ARIMA(0,1,1)(1,1,0)[12]: AIC=325.68   AICc=326.82   BIC=329.34
## ARIMA(1,1,0)(1,1,0)[12]: AIC=325.68   AICc=326.83   BIC=329.34

## RMSE suggests MA in the seasonal part. Looks like the ARIMA(0,1,1)(0,1,1)[12]
## model is a better fit than the ARIMA(1,1,0)(0,1,1)[12] model, so we'll use that.

## What does the forecast plot look like?
afc_by_month_fc = forecast(drafts_afc_per_month_model, h=3);

## ACTRIAL_AfC_submission_forecast.png
autoplot(afc_by_month_fc) +
  geom_line(data=data.frame(x=2017+c(8:10)/12,
                            y=drafts_afc_per_month[log_month %in% c('201709', '201710', '201711')]$n_afcs),
            aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 5000)) +
  xlab('Date') + ylab('Number of AfCs submitted') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("AfC submission forecast for ACTRIAL");

## Histogram of time to review:
ggplot(drafts_afc_reviews[submission_date >= '2014-07-01' & 
                           submission_date < '2017-11-15' &
                           review_action == 'declined' &
                           as.numeric(time_to_review) > 0],
       aes(1 + as.numeric(time_to_review)/60)) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Distribution of time to review at AfC") +
  scale_x_log10(
    "Time to review",
    breaks=c(1, 60, 24*60, 7*24*60, 30*24*60, 180*24*30, 356*24*60),
    labels=c("minute", "hour", "day", "week", "month", "6 months", "year"));

## What's the distribution of number of submissions?
qplot(drafts_afc_reviews[, list(n_submissions=sum(.N)), by=page_id]$n_submissions,
      geom = 'histogram');
## Most of them are submitted once, maybe twice.

process_backlog = function(afc_dataset) {
  ## Extract data from the dataset on AfC submissions, identifying pages that were
  ## reviewed on a different date than they were submitted, and estimate the AfC
  ## backlog per day.
  backlog_entries = afc_dataset[date(rtimestamp) > (date(stimestamp) +1)];
  
  print(paste("processing", nrow(backlog_entries), "AfC submissions"));
  
  dates = list();
  for(i in 1:nrow(backlog_entries)) {
    dates[[length(dates) +1]] = data.table(
      submission_date=seq(date(backlog_entries[i,]$stimestamp) +1,
                          date(backlog_entries[i,]$rtimestamp),
                          by='day'));
    
    if(i %% 1000 == 0) {
      print(paste("completed", i, "submissions"));
    }
  }
  rbindlist(dates);
}

afc_backlog = process_backlog(drafts_afc_reviews);

ggplot(afc_backlog[submission_date > '2014-07-01' &
                     submission_date < '2017-11-15',
                   list(n_afcs=sum(.N)), by=submission_date],
       aes(x=submission_date, y=n_afcs)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 1700), breaks = c(0:8)*200) +
  xlab('Date') + ylab('Number of pending submissions') +
  ggtitle("Estimated AfC backlog");

## How does this compare to the AfC backlog data from User:Enterprisey?
afc_backlog_historic = fread('datasets/AfC-backlog-historic.csv');
afc_backlog_historic[, c("event_date", "event_time") := IDateTime(
  as.POSIXct(timestamp, format='%Y-%m-%dT%H:%M:%S', tz='UTC'))];

## Plot them together:
ggplot(afc_backlog[submission_date > '2014-07-01' &
                     submission_date < '2017-11-15',
                   list(n_afcs=sum(.N)), by=submission_date],
       aes(x=submission_date, y=n_afcs, colour='Our estimate')) +
  geom_line() +
  geom_line(data=afc_backlog_historic,
            aes(x=event_date, y=size, colour="AfC Pending")) +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 3000), breaks = c(0:3)*1000) +
  scale_colour_manual(values = cbPalette) +
  xlab('Date') + ylab('Number of pending submissions') +
  ggtitle("Estimated AfC backlog");

## Looks like we've got a good estimate, although it doesn't count all AfCs.
## Plot the backlog from 2016 onwards, and show where ACTRIAL started:
ggplot(afc_backlog[submission_date > '2016-01-01' &
                     submission_date < '2017-11-15',
                   list(n_afcs=sum(.N)), by=submission_date],
       aes(x=submission_date, y=n_afcs)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  xlab('Date') + ylab('Number of pending submissions') +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(limits = c(0, 1700), breaks = c(0:8)*200) +
  ggtitle("Estimated AfC backlog, 2016-2017");

## Calculate weekly slope and plot, as we did for the NPP backlog.
slope = function(vals) {
  ## Given a vector v of length n, calculate the slope as v[n-1] - v[1]/(n-1)
  (vals[length(vals)] - vals[1])/(length(vals) -1);
}

afc_backlog_daily = afc_backlog[, list(n_afcs=sum(.N)),
                                by=submission_date][order(submission_date)];
afc_backlog_daily[, n_afcs.slope := rollapply(n_afcs,
                                              width=7,
                                              FUN=slope,
                                              fill=0,
                                              align='center')];

## Not sure how to fix geom_bar to span -Inf/Inf, so I hack this by turning the
## slope positive, then relabelling the Y-axis.
ggplot(afc_backlog_daily[submission_date > '2014-07-01' &
                           submission_date < '2017-11-15'],
  aes(x=submission_date, y=n_afcs.slope)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha = 0.75) +
  geom_line() +
  xlab("Date") + ylab("Number of pending reviews / day") +
  ggtitle("7-day slope of estimated AfC backlog") +
  scale_x_date(date_breaks='1 year', date_labels = "%Y");

## Is it on average growing faster than in 2015 & 2016?
qplot(afc_backlog_daily[(submission_date >= '2015-09-15' &
                          submission_date < '2015-11-15') |
                          (submission_date >= '2016-09-15' &
                             submission_date < '2016-11-15')]$n_afcs.slope,
      geom = 'histogram');
qplot(afc_backlog_daily[(submission_date >= '2017-09-15' &
                           submission_date < '2017-11-15')]$n_afcs.slope,
      geom = 'histogram');

## It's a little skewed, but not too badly. What are the summaries?
summary(afc_backlog_daily[(submission_date >= '2015-09-15' &
                             submission_date < '2015-11-15') |
                            (submission_date >= '2016-09-15' &
                               submission_date < '2016-11-15')]$n_afcs.slope);
summary(afc_backlog_daily[(submission_date >= '2017-09-15' &
                             submission_date < '2017-11-15')]$n_afcs.slope);

## T-test and Mann-Whitney test
t.test(
  afc_backlog_daily[(submission_date >= '2015-09-15' &
                       submission_date < '2015-11-15') |
                      (submission_date >= '2016-09-15' &
                         submission_date < '2016-11-15')]$n_afcs.slope,
  afc_backlog_daily[(submission_date >= '2017-09-15' &
                       submission_date < '2017-11-15')]$n_afcs.slope
);
wilcox.test(
  afc_backlog_daily[(submission_date >= '2015-09-15' &
                       submission_date < '2015-11-15') |
                      (submission_date >= '2016-09-15' &
                         submission_date < '2016-11-15')]$n_afcs.slope,
  afc_backlog_daily[(submission_date >= '2017-09-15' &
                       submission_date < '2017-11-15')]$n_afcs.slope
);

## Question is, how fast is the growth from July until ACTRIAL, versus the
## growth during ACTRIAL?

## 1: Summary shows min and max:
summary(afc_backlog_daily[submission_date > '2017-07-01' &
                            submission_date < '2017-09-15']$n_afcs);
## When was the bottom and the peak?
afc_backlog_daily[n_afcs == 314 | n_afcs == 944];

## 2017-07-09 was the bottom, 2017-09-04 the top, for a diff of:
as.Date('2017-09-04') - as.Date('2017-07-09');
## 57 days.

## 2: Same for ACTRIAL
summary(afc_backlog_daily[submission_date > '2017-09-15' &
                            submission_date < '2017-11-15']$n_afcs);
## When was the bottom and the peak?
afc_backlog_daily[n_afcs == 846 | n_afcs == 1628];
## 2017-09-22 was the bottom, 2017-11-09 was the top, for a diff of:
as.Date('2017-11-09') - as.Date('2017-09-22');
## 48 days.

## Difference in average growth, first pre-ACTRIAL then ACTRIAL:
(944-314)/57;
(1628-846)/48;
## 11.05 vs 16.29, so about 5 more pages per day. Pre-ACTRIAL we had a growth
## of 630 over 57 days, while during ACTRIAL we've seen a growth of 782 over 48 days.
