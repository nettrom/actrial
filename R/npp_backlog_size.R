## H13: The size of the backlog of articles in the New Page Patrol queue
## will remain stable.

library(data.table);
library(ggplot2);

## Load in the dataset
## npp_queue_size = fread('datasets/npp_backlog_size.tsv');
npp_queue_size = fread('https://tools.wmflabs.org/actrial/datasets/npp_backlog_size.tsv');

npp_queue_size[, npp_timestamp := as.POSIXct(npp_timestamp,
                                             format='%Y-%m-%d %H:%M:%S',
                                             tz="UTC")];

ggplot(npp_queue_size, aes(x=npp_timestamp, y=npp_num_articles)) + geom_line() +
  xlab("Date") + ylab("Number of non-redirect articles") +
  ggtitle("Size of New Page Patrol queue") +
  geom_vline(xintercept=as.POSIXct('2017-09-14 22:30:00', tz='UTC'), linetype='dashed') +
  scale_x_datetime(date_breaks='2 weeks') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.75e+4),
                     breaks = c(0:8)*2000);

## In this analysis, we'll only use the observations shortly after midnight.
npp_queue_size = npp_queue_size[hour(npp_timestamp) < 1];

ggplot(npp_queue_size[npp_timestamp < as.Date('2017-11-15')]
       , aes(x=npp_timestamp, y=npp_num_articles)) + geom_line() +
  xlab("Date") + ylab("Number of non-redirect articles") +
  ggtitle("Size of New Page Patrol queue") +
  geom_vline(xintercept=as.POSIXct('2017-09-14 22:30:00', tz='UTC'),
             linetype='dashed') +
  scale_x_datetime(date_breaks='1 month', date_labels = "%b") +
  scale_y_continuous(limits = c(0, 1.7e+4), breaks = c(0:8)*2000);

## To what extent does it look flat because it's compressed?
ggplot(npp_queue_size[npp_timestamp >= as.Date('2017-09-01') &
                        npp_timestamp < as.Date('2017-11-15')]
       , aes(x=npp_timestamp, y=npp_num_articles)) + geom_line() +
  xlab("Date") + ylab("Number of non-redirect articles") +
  ggtitle("Size of New Page Patrol queue") +
  geom_vline(xintercept=as.POSIXct('2017-09-14 22:30:00', tz='UTC'),
             linetype='dashed') +
  scale_x_datetime(date_breaks='1 month', date_labels = "%b") +
  scale_y_continuous(limits = c(12000, 17000));

## 7-day moving average?
npp_queue_size[
  , num_pages.md7 := rollapply(npp_num_articles,
                                   width=7,
                                   FUN=median,
                                   na.rm=TRUE,
                                   fill=0,
                                   align='right')];
npp_queue_size[
  , num_pages.ma7 := rollapply(npp_num_articles,
                               width=7,
                               FUN=mean,
                               na.rm=TRUE,
                               fill=0,
                               align='right')];

npp_queue_size$num_pages.diff = c(0, diff(npp_queue_size$npp_num_articles));

npp_queue_size[
  , num_pages.diff_ma7 := rollapply(num_pages.diff,
                               width=7,
                               FUN=mean,
                               na.rm=TRUE,
                               fill=0,
                               align='right')];
npp_queue_size[
  , num_pages.ma7c := rollapply(npp_num_articles,
                                width=7,
                                FUN=mean,
                                na.rm=TRUE,
                                fill=0,
                                align='center')];
npp_queue_size$num_pages.ma7c.diff = c(0, diff(npp_queue_size$num_pages.ma7c));

npp_queue_size[
  , num_pages.slope := rollapply(npp_num_articles,
                                width=7,
                                FUN=slope,
                                fill=0,
                                align='center')];

slope = function(vals) {
  ## Given a vector v of length n, calculate the slope as v[n-1] - v[1]/(n-1)
  (vals[length(vals)] - vals[1])/(length(vals) -1);
}

## Label weekends because we're curious
npp_queue_size[, npp_weekend := NA];
npp_queue_size[wday(npp_timestamp, label = TRUE) %in%
                 c('Sat', 'Sun'), npp_weekend := 1/0];

ggplot(npp_queue_size[
  , list(npp_date = as.Date(npp_timestamp), npp_num_articles, num_pages.diff, npp_weekend)][
    npp_date >= '2017-09-01' & npp_date < '2017-11-15'
  ],
  aes(x=npp_date, y=npp_num_articles)) +
  geom_line() +
  geom_vline(xintercept=as.Date('2017-09-14 22:30:00', tz='UTC'),
             linetype='dashed') +
  geom_bar(stat="identity", aes(x=npp_date,
                                y=npp_weekend),
           fill='yellow', alpha=.4, width = 1) +
  scale_x_date(date_breaks='1 month', date_labels = "%b") +
  xlab("Date") + ylab("Number of non-redirect articles") +
  ggtitle("Size of New Page Patrol queue");
  
## Not sure how to fix geom_bar to span -Inf/Inf, so I hack this by turning the
## slope positive, then relabelling the Y-axis.
ggplot(npp_queue_size[
  , list(npp_date = as.Date(npp_timestamp),
         num_pages.slope = 400 + num_pages.slope, npp_weekend)][
    npp_date >= '2017-09-01' & npp_date < '2017-11-15'
    ],
  aes(x=npp_date, y=num_pages.slope)) +
  # geom_bar(stat="identity", aes(x=npp_date,
  #                               y=npp_weekend),
  #          fill='yellow', alpha=.4, width = 1) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha = 0.75) +
  geom_line() +
  xlab("Date") + ylab("Number of articles / day") +
  ggtitle("7-day slope of New Page Patrol queue size") +
  scale_x_date(date_breaks='1 month', date_labels = "%b") +
  scale_y_continuous(labels = function(x) { x - 400 });

## What's the count on Sept 1, Sept 15, Oct 1 and Nov 14?
npp_queue_size[date(npp_timestamp) == as.Date('2017-09-01') |
                 date(npp_timestamp) == as.Date('2017-09-15') |
                 date(npp_timestamp) == as.Date('2017-10-01') |
                 date(npp_timestamp) == as.Date('2017-11-14')]

diff(npp_queue_size[npp_timestamp < as.Date('2017-11-15')]$npp_num_articles,
     lag = 14);
