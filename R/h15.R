## H15: The rate of article growth will be reduced.

library(data.table);
library(ggplot2);
library(forecast);
library(tseries);
library(zoo);
library(lubridate);

## This analysis assumes that all_creations_main from creation_datasets.R
## is loaded into memory.

## Load in the dataset of main namespace deletions from July 2014 to Nov 15, 2017,
## and parse the deletion timestamp.
main_namespace_deletions = fread(
  'bunzip2 -c datasets/enwiki_main_namespace_deletions_2014-2017.tar.bz2',
  colClasses = c('numeric', 'character'));
main_namespace_deletions[
  , deletion_timestamp := as.POSIXct(log_timestamp,
                                     format='%Y%m%d%H%M%S', tz='UTC')];

## Load in the dataset on page moves into Main from User and Draft
main_namespace_moves = fread('bunzip2 -c datasets/pages_moved_into_main.tsv.bz2');

## Add a column to the creation dataset that's the POSIXct representation of
## the creation timestamp:
all_creations_main[
  , creation_timestamp := as.POSIXct(
    paste(creation_date, creation_time),
    tz='UTC')
];

## Add a similar column to the page moves dataset:
main_namespace_moves[
  , move_timestamp := as.POSIXct(pmim_timestamp,
                                 format="%Y-%m-%d %H:%M:%S", tz='UTC')];

## Add a date column to the page moves dataset, which we'll call "creation_date"
## so it matches the other datasets.
main_namespace_moves[
  , creation_date := as.IDate(move_timestamp, format="%Y-%m-%d %H:%M:%S", tz='UTC')
];

## Now, we can left join creations and deletions, calculate the time to deletion,
## and remove anything that was deleted before some number of days passed.

setkey(all_creations_main, page_id);
setkey(main_namespace_deletions, log_page);
setkey(main_namespace_moves, pmim_page_id);

growth_at_k = function(k=30) {
  rbind(
    main_namespace_deletions[non_autopatrolled_creations_main[
      creation_date >= '2014-07-01' & creation_date < '2017-11-15'
      ]][is.na(deletion_timestamp) | 
           (as.numeric(deletion_timestamp - creation_timestamp) > 60*60*24*k)][
             , list(num_creations = sum(.N)), by=creation_date],
    main_namespace_deletions[main_namespace_moves[
      date(move_timestamp) >= '2014-07-01' & date(move_timestamp) < '2017-11-15'
      ]][is.na(deletion_timestamp) | 
           (as.numeric(deletion_timestamp - move_timestamp) > 60*60*24*k)][
             , list(num_creations = sum(.N)), by=creation_date]
  )[, list(num_creations = sum(num_creations)), by=creation_date];
}

growth_at_k_bimonthly = function(k=30) {
  rbind(
    main_namespace_deletions[non_autopatrolled_creations_main[
      creation_date >= '2014-07-01' & creation_date < '2017-11-15'
      ]][is.na(deletion_timestamp) | 
              (as.numeric(deletion_timestamp - creation_timestamp) > 60*60*24*k)][
                , creation_bimonth := mapply(function(x) { if(day(x) < 15) format(x, "%Y%m01") else format(x, "%Y%m15") }, creation_timestamp)][
                  , list(num_creations = sum(.N)), by=creation_bimonth],
    main_namespace_deletions[main_namespace_moves[
      date(move_timestamp) >= '2014-07-01' & date(move_timestamp) < '2017-11-15'
      ]][is.na(deletion_timestamp) | 
              (as.numeric(deletion_timestamp - move_timestamp) > 60*60*24*k)][
                , creation_bimonth := mapply(function(x) { if(day(x) < 15) format(x, "%Y%m01") else format(x, "%Y%m15") }, move_timestamp)][
                  , list(num_creations = sum(.N)), by=creation_bimonth]
  )[, list(num_creations = sum(num_creations)), by=creation_bimonth];
}

## Growth rate based on deletions within 30, 60, and 90 days:
growth_rate_k30 = growth_at_k();
growth_rate_k60 = growth_at_k(k=60);
growth_rate_k90 = growth_at_k(k=90);

ggplot(growth_rate_k30,
       aes(x=creation_date, y=num_creations)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() + 
  geom_smooth(method='loess', span=0.2) +
  scale_y_continuous(limits = c(0, 1250)) +
  xlab('Date') + ylab('Article growth per day') +
  ggtitle('English Wikipedia growth per day of articles not deleted within 30 days');

## Make 7 day moving medians, rbind, make a plot.
growth_rate_k30[order(creation_date)
  , growth.md7 := rollapply(num_creations,
                            width=7, FUN=median,
                            na.rm=TRUE, fill=0, align='right')
];
growth_rate_k60[order(creation_date)
  , growth.md7 := rollapply(num_creations,
                            width=7, FUN=median,
                            na.rm=TRUE, fill=0, align='right')
  ];
growth_rate_k90[order(creation_date)
  , growth.md7 := rollapply(num_creations,
                            width=7, FUN=median,
                            na.rm=TRUE, fill=0, align='right')
  ];

ggplot(
  rbind(
    growth_rate_k30[, list(creation_date, num_creations, k=30)],
    growth_rate_k60[, list(creation_date, num_creations, k=60)],
    growth_rate_k90[, list(creation_date, num_creations, k=90)]),
  aes(x=creation_date, y=num_creations, fill=k, colour=k)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() + 
  scale_y_continuous(limits = c(200, 600)) +
  xlab('Date') + ylab('Articles created per day') +
  ggtitle('English Wikipedia growth per day of articles not deleted within k days');

ggplot(
  rbind(
    growth_rate_k30[growth.md7 > 0, list(creation_date, growth.md7, k=30)],
    growth_rate_k60[growth.md7 > 0, list(creation_date, growth.md7, k=60)],
    growth_rate_k90[growth.md7 > 0, list(creation_date, growth.md7, k=90)]),
  aes(x=creation_date, y=growth.md7, fill=factor(k), colour=factor(k))) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() + 
  scale_y_continuous(limits = c(0, 700), breaks = c(0:7)*100) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date('2014-07-01'), as.Date('2017-11-16'))) +
  scale_color_manual(values=cbPalette) +
  guides(color=guide_legend(title="k")) +
  xlab('Date') + ylab('Article growth per day, 7-day moving median') +
  ggtitle('English Wikipedia growth per day of articles not deleted within k days');

## What's the diff between 30 and 90 days?
summary(merge(growth_rate_k30, growth_rate_k90, by = 'creation_date')[
  , list(diff_creations = num_creations.x - num_creations.y)]$diff_creations);
## 75% of them are less than 12, little reason to extend it then.

growth_30_90 = merge(growth_rate_k30, growth_rate_k90, by = 'creation_date');
growth_30_90[, diff_creations := num_creations.x - num_creations.y];
growth_30_90[, diff_prop := 100*diff_creations / num_creations.x];

summary(growth_30_90$diff_prop);

## Make a bimonthly plot of the k30 dataset.

growth_rate_k30_bimonthly = growth_at_k_bimonthly();
growth_rate_k30_bimonthly[
  , creation_date := as.Date(creation_bimonth, format="%Y%m%d")];

ggplot(growth_rate_k30_bimonthly[!is.na(creation_date) &
                                   creation_date < '2017-12-01'],
       aes(x=creation_date, y=num_creations)) +
  geom_line() +
  ggtitle('Bimonthly growth of articles not deleted within 30 days') +
  xlab('Date') + ylab('Number of articles created') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11000),
                     breaks = c(0:10)*1000) +
  geom_smooth(method='loess', span=0.2);

## Let's build a forecasting model

growth_rate_k30_bimonthly_ts = ts(
  growth_rate_k30_bimonthly[creation_date < '2017-09-15']$num_creations,
  frequency=24);
autoplot(growth_rate_k30_bimonthly_ts);

## Let's investigate seasonality and stationarity:
tsdisplay(growth_rate_k30_bimonthly_ts);

## It looks stationary, the PACF suggests there is a yearly season:
tsdisplay(diff(growth_rate_k30_bimonthly_ts/10, differences = 24));

## Taking the seasonal component out appears to help, but we'll
## investigate both seasonal and non-seasonal models to be certain.

## Test for stationarity, both with and without seasonal:
adf.test(diff(growth_rate_k30_bimonthly_ts/10),
         alternative = 'stationary');
adf.test(diff(growth_rate_k30_bimonthly_ts/10, differences = 24),
         alternative = 'stationary');

## It's not stationary, requires diffing. 
tsdisplay(diff(growth_rate_k30_bimonthly_ts));

## ACFs and PACFs suggest an ARIMA(2,1,0) model, or perhaps
## an ARIMA(3,0,0)(0,1,1)[24] model.

growth_rate_k30_bimonthly_model.auto = auto.arima(
  growth_rate_k30_bimonthly_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(growth_rate_k30_bimonthly_model.auto);

ggAcf(residuals(growth_rate_k30_bimonthly_model.auto));
ggPacf(residuals(growth_rate_k30_bimonthly_model.auto));
Box.test(residuals(growth_rate_k30_bimonthly_model.auto),
         lag=72, fitdf=3, type="Ljung");

## Model looks reasonably good, and it's simple. Let's check some competing
## models.
## Auto model: ARIMA(0,1,3): AIC=1254.42   AICc=1254.99   BIC=1263.75
## ARIMA(3,1,0)(0,0,1)[24]: AIC=1258.22   AICc=1259.07   BIC=1269.87
## ARIMA(3,1,0)(0,1,1)[24]: AIC=893.96   AICc=895.27   BIC=903.72
## ARIMA(0,1,3)(0,1,1)[24]: AIC=891.8   AICc=893.1   BIC=901.56
## ARIMA(3,1,1)(0,1,1)[24]: AIC=891.96   AICc=893.83   BIC=903.67
## ARIMA(1,1,3)(0,1,1)[24]: AIC=891.33   AICc=893.2   BIC=903.04
## ARIMA(0,1,3)(0,1,2)[24]: AIC=892.84   AICc=894.71   BIC=904.55
## ARIMA(2,1,0)(0,1,1)[24]: AIC=893.22   AICc=894.08   BIC=901.03
## ARIMA(0,1,2)(0,1,1)[24]: AIC=894.48   AICc=895.33   BIC=902.28
## ARIMA(1,1,2)(0,1,1)[24]: AIC=891.45   AICc=892.75   BIC=901.2
## ARIMA(2,1,1)(0,1,1)[24]: AIC=889.71   AICc=891.01   BIC=899.46
## ARIMA(1,1,1)(0,1,1)[24]: AIC=891.81   AICc=892.66   BIC=899.61
## ARIMA(0,1,4)(0,1,1)[24]: AIC=891.76   AICc=893.63   BIC=903.47
## ARIMA(0,1,5)(0,1,1)[24]: AIC=892.05   AICc=894.59   BIC=905.71

growth_rate_k30_bimonthly_model = Arima(
  growth_rate_k30_bimonthly_ts,
  order = c(3,1,1),
  seasonal = list(order = c(0,1,1),
                  frequency = 24));
summary(growth_rate_k30_bimonthly_model);

ggAcf(residuals(growth_rate_k30_bimonthly_model));
ggPacf(residuals(growth_rate_k30_bimonthly_model));
Box.test(residuals(growth_rate_k30_bimonthly_model),
         lag=72, fitdf=6, type="Ljung");

growth_rate_k30_bimonthly_fc = forecast(
  growth_rate_k30_bimonthly_model, h=4);

autoplot(growth_rate_k30_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=4 + 5/24 + (c(0:3)/24),
      y=growth_rate_k30_bimonthly[
        creation_bimonth %in% c('20170915', '20171001', '20171015',
                                '20171101')]$num_creations),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 10500), breaks = c(0:5)*2000) +
  scale_x_continuous(breaks = 1 + c(0:14)/4, minor_breaks = NULL,
                     labels = c('Jul 2014', 'Oct', 'Jan 2015', 'Apr',
                                'Jul', 'Oct', 'Jan 2016', 'Apr', 'Jul',
                                'Oct', 'Jan 2017', 'Apr', 'Jul', 'Oct',
                                'Jan 2018')) +
  xlab('Time') + ylab('Number of articles') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for bimonthly article growth");

## Future work: split it up by autopatrolled and other creations, then see whether
## there are meaningful differences between them.
