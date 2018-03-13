## H21: The quality of newly created articles after 30 days will be unchanged.

library(data.table);
library(ggplot2);
library(lubridate);

## Read in the dataset
surviving_articles = fread('bunzip2 -c datasets/h21_30day_predictions.tsv.bz2',
                           colClasses = list(character = c('creation_timestamp',
                                                           'deletion_timestamp')),
                           na.strings = c('NULL'));

## Parse the creation and deletion timestamps
surviving_articles[, creation_timestamp := as.POSIXct(creation_timestamp,
                                                      format='%Y-%m-%d %H:%M:%S',
                                                      tz='UTC')];
surviving_articles[, deletion_timestamp := as.POSIXct(deletion_timestamp,
                                                      format='%Y-%m-%d %H:%M:%S',
                                                      tz='UTC')];

## Calculate weighed quality sums at creation and 30 days after creation,
## using the approach from Halfaker, 2017.
surviving_articles[, c_qual_sum := c_start_prob + 2*c_c_prob + 3*c_b_prob +
                     4*c_ga_prob + 5*c_fa_prob];
surviving_articles[, s_qual_sum := s_start_prob + 2*s_c_prob + 3*s_b_prob +
                     4*s_ga_prob + 5*s_fa_prob];

## Calculate change in quality:
surviving_articles[, qual_sum_delta := s_qual_sum - c_qual_sum];

## 1: Inspecting articles turning into redirects.
100*length(surviving_articles[redirect_at_30 == 1]$page_id)/
  length(surviving_articles$page_id);

## How has this proportion developed over time?
surviving_articles_prop_redirects_daily = merge(
  surviving_articles[, list(num_creations=sum(.N)), by=date(creation_timestamp)],
  surviving_articles[redirect_at_30 == 1,
                     list(num_redirects=sum(.N)), by=date(creation_timestamp)],
  by='date');
surviving_articles_prop_redirects_daily[
  , prop_redirects := 100*num_redirects/num_creations];

ggplot(surviving_articles_prop_redirects_daily,
       aes(x=date, y=prop_redirects)) +
  geom_line() +
  scale_y_continuous("Proportion in %", limits = c(0, 30)) +
  scale_x_date("Date", date_breaks='1 year', date_labels = '%Y') +
  ggtitle("Proportion of creations being redirects at 30 days") +
  geom_smooth(method='loess', span=0.2);

## Looks like there's a potential reduction during ACTRIAL. In this case we will
## not dig further, but it could be interesting to check out down the line.
## Instead, we remove the redirects.
surviving_articles = surviving_articles[redirect_at_30 == 0];

## Summary
summary(surviving_articles$qual_sum_delta);

## 1%, 5%, 10%, 90%, 95%, and 99% percentiles:
quantile(surviving_articles$qual_sum_delta, c(0.01, 0.05, 0.1, 
                                              0.90, 0.95, 0.99));

## Looks like we have some outliers.
## How many revisions are outside the 1-99 percentiles?
surviving_articles[
  qual_sum_delta < as.numeric(quantile(surviving_articles$qual_sum_delta, c(0.01))) |
    qual_sum_delta > as.numeric(quantile(surviving_articles$qual_sum_delta, c(0.99)))];

## distribution_of_quality_change_articles_surviving_30days.png
ggplot(surviving_articles, aes(x=qual_sum_delta)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous("Change in quality weighed sum",
                     limits = c(-5,5), breaks = c(-5:5)) +
  ggtitle('Distribution of 30 day quality change');

## Based on the distribution, I see no reason to not remove the outliers.
surviving_articles = surviving_articles[
  qual_sum_delta > as.numeric(quantile(surviving_articles$qual_sum_delta, c(0.01))) &
    qual_sum_delta < as.numeric(quantile(surviving_articles$qual_sum_delta, c(0.99)))];

## Calculate average per day
qualsum_delta_surviving_daily = surviving_articles[
  , list(mean_qualsum_delta=mean(qual_sum_delta)), by=date(creation_timestamp)
];

## 
ggplot(qualsum_delta_surviving_daily,
       aes(x=date, y=mean_qualsum_delta)) +
  geom_line() +
  scale_y_continuous("Change in weighed quality sum", limits = c(-0.5, 0.5)) +
  scale_x_date("Date", date_breaks='1 year', date_labels = '%Y') +
  ggtitle("Daily average quality change in 30 days") +
  geom_smooth(method='loess', span=0.2);

## More recent plot:
ggplot(qualsum_delta_surviving_daily[date > '2016-01-01' &
                                       date < '2017-11-15'],
       aes(x=date, y=mean_qualsum_delta)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha = 0.75) +
  geom_line() +
  scale_y_continuous("Change in weighed quality sum", limits = c(-0.5, 0.5)) +
  scale_x_date("Date", date_breaks='1 month', date_labels = '%m') +
  ggtitle("Daily average quality change in 30 days, 2016-2017") +
  geom_smooth(method='loess', span=0.2);

## We'll do both the average change per day (an average of averages),
## and a model. Given the apparent increase in 2017, the latter
## gets more weigh.

summary(qualsum_delta_surviving_daily[
  (date >= '2014-09-15' & date < '2014-11-15') |
    (date >= '2015-09-15' & date < '2015-11-15') |
    (date >= '2016-09-15' & date < '2016-11-15')]$mean_qualsum_delta);

summary(qualsum_delta_surviving_daily[
  (date >= '2017-09-15' & date < '2017-11-15')]$mean_qualsum_delta);

## Median and means are close (as expected, it's an average of an average),
## so a t-test should be fine.
t.test(qualsum_delta_surviving_daily[
  (date >= '2014-09-15' & date < '2014-11-15') |
    (date >= '2015-09-15' & date < '2015-11-15') |
    (date >= '2016-09-15' & date < '2016-11-15')]$mean_qualsum_delta,
  qualsum_delta_surviving_daily[
    (date >= '2017-09-15' & date < '2017-11-15')]$mean_qualsum_delta
);

t.test(
  surviving_articles[
    (date(creation_timestamp) >= '2014-09-15' & date(creation_timestamp) < '2014-11-15') |
      (date(creation_timestamp) >= '2015-09-15' & date(creation_timestamp) < '2015-11-15') |
      (date(creation_timestamp) >= '2016-09-15' & date(creation_timestamp) < '2016-11-15')
    ]$qual_sum_delta,
  surviving_articles[
    (date(creation_timestamp) >= '2017-09-15' & date(creation_timestamp) < '2017-11-15')
  ]$qual_sum_delta
);

## Slightly larger increase and it's significant. Still worth investigating
## if there's an increasing trend over time that forecasts it.

## What's the creation quality over time for these articles?
surviving_articles_cqualsum_daily = surviving_articles[
  , list(mean_cqualsum=mean(c_qual_sum)), by=date(creation_timestamp)];

ggplot(surviving_articles_cqualsum_daily,
       aes(x=date, y=mean_cqualsum)) +
  geom_line() +
  scale_y_continuous("Weighed quality sum", limits = c(0, 1.0)) +
  scale_x_date("Date", date_breaks='1 year', date_labels = '%Y') +
  ggtitle("Average quality at creation for articles surviving at least 30 days") +
  geom_smooth(method='loess', span=0.2);

## Use the bimonthly approach from previous analysis and build a forecasting model.
surviving_articles[day(creation_timestamp) < 15,
                   creation_bimonth := format(creation_timestamp, "%Y%m01")];
surviving_articles[day(creation_timestamp) >= 15,
                   creation_bimonth := format(creation_timestamp, "%Y%m15")];

qualsum_delta_surviving_bimonthly = surviving_articles[
  , list(mean_qualsum_delta=mean(qual_sum_delta)), by=creation_bimonth];
qualsum_delta_surviving_bimonthly[
  , creation_date := as.Date(creation_bimonth, format="%Y%m%d")];


ggplot(qualsum_delta_surviving_bimonthly[creation_date < '2017-12-01'],
       aes(x=creation_date, y=mean_qualsum_delta)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date("Date", date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous("Change in weighed quality sum", limits=c(0, 0.3)) +
  geom_smooth(method='loess', span=0.2) +
  ggtitle('Bimonthly average quality change in 30 days');

## Analyze the time series.
qualsum_delta_surviving_bimonthly_ts = ts(
  qualsum_delta_surviving_bimonthly[creation_date < '2017-09-15']$mean_qualsum_delta,
  frequency=24);
autoplot(qualsum_delta_surviving_bimonthly_ts);

## Let's investigate stationarity and seasonality. Given the shifts in the mean
## over time, it should not be stationary.
tsdisplay(qualsum_delta_surviving_bimonthly_ts);
tsdisplay(diff(qualsum_delta_surviving_bimonthly_ts));
tsdisplay(diff(qualsum_delta_surviving_bimonthly_ts, differences = 24));

## Clearly non-stationary, not obvious that it's seasonal.
adf.test(qualsum_delta_surviving_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(qualsum_delta_surviving_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(qualsum_delta_surviving_bimonthly_ts, differences = 24),
         alternative = 'stationary');

## Based on the AFC and PACF of the differenced time series, an ARIMA(1,1,0)
## model might fit well. We'll investigate other models too.
qualsum_delta_surviving_bimonthly_model.auto = auto.arima(
  qualsum_delta_surviving_bimonthly_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(qualsum_delta_surviving_bimonthly_model.auto);

ggAcf(residuals(qualsum_delta_surviving_bimonthly_model.auto));
ggPacf(residuals(qualsum_delta_surviving_bimonthly_model.auto));
Box.test(residuals(qualsum_delta_surviving_bimonthly_model.auto),
         lag=72, fitdf=3, type="Ljung");

## Model passes the tests, but is a seasonal model better?
qualsum_delta_surviving_bimonthly_model = Arima(
  qualsum_delta_surviving_bimonthly_ts,
  order = c(0,1,1)
);
summary(qualsum_delta_surviving_bimonthly_model);

ggAcf(residuals(qualsum_delta_surviving_bimonthly_model));
ggPacf(residuals(qualsum_delta_surviving_bimonthly_model));
Box.test(residuals(qualsum_delta_surviving_bimonthly_model),
         lag=72, fitdf=2, type="Ljung");

## Auto-model is listed first:
## ARIMA(0,1,3) : AIC=-440.98   AICc=-440.42   BIC=-431.66
## ARIMA(0,1,2): AIC=-439.07   AICc=-438.74   BIC=-432.08
## ARIMA(0,1,1): AIC=-440.43   AICc=-440.27   BIC=-435.77
## ARIMA(0,1,1)(1,0,0)[24]: AIC=-438.71   AICc=-438.37   BIC=-431.71
## ARIMA(0,1,1)(0,0,1)[24]: AIC=-438.73   AICc=-438.4   BIC=-431.74
## ARIMA(1,1,0)(0,0,1)[24]: AIC=-436.55   AICc=-436.21   BIC=-429.56
## ARIMA(0,1,1)(0,0,1)[24] with drift: AIC=-437.38   AICc=-436.82   BIC=-428.06

## The BIC is much better for the ARIMA(0,1,1) model. I'd prefer to choose that
## one then, simplicity without sacrificing much performance.

qualsum_delta_surviving_bimonthly_fc = forecast(
  qualsum_delta_surviving_bimonthly_model, h=4);

autoplot(qualsum_delta_surviving_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=3.25 + 23/24 + (c(0:3)/24),
      y=qualsum_delta_surviving_bimonthly[
        creation_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$mean_qualsum_delta),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 0.3)) +
  scale_x_continuous(breaks = 1 + c(0:14)/4, minor_breaks = NULL,
                     labels = c('Jul 2014', 'Oct',
                                'Jan 2015', 'Apr', 'Jul', 'Oct',
                                'Jan 2016', 'Apr', 'Jul', 'Oct',
                                'Jan 2017', 'Apr', 'Jul', 'Oct', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Change in weighed quality sum') +
  ggtitle("ACTRIAL forecast for bimonthly average quality change in 30 days");
