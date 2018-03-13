## H20: The quality of articles entering the NPP queue will increase.

## This analysis expects the dataset of non-autopatrolled article creations,
## `non_autopatrolled_creations_main`, from creation_datasets.R, to be loaded
## into memory.

library(data.table);
library(ggplot2);
library(lubridate);
library(forecast);
library(tseries);


## Read in the dataset of quality predictions
qual_preds = fread('bunzip2 -c datasets/creations_main_unpatrolled_predictions.tsv.bz2');

setkey(non_autopatrolled_creations_main, revision_id);
setkey(qual_preds, rev_id);

## Left join so that we can determine how many revisions we do not have predictions
## for, as that's our first measure.
main_creations_predictions = qual_preds[non_autopatrolled_creations_main];

## To measure article quality, we use Halfaker's approach from the "Keilana effect"
## paper, a weighed sum of ORES WP 1.0 article quality prediction:
## sum(I(c)*P(c)) where 'c' is the class, I(c) is 0 for "Stub",
## "1" for Start, and so on up to "5" for FA.
main_creations_predictions[, qual_sum := start_prob + 2*c_prob + 3*b_prob +
                             4*ga_prob + 5*fa_prob];

## 1: From Jan 1, 2009 through November 2017, how many non-autopatrolled creations
## were there?
length(main_creations_predictions$creation_timestamp);
## How many do we have predictions for?
length(main_creations_predictions[!is.na(draft_prediction)]$creation_timestamp);
## What proportion do we not have predictions for?
100*length(main_creations_predictions[!is.na(draft_prediction)]$creation_timestamp)/
  length(main_creations_predictions$creation_timestamp);

## Calculate the proportion daily and plot it across the entirety of our dataset,
## as well as since 2016:
permanent_deletions_daily = merge(
  main_creations_predictions[, list(n_creations=sum(.N)), by=creation_date],
  main_creations_predictions[is.na(draft_prediction),
                             list(n_deletions=sum(.N)), by=creation_date],
  by='creation_date'
);
permanent_deletions_daily[, prop_deletions := 100*n_deletions/n_creations];

## prop_non-autpatrolled_creations_permanently_deleted_2009-2017_with_trend.png
ggplot(permanent_deletions_daily,
       aes(x=creation_date, y=prop_deletions)) +
  geom_line() +
  scale_y_continuous(breaks = c(0:5)*10, limits = c(0,55)) +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of created articles permanently deleted") +
  geom_smooth(method='loess', span=0.2);

## prop_non-autpatrolled_creations_permanently_deleted_2016-2017_with_trend.png
ggplot(permanent_deletions_daily[creation_date >= '2016-01-01'],
       aes(x=creation_date, y=prop_deletions)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha = 0.75) +
  geom_line() +
  scale_y_continuous(breaks = c(0:5)*10, limits = c(0,55)) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of created articles permanently deleted, 2016 onwards") +
  geom_smooth(method='loess', span=0.2);

## Comparison of ACTRIAL against 2012-2016. First, let's examine the distribution
## and the summary statistics.

qplot(permanent_deletions_daily[
  (creation_date >= '2012-09-15' & creation_date < '2012-11-15') |
    (creation_date >= '2013-09-15' & creation_date < '2013-11-15') |
    (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15')]$prop_deletions,
  geom = 'histogram');

qplot(permanent_deletions_daily[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15')]$prop_deletions,
  geom = 'histogram');

summary(permanent_deletions_daily[
  (creation_date >= '2012-09-15' & creation_date < '2012-11-15') |
    (creation_date >= '2013-09-15' & creation_date < '2013-11-15') |
    (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15')]$prop_deletions);
summary(permanent_deletions_daily[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15')]$prop_deletions);

t.test(permanent_deletions_daily[
  (creation_date >= '2012-09-15' & creation_date < '2012-11-15') |
    (creation_date >= '2013-09-15' & creation_date < '2013-11-15') |
    (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15')]$prop_deletions,
  permanent_deletions_daily[
    (creation_date >= '2017-09-15' & creation_date < '2017-11-15')]$prop_deletions
);

## 2: What proportion of articles that we have predictions for are flagged as
## "OK" by the draft quality model? We use a threshold of 0.664 based on our
## analysis of precision and recall of the draft quality model.

main_creations_predicted_ok = merge(
  main_creations_predictions[!is.na(draft_prediction),
                             list(n_predictions=sum(.N)), by=creation_date],
  main_creations_predictions[draft_prediction == 'OK' &
                               ok_prob >= 0.664,
                             list(n_ok=sum(.N)), by=creation_date],
  by='creation_date'
);
main_creations_predicted_ok[, prop_ok := 100*n_ok/n_predictions];

## prop_articles_not_flagged_draftquality_2009-2017.png
ggplot(main_creations_predicted_ok,
       aes(x=creation_date, y=100-prop_ok)) +
  geom_line() +
  scale_y_continuous(breaks = c(0:4)*10, limits = c(0,40)) +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of predicted creations not flagged as OK") +
  geom_smooth(method='loess', span=0.2);

## prop_articles_not_flagged_draftquality_2016-2017.png
ggplot(main_creations_predicted_ok[creation_date >= '2016-01-01'],
       aes(x=creation_date, y=100-prop_ok)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha = 0.75) +
  geom_line() +
  scale_y_continuous(breaks = c(0:4)*10, limits = c(0,40)) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of predicted creations not flagged as OK, 2016 onwards") +
  geom_smooth(method='loess', span=0.2);

## Compare it against 2012-2016:

qplot(main_creations_predicted_ok[
  (creation_date >= '2012-09-15' & creation_date < '2012-11-15') |
    (creation_date >= '2013-09-15' & creation_date < '2013-11-15') |
    (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15')]$prop_ok,
      geom = 'histogram');
qplot(main_creations_predicted_ok[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15')]$prop_ok,
  geom = 'histogram');

summary(main_creations_predicted_ok[
  (creation_date >= '2012-09-15' & creation_date < '2012-11-15') |
    (creation_date >= '2013-09-15' & creation_date < '2013-11-15') |
    (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15')]$prop_ok);
summary(main_creations_predicted_ok[
  (creation_date >= '2017-09-15' & creation_date < '2017-11-15')]$prop_ok);

t.test(main_creations_predicted_ok[
  (creation_date >= '2012-09-15' & creation_date < '2012-11-15') |
    (creation_date >= '2013-09-15' & creation_date < '2013-11-15') |
    (creation_date >= '2014-09-15' & creation_date < '2014-11-15') |
    (creation_date >= '2015-09-15' & creation_date < '2015-11-15') |
    (creation_date >= '2016-09-15' & creation_date < '2016-11-15')]$prop_ok,
  main_creations_predicted_ok[
    (creation_date >= '2017-09-15' & creation_date < '2017-11-15')]$prop_ok);

## Switch to a bimonthly calculation, make a time series, analyze it and
## train an ARIMA model, then use that to forecast during ACTRIAL.

main_creations_predictions[day(creation_date) < 15,
                         creation_bimonth := format(creation_date, "%Y%m01")];
main_creations_predictions[day(creation_date) >= 15,
                         creation_bimonth := format(creation_date, "%Y%m15")];

main_creations_predicted_ok_bimonthly = merge(
  main_creations_predictions[!is.na(draft_prediction),
                             list(n_predictions=sum(.N)), by=creation_bimonth],
  main_creations_predictions[draft_prediction == 'OK' &
                               ok_prob >= 0.664,
                             list(n_ok=sum(.N)), by=creation_bimonth],
  by='creation_bimonth'
);
main_creations_predicted_ok_bimonthly[, prop_ok := 100*n_ok/n_predictions];
main_creations_predicted_ok_bimonthly[
  , creation_date := as.Date(creation_bimonth, format="%Y%m%d")];

ggplot(main_creations_predicted_ok_bimonthly,
       aes(x=creation_date, y=100-prop_ok)) +
  geom_line() +
  scale_y_continuous(breaks = c(0:3)*10, limits = c(0,35)) +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("Proportion of predicted creations not flagged as OK") +
  geom_smooth(method='loess', span=0.2);

## Analyze the time series.
main_creations_predicted_ok_bimonthly_ts = ts(
  main_creations_predicted_ok_bimonthly[creation_date < '2017-09-15']$prop_ok,
  frequency=24);
autoplot(main_creations_predicted_ok_bimonthly_ts);

## Let's investigate stationarity and seasonality. Given the shifts in the mean
## over time, it should not be stationary.
tsdisplay(main_creations_predicted_ok_bimonthly_ts);
tsdisplay(diff(main_creations_predicted_ok_bimonthly_ts));
tsdisplay(diff(main_creations_predicted_ok_bimonthly_ts, differences = 24));

## It's not stationary, and the ACF/PACF of the differenced time series
## clearly point to a seasonal component.
adf.test(main_creations_predicted_ok_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(main_creations_predicted_ok_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(main_creations_predicted_ok_bimonthly_ts, differences = 24),
         alternative = 'stationary');

## Based on the ACF and PACF after a seasonal diff, an ARIMA(2,0,q)(0,1,1)[24]
## should be a good starting point. Unsure what q will be though, ACF suggests 5.

## Increasing order because we're not doing stepwise testing and might find
## a model with order higher than the default maximum of 5.
main_creations_predicted_ok_bimonthly_model.auto = auto.arima(
  main_creations_predicted_ok_bimonthly_ts,
  max.order = 10,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(main_creations_predicted_ok_bimonthly_model.auto);

ggAcf(residuals(main_creations_predicted_ok_bimonthly_model.auto));
ggPacf(residuals(main_creations_predicted_ok_bimonthly_model.auto));
Box.test(residuals(main_creations_predicted_ok_bimonthly_model.auto),
         lag=72, fitdf=2, type="Ljung");

## Model looks okay. Can we improve on it?

main_creations_predicted_ok_bimonthly_model = Arima(
  main_creations_predicted_ok_bimonthly_ts,
  order = c(1,1,0),
  seasonal = list(order = c(0,1,1),
                  frequency = 24)
);
summary(main_creations_predicted_ok_bimonthly_model);

ggAcf(residuals(main_creations_predicted_ok_bimonthly_model));
ggPacf(residuals(main_creations_predicted_ok_bimonthly_model));
Box.test(residuals(main_creations_predicted_ok_bimonthly_model),
         lag=72, fitdf=3, type="Ljung");

## ARIMA(1,1,0)(0,1,1)[24]: AIC=691.03   AICc=691.16   BIC=700.67
## ARIMA(2,1,0)(0,0,1)[24]: AIC=759.99   AICc=760.19   BIC=773.34
## ARIMA(2,1,0)(0,1,1)[24]: AIC=692.82   AICc=693.05   BIC=705.68
## ARIMA(2,1,1)(0,1,1)[24]: AIC=694.43   AICc=694.77   BIC=710.51
## ARIMA(3,1,0)(0,1,1)[24]: AIC=693.98   AICc=694.32   BIC=710.06
## ARIMA(4,1,0)(0,1,1)[24]: AIC=689.62   AICc=690.1   BIC=708.91

## The ARIMA(1,1,0)(0,1,1)[24] model passes all tests. The more complex AR(4)-
## based model has a much better fit, but it's difficult to determine that
## it's significantly better. We go with the simpler model.

main_creations_predicted_ok_bimonthly_fc = forecast(
  main_creations_predicted_ok_bimonthly_model, h=4);

autoplot(main_creations_predicted_ok_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=8.75 + 23/24 + (c(0:3)/24),
      y=main_creations_predicted_ok_bimonthly[
        creation_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$prop_ok),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = 1 + c(0:18)/2, minor_breaks = NULL,
                     labels = c('Jan 2009', 'Jul', 'Jan 2010', 'Jul',
                                'Jan 2011', 'Jul', 'Jan 2012', 'Jul',
                                'Jan 2013', 'Jul', 'Jan 2014', 'Jul',
                                'Jan 2015', 'Jul', 'Jan 2016', 'Jul',
                                'Jan 2017', 'Jul', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("ACTRIAL forecast for proportion of creations flagged OK");

## 3: Of the articles that are flagged as "OK", is there a change in their quality?

main_creations_predictions_qualsum = main_creations_predictions[
  draft_prediction == "OK" & ok_prob >= 0.664,
  list(mean_qualsum=mean(qual_sum)), by=creation_date];

## quality_avg_weighed_sum_2009-2017_nonflagged_articles.png
ggplot(main_creations_predictions_qualsum,
       aes(x=creation_date, y=mean_qualsum)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle('Average quality weighed sum of creations labelled "OK"') +
  geom_smooth(method='loess', span=0.2) + ylim(0, 1.5);

## quality_avg_weighed_sum_2016-2017_nonflagged_articles.png
ggplot(main_creations_predictions_qualsum[
  creation_date >= '2016-01-01'],
       aes(x=creation_date, y=mean_qualsum)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha = 0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle('Average quality weighed sum of creations labelled "OK", 2016-2017') +
  geom_smooth(method='loess', span=0.2) + ylim(0, 1.5);

## What's the average during ACTRIAL?
summary(main_creations_predictions_qualsum[
  creation_date >= '2017-09-15' & creation_date < '2017-11-15']$mean_qualsum);

## Calculate a bimonthly average, plot, then create and analyze the time series.

main_creations_predictions_qualsum_bimonthly = main_creations_predictions[
  draft_prediction == "OK" & ok_prob >= 0.664,
  list(mean_qualsum=mean(qual_sum)), by=creation_bimonth];
main_creations_predictions_qualsum_bimonthly[
  , creation_date := as.Date(creation_bimonth, format="%Y%m%d")];

## Plot:
ggplot(main_creations_predictions_qualsum_bimonthly,
  aes(x=creation_date, y=mean_qualsum)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,1.0)) +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle('Average bimonthly quality weighed sum of creations labelled "OK"') +
  geom_smooth(method='loess', span=0.2);

## Analyze the time series.
main_creations_qualsum_bimonthly_ts = ts(
  main_creations_predictions_qualsum_bimonthly[creation_date < '2017-09-15']$mean_qualsum,
  frequency=24);
autoplot(main_creations_qualsum_bimonthly_ts);

## Let's investigate stationarity and seasonality. Given the shifts in the mean
## over time, it should not be stationary.
tsdisplay(main_creations_qualsum_bimonthly_ts);
tsdisplay(diff(main_creations_qualsum_bimonthly_ts));
tsdisplay(diff(main_creations_qualsum_bimonthly_ts, differences = 24));

## It's not stationary. The ACF/PACF of the differenced time series
## does not make it clear that it has a seasonal component.
adf.test(main_creations_qualsum_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(main_creations_qualsum_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(main_creations_qualsum_bimonthly_ts, differences = 24),
         alternative = 'stationary');

## An ARIMA(1,1,0) model is a good starting point. We will also check seasonal
## models and drift.

main_creations_qualsum_bimonthly_model.auto = auto.arima(
  main_creations_qualsum_bimonthly_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(main_creations_qualsum_bimonthly_model.auto);

ggAcf(residuals(main_creations_qualsum_bimonthly_model.auto));
ggPacf(residuals(main_creations_qualsum_bimonthly_model.auto));
Box.test(residuals(main_creations_qualsum_bimonthly_model.auto),
         lag=72, fitdf=4, type="Ljung");

## Model looks okay. Can we improve on it?

# main_creations_qualsum_bimonthly_model = Arima(
#   main_creations_qualsum_bimonthly_ts,
#   order = c(1,1,1),
#   seasonal = list(order = c(1,0,0),
#                   frequency = 24)
# );
# summary(main_creations_qualsum_bimonthly_model);
# 
# ggAcf(residuals(main_creations_qualsum_bimonthly_model));
# ggPacf(residuals(main_creations_qualsum_bimonthly_model));
# Box.test(residuals(main_creations_qualsum_bimonthly_model),
#          lag=72, fitdf=3, type="Ljung");

## ARIMA(1,1,2): AIC=-726.07   AICc=-725.88   BIC=-712.72
## ARIMA(2,1,1): AIC=-726.41   AICc=-726.22   BIC=-713.06
## ARIMA(0,1,1)(0,1,1)[24]: AIC=-600.81   AICc=-600.68   BIC=-591.17
## ARIMA(0,1,1)(1,1,0)[24]: AIC=-590.31   AICc=-590.18   BIC=-580.67
## ARIMA(0,1,1)(1,0,0)[24]: AIC=-725.81   AICc=-725.7   BIC=-715.8
## ARIMA(0,1,1)(0,0,1)[24]: AIC=-725.17   AICc=-725.05   BIC=-715.15
## ARIMA(0,1,2)(1,0,0)[24]: AIC=-725.86   AICc=-725.66   BIC=-712.51
## ARIMA(1,1,1)(1,0,0)[24]: AIC=-727.79   AICc=-727.6   BIC=-714.44

## Not really any improvements. Let auto-generated model has a nice AFC/PACF
## plot, so we'll stick with that.

main_creations_qualsum_bimonthly_fc = forecast(
  main_creations_qualsum_bimonthly_model.auto, h=4);

autoplot(main_creations_qualsum_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=8.75 + 23/24 + (c(0:3)/24),
      y=main_creations_predictions_qualsum_bimonthly[
        creation_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$mean_qualsum),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 1.0)) +
  scale_x_continuous(breaks = 1 + c(0:18)/2, minor_breaks = NULL,
                     labels = c('Jan 2009', 'Jul', 'Jan 2010', 'Jul',
                                'Jan 2011', 'Jul', 'Jan 2012', 'Jul',
                                'Jan 2013', 'Jul', 'Jan 2014', 'Jul',
                                'Jan 2015', 'Jul', 'Jan 2016', 'Jul',
                                'Jan 2017', 'Jul', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle('ACTRIAL forecast for average quality weighed sum of "OK" creations');
