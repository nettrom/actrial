## H22: The quality of articles entering the AfC queue will be unchanged.

## This analysis expects the AfC datasets from afc.R to be loaded into memory.

library(data.table);
library(ggplot2);
library(forecast);
library(tseries);

## Read in the dataset
drafts_predictions = fread('datasets/drafts_afc/drafts_predictions.tsv', na.strings = c('NULL'));

drafts_predictions[, c("rev_date", "rev_time") := IDateTime(
  as.POSIXct(rev_timestamp, format='%Y-%m-%d %H:%M:%S', tz='UTC'))];

## We first join drafts_afc_merged to drafts_afc_reviews to get the revision
## IDs of submitted & reviewed revisions, then join with drafts_predictions to get
## the prediction for those revisions.

setkey(drafts_afc_merged, page_id, submission_timestamp);
setkey(drafts_afc_reviews, page_id, submission_timestamp);

drafts_afc_predictions = merge(
  drafts_afc_reviews[drafts_afc_merged, nomatch = 0][
  , list(page_id, submission_timestamp, review_timestamp, review_action,
         rtimestamp, stimestamp, time_to_review, time_to_review_days,
         submission_date, submission_time, review_date, review_time,
         rev_id)],
  drafts_predictions,
  by='rev_id', all.x = TRUE);

## To measure article quality, we use Halfaker's approach from the "Keilana effect"
## paper, a weighed sum of ORES WP 1.0 article quality prediction:
## sum(I(c)*P(c)) where 'c' is the class, I(c) is 0 for "Stub",
## "1" for Start, and so on up to "5" for FA.
drafts_afc_predictions[, qual_sum := start_prob + 2*c_prob + 3*b_prob +
                             4*ga_prob + 5*fa_prob];

## 1: From July 1, 2014 through November 2017, how many AfC submissions were there?
length(drafts_afc_predictions$rev_id);
## How many do we have predictions for?
length(drafts_afc_predictions[!is.na(draft_prediction)]$rev_id);
## What proportion do we not have predictions for?
100*length(drafts_afc_predictions[!is.na(draft_prediction)]$rev_id)/
  length(drafts_afc_predictions$rev_id);

## Given that it's almost 100%, there is no reason to plot it? But we'll still do it.

drafts_afc_permdeleted_daily = merge(
  drafts_afc_predictions[, list(n_submissions=sum(.N)), by=submission_date],
  drafts_afc_predictions[is.na(draft_prediction),
                         list(n_permdeleted=sum(.N)), by=submission_date],
  by='submission_date');
drafts_afc_permdeleted_daily = merge(
  data.table(submission_date = seq(as.Date('2014-07-01'), as.Date('2017-11-30'),
                                     by='day')),
  drafts_afc_permdeleted_daily,
  by = 'submission_date',
  all.x = TRUE
);
drafts_afc_permdeleted_daily[, prop_deleted := 100*n_permdeleted/n_submissions];
drafts_afc_permdeleted_daily[is.na(n_submissions), n_permdeleted := 0];
drafts_afc_permdeleted_daily[is.na(n_submissions), prop_deleted := 0.0];

## prop_afcs_permdeleted_2014-2017.png
ggplot(drafts_afc_permdeleted_daily[submission_date >= '2014-07-01' &
                                      submission_date < '2017-12-01'],
       aes(x=submission_date, y=prop_deleted)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 20)) +
  geom_smooth(method='loess', span=0.2) +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle('Proportion of AfC submissions unretrievable in early 2018');

## prop_afcs_permdeleted_2016-2017.png
ggplot(drafts_afc_permdeleted_daily[submission_date >= '2016-01-01' &
                                      submission_date < '2017-12-01'],
       aes(x=submission_date, y=prop_deleted)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 20)) +
  geom_smooth(method='loess', span=0.2) +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle('Proportion of AfC submissions unretrievable in early 2018, 2016-2017');

## It's roughly flat, compare 2014, 2015, and 2016 against ACTRIAL.
## Might use a non-parametric test due to the skewness of the proportion.
qplot(
  drafts_afc_permdeleted_daily[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$prop_deleted
);
qplot(
  drafts_afc_permdeleted_daily[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$prop_deleted
);

summary(
  drafts_afc_permdeleted_daily[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$prop_deleted
);
summary(
  drafts_afc_permdeleted_daily[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$prop_deleted
);

wilcox.test(
  drafts_afc_permdeleted_daily[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$prop_deleted,
  drafts_afc_permdeleted_daily[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$prop_deleted
);

## Proportion of days with no deletions:
drafts_afc_permdeleted_daily[
  (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
    (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
    (submission_date >= '2016-09-15' & submission_date < '2016-11-15')][
      n_permdeleted == 0];

drafts_afc_permdeleted_daily[
  (submission_date >= '2017-09-15' & submission_date < '2017-11-15')][
      n_permdeleted == 0];

## Pre-ACTRIAL: 106 out of 183 days, or 57.9%.
## ACTRIAL: 9 out of 61 days, or 14.8%.

## Note the decreasing trend during ACTRIAL, I would like to do a forecasting
## model on the bimonthly proportion.

## 2: How many AfC submissions were predicted to be OK by the draft quality model?
length(drafts_afc_predictions[draft_prediction == "OK" &
                                ok_prob >= 0.664]$rev_id);
## What proportion is that?
100*length(drafts_afc_predictions[draft_prediction == "OK" &
                                    ok_prob >= 0.664]$rev_id)/
  length(drafts_afc_predictions$rev_id);

drafts_afc_ok_daily = merge(
  drafts_afc_predictions[!is.na(draft_prediction),
                         list(n_predictions=sum(.N)), by=submission_date],
  drafts_afc_predictions[draft_prediction == "OK" &
                                               ok_prob >= 0.664,
                         list(n_ok=sum(.N)), by=submission_date],
  by='submission_date');
drafts_afc_ok_daily[, prop_ok := 100*n_ok/n_predictions];

## prop_afcs_not_flagged_draftquality_2014-2017.png
ggplot(drafts_afc_ok_daily[submission_date < '2017-12-01'],
       aes(x=submission_date, y=prop_ok)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 45)) +
  geom_smooth(method='loess', span=0.2) +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle('Proportion of AfC submissions predicted "OK"');

## prop_afcs_not_flagged_draftquality_2016-2017.png
ggplot(drafts_afc_ok_daily[submission_date >= '2016-01-01' &
                             submission_date < '2017-12-01'],
       aes(x=submission_date, y=prop_ok)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  xlab('Date') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 45)) +
  geom_smooth(method='loess', span=0.2) +
  ggtitle('Proportion of AfC submissions predicted "OK", 2016-2017');

## Because of the increasing proportion over time, we go directly to a bimonthly
## forecasting model.

drafts_afc_predictions[day(submission_date) < 15,
                       submission_bimonth := format(submission_date, "%Y%m01")];
drafts_afc_predictions[day(submission_date) >= 15,
                       submission_bimonth := format(submission_date, "%Y%m15")];

drafts_afc_ok_bimonthly = merge(
  drafts_afc_predictions[!is.na(draft_prediction),
                         list(n_predictions=sum(.N)), by=submission_bimonth],
  drafts_afc_predictions[draft_prediction == "OK" &
                           ok_prob >= 0.664,
                         list(n_ok=sum(.N)), by=submission_bimonth],
  by='submission_bimonth'
);
drafts_afc_ok_bimonthly[, prop_ok := 100*n_ok/n_predictions];
drafts_afc_ok_bimonthly[
  , submission_date := as.Date(submission_bimonth, format="%Y%m%d")];

ggplot(drafts_afc_ok_bimonthly[submission_date < '2017-12-01'],
       aes(x=submission_date, y=prop_ok)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  xlab('Date') + ylab('Number of submissions') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 25)) +
  geom_smooth(method='loess', span=0.2) +
  ggtitle('Bimonthly proportion of AfC submissions predicted "OK"');

## Analyze the time series.
drafts_afc_ok_bimonthly_ts = ts(
  drafts_afc_ok_bimonthly[submission_date < '2017-09-15']$prop_ok,
  frequency=24);
autoplot(drafts_afc_ok_bimonthly_ts);

## Let's investigate stationarity and seasonality. Given the shifts in the mean
## over time, it should not be stationary.
tsdisplay(drafts_afc_ok_bimonthly_ts);
tsdisplay(diff(drafts_afc_ok_bimonthly_ts));
tsdisplay(diff(drafts_afc_ok_bimonthly_ts, differences = 24));

## Clearly non-stationary, not obvious that it's seasonal.
adf.test(drafts_afc_ok_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(drafts_afc_ok_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(drafts_afc_ok_bimonthly_ts, differences = 24),
         alternative = 'stationary');

## Based on the AFC and PACF of the differenced time series, an ARIMA(2,1,0)
## model might fit well. We'll investigate other models too.
drafts_afc_ok_bimonthly_model.auto = auto.arima(
  drafts_afc_ok_bimonthly_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(drafts_afc_ok_bimonthly_model.auto);

ggAcf(residuals(drafts_afc_ok_bimonthly_model.auto));
ggPacf(residuals(drafts_afc_ok_bimonthly_model.auto));
Box.test(residuals(drafts_afc_ok_bimonthly_model.auto),
         lag=72, fitdf=3, type="Ljung");

## Model passes the tests, but do we really need drift and a seasonal component?

# drafts_afc_ok_bimonthly_model = Arima(
#   drafts_afc_ok_bimonthly_ts,
#   order = c(3,1,0),
#   seasonal = list(order = c(0,1,1),
#                   frequency = 24)
# );
# summary(drafts_afc_ok_bimonthly_model);
# 
# ggAcf(residuals(drafts_afc_ok_bimonthly_model));
# ggPacf(residuals(drafts_afc_ok_bimonthly_model));
# Box.test(residuals(drafts_afc_ok_bimonthly_model),
#          lag=72, fitdf=4, type="Ljung");

## Auto-model is listed first:
## ARIMA(3,0,0)(0,1,0)[24] with drift: AIC=214.62   AICc=215.9   BIC=224.48
## ARIMA(2,1,0): AIC=283.2   AICc=283.54   BIC=290.19
## ARIMA(0,0,0)(0,1,1)[24]: AIC=263.24   AICc=263.48   BIC=267.18
## ARIMA(3,1,0)(0,1,1)[24]: AIC=215.27   AICc=216.57   BIC=225.02
## ARIMA(3,0,0)(0,1,1)[24]: AIC=220.05   AICc=221.33   BIC=229.9
## ARIMA(2,1,0)(0,1,1)[24]: AIC=213.29   AICc=214.14   BIC=221.1
## ARIMA(4,1,0)(1,1,0)[24]: AIC=213.33   AICc=215.2   BIC=225.04

## The auto-generated model is slightly simpler than those using I(1) in
## both the non-seasonal and seasonal component. Let's stick with the auto-model.

drafts_afc_ok_bimonthly_fc = forecast(
  drafts_afc_ok_bimonthly_model.auto, h=4);

autoplot(drafts_afc_ok_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=3.25 + 23/24 + (c(0:3)/24),
      y=drafts_afc_ok_bimonthly[
        submission_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$prop_ok),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 25)) +
  scale_x_continuous(breaks = 1 + c(0:14)/4, minor_breaks = NULL,
                     labels = c('Jul 2014', 'Oct',
                                'Jan 2015', 'Apr', 'Jul', 'Oct',
                                'Jan 2016', 'Apr', 'Jul', 'Oct',
                                'Jan 2017', 'Apr', 'Jul', 'Oct', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Proportion in %') +
  ggtitle("ACTRIAL forecast for proportion of creations flagged OK");

## Of the drafts that are flagged OK, what is their average quality?

drafts_afc_qualsum_daily = drafts_afc_predictions[
  draft_prediction == "OK" & ok_prob >= 0.664,
  list(mean_qualsum=mean(qual_sum)), by=submission_date];

## quality_avg_weighed_sum_2014-2017_nonflagged_afcs.png
ggplot(drafts_afc_qualsum_daily[submission_date >= '2014-07-01' &
                                  submission_date < '2017-11-15'],
       aes(x=submission_date, y=mean_qualsum)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 3)) +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle('Average quality weighed sum of AfCs labelled "OK"') +
  geom_smooth(method='loess', span=0.2);

## quality_avg_weighed_sum_2016-2017_nonflagged_afcs.png
ggplot(drafts_afc_qualsum_daily[submission_date >= '2016-01-01' &
                                  submission_date < '2017-11-15'],
       aes(x=submission_date, y=mean_qualsum)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(limits = c(0, 3)) +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle('Average quality weighed sum of AfCs labelled "OK", 2016-2017') +
  geom_smooth(method='loess', span=0.2);

## Not clear that there's an increasing trend over time. We'll compare ACTRIAL
## against 2014-2016, then build a time series.
qplot(drafts_afc_qualsum_daily[
  (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
    (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
    (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$mean_qualsum,
  geom = 'histogram');
qplot(drafts_afc_qualsum_daily[
  (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$mean_qualsum,
  geom = 'histogram');

summary(drafts_afc_qualsum_daily[
  (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
    (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
    (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$mean_qualsum);
summary(drafts_afc_qualsum_daily[
  (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$mean_qualsum);

## Indicates a little bit of skewness, we'll use both tests and weigh
## the Mann-Whitney more.
t.test(
  drafts_afc_qualsum_daily[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$mean_qualsum,
  drafts_afc_qualsum_daily[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$mean_qualsum
);
wilcox.test(
  drafts_afc_qualsum_daily[
    (submission_date >= '2014-09-15' & submission_date < '2014-11-15') |
      (submission_date >= '2015-09-15' & submission_date < '2015-11-15') |
      (submission_date >= '2016-09-15' & submission_date < '2016-11-15')]$mean_qualsum,
  drafts_afc_qualsum_daily[
    (submission_date >= '2017-09-15' & submission_date < '2017-11-15')]$mean_qualsum
);

drafts_afc_qualsum_bimonthly = drafts_afc_predictions[
  draft_prediction == "OK" & ok_prob >= 0.664,
  list(mean_qualsum=mean(qual_sum)), by=submission_bimonth];
drafts_afc_qualsum_bimonthly[
  , submission_date := as.Date(submission_bimonth, format="%Y%m%d")];

## quality_avg_weighed_sum_2014-2017_nonflagged_afcs_bimonthly.png
ggplot(drafts_afc_qualsum_bimonthly[submission_date >= '2014-07-01' &
                                      submission_date < '2017-11-15'],
       aes(x=submission_date, y=mean_qualsum)) +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 1.5)) +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle('Average bimonthly quality weighed sum of AfCs labelled "OK"') +
  geom_smooth(method='loess', span=0.2);

## Analyze the time series.
drafts_afc_qualsum_bimonthly_ts = ts(
  drafts_afc_qualsum_bimonthly[submission_date < '2017-09-15']$mean_qualsum,
  frequency=24);
autoplot(drafts_afc_qualsum_bimonthly_ts);

## Let's investigate stationarity and seasonality. Given the shifts in the mean
## over time, it should not be stationary.
tsdisplay(drafts_afc_qualsum_bimonthly_ts);
tsdisplay(diff(drafts_afc_qualsum_bimonthly_ts));
tsdisplay(diff(drafts_afc_qualsum_bimonthly_ts, differences = 24));

## Appears to be non-stationary, and it might be seasonal.
adf.test(drafts_afc_qualsum_bimonthly_ts,
         alternative = 'stationary');
adf.test(diff(drafts_afc_qualsum_bimonthly_ts),
         alternative = 'stationary');
adf.test(diff(drafts_afc_qualsum_bimonthly_ts, differences = 24),
         alternative = 'stationary');

## ARIMA (2,1,0)(0,1,1)[24] could be a good start.

drafts_afc_qualsum_bimonthly_model.auto = auto.arima(
  drafts_afc_qualsum_bimonthly_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(drafts_afc_qualsum_bimonthly_model.auto);

ggAcf(residuals(drafts_afc_qualsum_bimonthly_model.auto));
ggPacf(residuals(drafts_afc_qualsum_bimonthly_model.auto));
Box.test(residuals(drafts_afc_qualsum_bimonthly_model.auto),
         lag=72, fitdf=3, type="Ljung");

## Suggests ARIMA(1,1,2) with drift, and it passes the tests.
## Is a seasonal model better? Do we need drift?

drafts_afc_qualsum_bimonthly_model = Arima(
  drafts_afc_qualsum_bimonthly_ts,
  order = c(1,1,2)
);
summary(drafts_afc_qualsum_bimonthly_model);

ggAcf(residuals(drafts_afc_qualsum_bimonthly_model));
ggPacf(residuals(drafts_afc_qualsum_bimonthly_model));
Box.test(residuals(drafts_afc_qualsum_bimonthly_model),
         lag=72, fitdf=3, type="Ljung");

## The seasonal models have no improvement:
## ARIMA(2,1,0)(0,1,1)[24]: AIC=-45.47   AICc=-44.62   BIC=-37.66
## ARIMA(2,1,0)(0,0,1)[24]: AIC=-96.24   AICc=-95.67   BIC=-86.92
## ARIMA(3,1,0)(0,0,1)[24]: AIC=-99.38   AICc=-98.52   BIC=-87.72
## ARIMA(3,1,0)(0,0,1)[24] with drift: AIC=-97.97   AICc=-96.75   BIC=-83.99
## ARIMA(2,1,1)(0,0,1)[24] with drift: AIC=-106.47   AICc=-105.25   BIC=-92.49
## ARIMA(1,1,2)(0,0,1)[24] with drift: AIC=-107.85   AICc=-106.63   BIC=-93.86
## ARIMA(1,1,2)(1,0,0)[24] with drift: AIC=-107.92   AICc=-106.7   BIC=-93.93

## Doesn't look like we need the drift.

drafts_afc_qualsum_bimonthly_fc = forecast(
  drafts_afc_qualsum_bimonthly_model, h=4);

autoplot(drafts_afc_qualsum_bimonthly_fc) +
  geom_line(
    data=data.frame(
      x=3.25 + 23/24 + (c(0:3)/24),
      y=drafts_afc_qualsum_bimonthly[
        submission_bimonth %in% c('20170915', '20171001', '20171015', '20171101')
        ]$mean_qualsum),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_x_continuous(breaks = 1 + c(0:14)/4, minor_breaks = NULL,
                     labels = c('Jul 2014', 'Oct',
                                'Jan 2015', 'Apr', 'Jul', 'Oct',
                                'Jan 2016', 'Apr', 'Jul', 'Oct',
                                'Jan 2017', 'Apr', 'Jul', 'Oct', 'Jan 2018')) +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  xlab('Date') + ylab('Weighed sum') +
  ggtitle('ACTRIAL forecast for bimonthly average quality weighed sum of AfCs labelled "OK"');


