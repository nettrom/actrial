## Analysis of user activity

library(data.table);
library(ggplot2);
library(forecast);
library(tseries);
library(zoo);
library(lubridate);

## Colourblind-friendly palette without and with black, from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Read in the activity dataset
useractivity = fread("bunzip2 -c datasets/activity_stats_20180207.tsv.bz2",
                     na.strings = c('NULL'));

## When we're looking at data by the type of account creation, we are
## mainly interested in whether the account was autocreated or not. Thus, merge
## the types into "autocreate" and "create".
useractivity[as_create_type %in% c('create2', 'byemail'), as_create_type := 'create'];

## Extract registration date from the registration timestamp
useractivity[, c("as_reg_date", "as_reg_time") := IDateTime(as.POSIXct(as_reg_timestamp, tz='UTC'))];

## Didn't look like NULLs were NA'ed correctly. Also, timestamps are off for
## accounts created as part of the SUL coercion.
useractivity[as_autoconfirmed_30_timestamp == "NULL", as_autoconfirmed_30_timestamp := NA];
useractivity[as_autoconfirmed_30_timestamp == "0000-00-00 00:00:00",
             as_autoconfirmed_30_timestamp := NA];

useractivity[!is.na(as_autoconfirmed_30_timestamp),
             c("as_ac_30_date", "as_ac_30_time") := IDateTime(as.POSIXct(as_autoconfirmed_30_timestamp, tz='UTC'))]

## Some accounts were created with a historic registration date due to SUL finalisation.
useractivity[as_ac_30_date < as_reg_date,
             c('as_autoconfirmed_30_timestamp', 'as_ac_30_date', 'as_ac_30_time') := list(NA, NA, NA)];

## Calculate the time between registration and autoconfirmed status
useractivity[!is.na(as_autoconfirmed_30_timestamp),
             as_ac_30_delta := as.POSIXct(as_autoconfirmed_30_timestamp, tz='UTC') - as.POSIXct(as_reg_timestamp, tz='UTC')]
## Some accounts have autoconfirmed delta < 4 because we use timestamps from
## the logging table (might be better to just add a second to every timestamp?)
useractivity[!is.na(as_autoconfirmed_30_timestamp) & as_ac_30_delta < 4,
             as_ac_30_delta := 4];

setkey(useractivity, as_reg_date, as_create_type);

## Proportion of users by date and type that have > 0 edits in the first 30 days
prop_nonzero_30 = merge(useractivity[, list(n_registrations=sum(.N)),
                                     by=list(as_reg_date, as_create_type)],
                        useractivity[as_num_edits_30 > 0, list(n_editors=sum(.N)),
                                     by=list(as_reg_date, as_create_type)],
                        by=c('as_reg_date', 'as_create_type'));

## Make me plots:
ggplot(prop_nonzero_30[, list(prop_nonzero = sum(n_editors)/sum(n_registrations)),
                       by=as_reg_date],
       aes(x=as_reg_date, y=100*prop_nonzero)) + geom_line() +
  ggtitle('Proportion of accounts with non-zero edits') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  xlab('Year') +
  ylab('Proportion in %');

ggplot(prop_nonzero_30[
  , list(prop_nonzero = 100*sum(n_editors)/sum(n_registrations)),
  by=list(as_reg_date, type=as_create_type)],
  aes(x=as_reg_date, y=prop_nonzero, group=type, colour=type)) +
  geom_line() +
  ggtitle('Accounts editing in first 30 days, by type') +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate'));

## H2: Proportion of registered accounts with non-zero edits is reduced.
## Let's plot the data from 2016 onwards, and add a line for ACTRIAL as
## usual, to see if there are indications on changes.

ggplot(prop_nonzero_30[
  as_reg_date >= '2016-01-01' & as_reg_date < '2017-11-15',
  list(prop_nonzero = 100*sum(n_editors)/sum(n_registrations)),
                       by=list(as_reg_date, type=as_create_type)],
       aes(x=as_reg_date, y=prop_nonzero,
           group=type, colour=type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  ggtitle('Accounts editing in first 30 days, since 2016') +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  geom_smooth(method='loess', span=0.2);

## Now, switch to monthly data.
prop_nonzero_30[
  , reg_month := format(as_reg_date, "%Y%m")];
prop_nonzero_30_monthly = prop_nonzero_30[
  , list(num_editors=sum(n_editors), num_registrations=sum(n_registrations)),
  by=c('reg_month', 'as_create_type')];
prop_nonzero_30_monthly[
  , reg_date := as.Date(paste0(reg_month, "01"), format="%Y%m%d")];
prop_nonzero_30_monthly[, prop_editors := 100*num_editors/num_registrations];
prop_nonzero_30_monthly[, type := as_create_type];

ggplot(prop_nonzero_30_monthly[reg_date < '2017-12-01'],
  aes(x=reg_date, y=prop_editors, group=type, colour=type)) +
  geom_line() +
  ggtitle('Accounts editing in first 30 days, monthly by type') +
  xlab('Registration month') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  geom_smooth(method='loess', span=0.2);

## Make a timeseries of the first three months of ACTRIAL and forecast.

## There are two months of missing data in 2011, March and April. There is also
## missing data in February and May, but we will assume that the data that is
## present is a reasonable approximation of the proportions for those months.

prop_nonzero_30_ac_ts = ts(
  c(prop_nonzero_30_monthly[type == 'autocreate' &
                              reg_month <= '201102']$prop_editors,
    c(NA, NA),
    prop_nonzero_30_monthly[type == 'autocreate' &
                              reg_month >= '201105' &
                              reg_month < '201709']$prop_editors),
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(prop_nonzero_30_ac_ts);

## We'll use na.interp to interpolate, which uses an STL decomposition because
## our time series is periodic.
prop_nonzero_30_ac_ts = na.interp(prop_nonzero_30_ac_ts);

## Let's investigate seasonality:
tsdisplay(diff(prop_nonzero_30_ac_ts, 12));

## That doesn't look stationary, let's diff again
tsdisplay(diff(diff(prop_nonzero_30_ac_ts, 12)));

## That looks better! Suggests (0,1,0)[12] as the seasonal component,
## with the non-seaonal part potentially having an MA(4) or MA(9) component,
## and likely AR(2) or AR(4).

prop_nonzero_30_ac_model = auto.arima(
  prop_nonzero_30_ac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(prop_nonzero_30_ac_model);

ggAcf(residuals(prop_nonzero_30_ac_model));
ggPacf(residuals(prop_nonzero_30_ac_model));
Box.test(residuals(prop_nonzero_30_ac_model), lag=36, fitdf=5, type="Ljung");

## The model passes the tests. Let's use it to forecast.
prop_nonzero_30_ac_fc = forecast(prop_nonzero_30_ac_model, h=3);

autoplot(prop_nonzero_30_ac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=prop_nonzero_30_monthly[
        type == 'autocreate' &
          reg_month %in% c('201709', '201710', '201711')]$prop_editors),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 10)) +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast, autocreated accounts editing within 30 days");

## Now, let's forecast for the non-autocreated accounts.

prop_nonzero_30_nac_ts = ts(
  prop_nonzero_30_monthly[type == 'create' & reg_date < '2017-09-01']$prop_editors,
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(prop_nonzero_30_nac_ts);

## Let's investigate seasonality:
tsdisplay(diff(prop_nonzero_30_nac_ts, 12));

## Not as clearly seasonal as the plot for autocreated accounts. This time series
## also appears to be non-stationary, so let's diff again:
tsdisplay(diff(diff(prop_nonzero_30_nac_ts, 12)));

## That looks better! Suggests (0,1,0)[12] as the seasonal component. Looks like
## there are also some strong quarterly effects.

prop_nonzero_30_nac_model = auto.arima(
  prop_nonzero_30_nac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(prop_nonzero_30_nac_model);

ggAcf(residuals(prop_nonzero_30_nac_model));
ggPacf(residuals(prop_nonzero_30_nac_model));
Box.test(residuals(prop_nonzero_30_nac_model), lag=36, fitdf=2, type="Ljung");

## That model doesn't pass the tests. Let's try to do this manually.

## Checked the ACF and PACF for ARIMA(0,1,0)(0,1,0)[12]. Suggested AR(1) and MA(1)
## for the seasonal component.
## Checked the AFC and PACF for ARIMA(0,1,0)(1,1,1)[12]. Seasonal component looked
## meaningfully reduced. MA(8) and AR(3) or AR(8) is suggested.
## Checked the AFC and PACF for ARIMA(8,1,8)(1,1,1)[12]. Passes the test, but is
## maybe overly complicated.
## By iterating through, I found that MA(8) or AR(8) is needed for it to pass
## the tests, otherwise we have a big spike in the ACF and PACF at lag 8.
## We do not need both MA(1) and AR(1) in the seasonal component.
## Comparing AICc and BIC for the various models indicates that MA is preferable
## to AR in both components of the model.

## Preferred model.
prop_nonzero_30_nac_model = Arima(prop_nonzero_30_nac_ts,
                                  order = c(0,1,8),
                                  seasonal = list(order = c(0,1,1),
                                                  period = 12));
summary(prop_nonzero_30_nac_model);

ggAcf(residuals(prop_nonzero_30_nac_model));
ggPacf(residuals(prop_nonzero_30_nac_model));
Box.test(residuals(prop_nonzero_30_nac_model), lag=36, fitdf=10, type="Ljung");

## The model passes the tests. Let's use it to forecast.
prop_nonzero_30_nac_fc = forecast(prop_nonzero_30_nac_model, h=3);

autoplot(prop_nonzero_30_nac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=prop_nonzero_30_monthly[
        type == 'create' &
          reg_month %in% c('201709', '201710', '201711')]$prop_editors),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 45)) +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast, non-autocreated accounts editing within 30 days");

ggplot(prop_nonzero_30[as_create_type == 'autocreate',
                       list(prop_nonzero = sum(n_editors)/sum(n_registrations)),
                       by=list(as_reg_date)],
       aes(x=as_reg_date, y=prop_nonzero*100)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12), breaks=seq(0,12,2)) +
  xlab('Year') +
  ylab('Proportion in %') +
  ggtitle('Proportion of autocreated accounts with non-zero edits');

## Based on the trends in number of autocreated accounts and the proportion
## of accounts making edits, is the number of accounts making edits fairly stable?
ggplot(prop_nonzero_30[as_create_type == 'autocreate'],
       aes(x=as_reg_date, y=n_editors)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 500)) +
  xlab('Year') +
  ylab('Number of editors') +
  ggtitle('Autocreated accounts making edits in the first 30 days') +
  geom_smooth(method='loess', span=0.25);

## H3: Proportion of accounts reaching autoconfirmed status in 30 days is unchanged.

## Proportion of users by date and type that reach autoconfirmed status in 30 days,
## also measuring number of editors because that's perhaps more interesting.
prop_autoconfirmed_30 = merge(merge(
  useractivity[, list(n_registrations=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 > 0, list(n_editors=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')),
  useractivity[as_num_edits_30 >= 10, list(n_autoconfirmed=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));

ggplot(prop_autoconfirmed_30[
  , list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_registrations)),
  by=as_reg_date], aes(x=as_reg_date, y=prop_autoconfirmed)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5), breaks=seq(0, 5, 1)) +
  
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts reaching autoconfirmed status in 30 days");

ggplot(prop_autoconfirmed_30[
  , list(n_autoconfirmed = sum(n_autoconfirmed)),
  by=as_reg_date], aes(x=as_reg_date, y=n_autoconfirmed)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 350), breaks=seq(0, 350, 50)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Number of accounts reaching autoconfirmed status in 30 days");

ggplot(prop_autoconfirmed_30[
  , list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_editors)),
  by=as_reg_date], aes(x=as_reg_date, y=prop_autoconfirmed)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(5, 16), breaks=seq(5, 15, 5)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts with non-zero edits autoconfirmed in 30 days");

## Plot to be uploaded to Commons as Prop autoconfirmed 30 by type 2009-2017.png 
ggplot(prop_autoconfirmed_30[
  as_create_type %in% c('create', 'autocreate'),
  list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_editors)),
  by=list(as_reg_date, type=as_create_type)],
  aes(x=as_reg_date, y=prop_autoconfirmed,
      group=type, colour=type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks=seq(0, 50, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts with non-zero edits autoconfirmed in 30 days");

## Plot from Jan 1, 2016 to November 15, 2017, with ACTRIAL line to look for
## trends:
ggplot(prop_autoconfirmed_30[
  as_reg_date >= '2016-01-01' & as_reg_date < '2017-11-15' &
  as_create_type %in% c('create', 'autocreate'),
  list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_editors)),
  by=list(as_reg_date, type=as_create_type)],
  aes(x=as_reg_date, y=prop_autoconfirmed,
      group=type, colour=type)) + 
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), breaks=seq(0, 30, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts with non-zero edits autoconfirmed in 30 days") +
  geom_smooth(method='loess', span=0.2);

## Now, let's switch to monthly data and check the trends.

prop_autoconfirmed_30[
  , reg_month := format(as_reg_date, "%Y%m")];
prop_autoconfirmed_30_monthly = prop_autoconfirmed_30[
  , list(num_editors=sum(n_editors), num_autoconfirmed=sum(n_autoconfirmed)),
  by=c('reg_month', 'as_create_type')];
prop_autoconfirmed_30_monthly[
  , reg_date := as.Date(paste0(reg_month, "01"), format="%Y%m%d")];
prop_autoconfirmed_30_monthly[, prop_autoconfirmed := 100*num_autoconfirmed/num_editors];
prop_autoconfirmed_30_monthly[, type := as_create_type];

ggplot(prop_autoconfirmed_30_monthly[reg_date < '2017-12-01'],
       aes(x=reg_date, y=prop_autoconfirmed, group=type, colour=type)) +
  geom_line() +
  ggtitle('Proportion of editing accounts autoconfirmed in 30 days, monthly') +
  xlab('Registration month') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  geom_smooth(method='loess', span=0.2);

## Build models for autocreated and non-autocreated accounts.
## The autocreated account time series has the same issue with missing data
## for March and April 2011 as described earlier. We will assume that the
## proportion for February and May are reasonable approximations, and use
## interpolation to add the data for March and April.

prop_autoconfirmed_30_ac_ts = ts(
  c(prop_autoconfirmed_30_monthly[type == 'autocreate' &
                              reg_month <= '201102']$prop_autoconfirmed,
    c(NA, NA),
    prop_autoconfirmed_30_monthly[type == 'autocreate' &
                              reg_month >= '201105' &
                              reg_month < '201709']$prop_autoconfirmed),
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(prop_autoconfirmed_30_ac_ts);

## We'll use na.interp to interpolate, which uses an STL decomposition because
## our time series is periodic.
prop_autoconfirmed_30_ac_ts = na.interp(prop_autoconfirmed_30_ac_ts);
autoplot(prop_autoconfirmed_30_ac_ts);

## Let's investigate seasonality:
tsdisplay(prop_autoconfirmed_30_ac_ts);
tsdisplay(diff(prop_autoconfirmed_30_ac_ts, 12));

## That looks seasonal with a yearly cycle, and stationary. Let's check
## the stationarity just to be sure.
adf.test(diff(prop_autoconfirmed_30_ac_ts, 12), alternative = 'stationary');
## Suggests the data is stationary after removing seasonal effects.

prop_autoconfirmed_30_ac_model.auto = auto.arima(
  prop_autoconfirmed_30_ac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(prop_autoconfirmed_30_ac_model.auto);

ggAcf(residuals(prop_autoconfirmed_30_ac_model.auto));
ggPacf(residuals(prop_autoconfirmed_30_ac_model.auto));
Box.test(residuals(prop_autoconfirmed_30_ac_model.auto), lag=36, fitdf=2, type="Ljung");

## I dislike that peak in the PACF at lag=19. How does a sesonal model fare?

prop_autoconfirmed_30_ac_model = Arima(prop_autoconfirmed_30_ac_ts,
                                       order = c(0,0,0),
                                       seasonal = c(0,1,1));
summary(prop_autoconfirmed_30_ac_model);

ggAcf(residuals(prop_autoconfirmed_30_ac_model));
ggPacf(residuals(prop_autoconfirmed_30_ac_model));
Box.test(residuals(prop_autoconfirmed_30_ac_model), lag=36, fitdf=1, type="Ljung");

prop_autoconfirmed_30_ac_fc = forecast(prop_autoconfirmed_30_ac_model, h=3);

autoplot(prop_autoconfirmed_30_ac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=prop_autoconfirmed_30_monthly[
        type == 'autocreate' &
          reg_month %in% c('201709', '201710', '201711')]$prop_autoconfirmed),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 20)) +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast, autocreated accounts > 0 edits autoconfirmed in < 30 days");

## Same kind of analysis and forecast for non-autocreated accounts.

prop_autoconfirmed_30_nac_ts = ts(
  prop_autoconfirmed_30_monthly[type == 'create' & reg_date < '2017-09-01']$prop_autoconfirmed,
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(prop_autoconfirmed_30_nac_ts);

## Let's investigate seasonality:
tsdisplay(prop_autoconfirmed_30_nac_ts);
tsdisplay(diff(prop_autoconfirmed_30_nac_ts, 12));

## That looks seasonal with a yearly cycle, but not stationary.
## Diffing again to see if that transforms into a stationary time series:
tsdisplay(diff(diff(prop_autoconfirmed_30_nac_ts, 12)));

adf.test(diff(diff(prop_autoconfirmed_30_ac_ts, 12)),
         alternative = 'stationary');
## Suggests that we have achieved stationarity.

prop_autoconfirmed_30_nac_model = auto.arima(
  prop_autoconfirmed_30_nac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(prop_autoconfirmed_30_nac_model);

ggAcf(residuals(prop_autoconfirmed_30_nac_model));
ggPacf(residuals(prop_autoconfirmed_30_nac_model));
Box.test(residuals(prop_autoconfirmed_30_nac_model), lag=36, fitdf=2, type="Ljung");

## I dislike the peak in the ACF and PACF at lag 11. This was not present in the
## seasonal model. Back to the drawing board:

prop_autoconfirmed_30_nac_model = Arima(prop_autoconfirmed_30_nac_ts,
                                       order = c(11,1,0),
                                       seasonal = list(order = c(1,1,1),
                                                       period = 12));
summary(prop_autoconfirmed_30_nac_model);

ggAcf(residuals(prop_autoconfirmed_30_nac_model));
ggPacf(residuals(prop_autoconfirmed_30_nac_model));
Box.test(residuals(prop_autoconfirmed_30_nac_model), lag=36, fitdf=14, type="Ljung");

## The ARIMA(11,1,0)(1,1,1)[12] model seems overly complicated. Through iterations
## building models and examining the ACF/PACF, I ended up with the following model
## that is simpler and meets most criteria. The peak at lag 11 is still present,
## but the residuals appear to be more random.
prop_autoconfirmed_30_nac_model = Arima(prop_autoconfirmed_30_nac_ts,
                                        order = c(1,1,1),
                                        seasonal = list(order = c(1,1,1),
                                                        period = 12));
summary(prop_autoconfirmed_30_nac_model);

ggAcf(residuals(prop_autoconfirmed_30_nac_model));
ggPacf(residuals(prop_autoconfirmed_30_nac_model));
Box.test(residuals(prop_autoconfirmed_30_nac_model), lag=36, fitdf=5, type="Ljung");

prop_autoconfirmed_30_nac_fc = forecast(prop_autoconfirmed_30_nac_model, h=3);

autoplot(prop_autoconfirmed_30_nac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=prop_autoconfirmed_30_monthly[
        type == 'create' &
          reg_month %in% c('201709', '201710', '201711')]$prop_autoconfirmed),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 20)) +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast, non-autocreated accounts > 0 edits autoconfirmed in < 30 days");

## Average number of edits in the first 30 days (both with and without) 0-edits.
avg_edits_30_by_date = merge(
  useractivity[, list(avg_edits_0=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_edits_1=geo.mean(as_num_edits_30)),
               by=list(as_reg_date)],
  by='as_reg_date'
);

avg_edits_30_by_type = merge(
  useractivity[, list(avg_edits_0=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_edits_1=geo.mean(as_num_edits_30)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')
);

## The first plot looks very much like the proportion of accounts making at
## least one edit, just with a different Y-axis. The second plot looks a lot
## like the proportion of accounts making at least one edit that reaches
## autoconfirmed status, except with a different Y-axis.
ggplot(avg_edits_30_by_date, aes(x=as_reg_date, y=avg_edits_0)) + geom_line();
ggplot(avg_edits_30_by_date, aes(x=as_reg_date, y=avg_edits_1)) + geom_line();

## Same for these two, they look similar to some of the other plots.
ggplot(avg_edits_30_by_type[as_create_type %in% c('create', 'autocreate')],
       aes(x=as_reg_date, y=avg_edits_0,
           group=as_create_type, colour=as_create_type)) +
  geom_line();

ggplot(avg_edits_30_by_type[as_create_type %in% c('create', 'autocreate')],
       aes(x=as_reg_date, y=avg_edits_1,
           group=as_create_type, colour=as_create_type)) +
  geom_line();

## Median time to reach autoconfirmed status
autoconfirmed_30_delta = useractivity[!is.na(as_autoconfirmed_30_timestamp),
                                      list(median_delta=median(as_ac_30_delta)),
                                      by=as_reg_date];

autoconfirmed_30_delta_by_type = useractivity[!is.na(as_autoconfirmed_30_timestamp),
                                      list(median_delta=median(as_ac_30_delta)),
                                      by=list(as_reg_date, as_create_type)];

ggplot(autoconfirmed_30_delta, aes(x=as_reg_date, y=median_delta)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks=seq(0, 8, 1)) +
  xlab('Year') + ylab('Time in days') +
  ggtitle("Median time to autoconfirmed status in 30 days");

ggplot(autoconfirmed_30_delta_by_type, aes(x=as_reg_date, y=median_delta,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), breaks=seq(0, 30, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Time in days') +
  ggtitle("Median time to autoconfirmed status in 30 days");

## Plot the most recent years with a line for ACTRIAL:
ggplot(autoconfirmed_30_delta_by_type[
  as_reg_date >= '2016-01-01' & as_reg_date < '2017-11-15'
], aes(x=as_reg_date, y=median_delta,
       group=as_create_type, colour=as_create_type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), breaks=seq(0, 30, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Registration date') + ylab('Time in days') +
  ggtitle("Median time to autoconfirmed status in 30 days");

## If we compare the medians of 2014, 2015, and 2016 against ACTRIAL,
## will we find a difference?

summary(as.numeric(useractivity[
  ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
     (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
     (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')) &
  !is.na(as_autoconfirmed_30_timestamp)]$as_ac_30_delta));
summary(as.numeric(useractivity[
     (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15') &
    !is.na(as_autoconfirmed_30_timestamp)]$as_ac_30_delta));

## Overall, the median is the same. Per type of account?

summary(as.numeric(useractivity[
  ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
     (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
     (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')) &
    !is.na(as_autoconfirmed_30_timestamp) &
    as_create_type == 'create']$as_ac_30_delta));
summary(as.numeric(useractivity[
  (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15') &
    !is.na(as_autoconfirmed_30_timestamp) &
    as_create_type == 'create']$as_ac_30_delta));

summary(as.numeric(useractivity[
  ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
     (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
     (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')) &
    !is.na(as_autoconfirmed_30_timestamp) &
    as_create_type == 'autocreate']$as_ac_30_delta));
summary(as.numeric(useractivity[
  (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15') &
    !is.na(as_autoconfirmed_30_timestamp) &
    as_create_type == 'autocreate']$as_ac_30_delta));

## These look different, are they consistently different? We could just check
## for each month, or something…
t.test(as.numeric(useractivity[as_reg_date >= as.Date('2017-06-01')
                    & as_reg_date < as.Date('2017-07-01')
                    & as_create_type == 'create']$as_ac_30_delta),
       as.numeric(useractivity[as_reg_date >= as.Date('2017-06-01')
                    & as_reg_date < as.Date('2017-07-01')
                    & as_create_type == 'autocreate']$as_ac_30_delta));


## Proportion of new accounts that survive (>= 1 edits in both week 1 & 5)
prop_survived_week_5 = merge(useractivity[, list(n_registrations=sum(.N)),
                                     by=list(as_reg_date, as_create_type)],
                        useractivity[as_num_edits_week_1 > 0
                                     & as_num_edits_week_5 > 0,
                                     list(n_editors=sum(.N)),
                                     by=list(as_reg_date, as_create_type)],
                        by=c('as_reg_date', 'as_create_type'));
prop_survived_week_5 = merge(
  prop_survived_week_5,
  useractivity[as_num_edits_week_1 > 0, list(n_nonzero=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));

ggplot(prop_survived_week_5[
  , list(prop_survived = 100*sum(n_editors)/sum(n_registrations)),
  by=as_reg_date],
       aes(x=as_reg_date, y=prop_survived)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2), breaks=seq(0, 2, 0.5)) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of surviving editors in week 5");

## This plot deliberately looks in favour of the non-autocreated accounts,
## but we know that a much lower proportion of these made any edits.
ggplot(prop_survived_week_5[
  , list(prop_survived = 100*sum(n_editors)/sum(n_registrations)),
                       by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_survived,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.75), breaks=seq(0, 2.5, 0.5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of surviving editors in week 5");

## So, let's fix that by only looking at proportion of surviving users based on
## making at least one edit in the first week.
ggplot(prop_survived_week_5[
  , list(prop_survived = 100*sum(n_editors)/sum(n_nonzero)),
  by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_survived,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0, 20, 5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of surviving editors in week 5");

## Plot the most recent two years:
ggplot(prop_survived_week_5[
  as_reg_date >= '2016-01-01' & as_reg_date < '2017-11-15'
  , list(prop_survived = 100*sum(n_editors)/sum(n_nonzero)),
  by=list(as_reg_date, as_create_type)],
  aes(x=as_reg_date, y=prop_survived,
      group=as_create_type, colour=as_create_type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  geom_smooth(method='loess', span=0.2) +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15), breaks=seq(0, 15, 5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Proportion in %') +
  ggtitle("Proportion of surviving editors in week 5");

## Let's switch to monthly data and see if there are trends there.
## We'll fix the interesting naming convention as well.

prop_survived_week_5[
  , reg_month := format(as_reg_date, "%Y%m")];
prop_survived_week_5_monthly = prop_survived_week_5[
  , list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero)),
  by=c('reg_month', 'as_create_type')];
prop_survived_week_5_monthly[
  , reg_date := as.Date(paste0(reg_month, "01"), format="%Y%m%d")];
prop_survived_week_5_monthly[, prop_surviving := 100*num_survivors/num_editors];
prop_survived_week_5_monthly[, type := as_create_type];

ggplot(prop_survived_week_5_monthly[reg_date < '2017-12-01'],
       aes(x=reg_date, y=prop_surviving, group=type, colour=type)) +
  geom_line() +
  ggtitle('Proportion of surviving editors in week 5') +
  xlab('Registration month') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  geom_smooth(method='loess', span=0.2);

## Let's build forecasting models for the autocreated and non-autocreated
## time series as before!

## As before, we have missing data for March and April of 2011 and will
## interpolate.

prop_survived_week_5_monthly_ac_ts = ts(
  c(prop_survived_week_5_monthly[type == 'autocreate' &
                                    reg_month <= '201102']$prop_surviving,
    c(NA, NA),
    prop_survived_week_5_monthly[type == 'autocreate' &
                                    reg_month >= '201105' &
                                    reg_month < '201709']$prop_surviving),
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(prop_survived_week_5_monthly_ac_ts);

## We'll use na.interp to interpolate, which uses an STL decomposition because
## our time series is periodic.
prop_survived_week_5_monthly_ac_ts = na.interp(prop_survived_week_5_monthly_ac_ts);
autoplot(prop_survived_week_5_monthly_ac_ts);

## Let's investigate seasonality:
tsdisplay(prop_survived_week_5_monthly_ac_ts);

## Not clear that it is seasonal, but it is definitely not stationary.
## We diff to see if that improves things.
tsdisplay(diff(prop_survived_week_5_monthly_ac_ts));

## Does not appear to be seasonal, and appears stationary.
adf.test(diff(prop_survived_week_5_monthly_ac_ts),
         alternative = 'stationary');

## Also looks like an ARIMA(1,1,0) or ARIMA(0,1,1) model will work for this.

prop_survived_week_5_monthly_ac_model.auto = auto.arima(
  prop_survived_week_5_monthly_ac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(prop_survived_week_5_monthly_ac_model.auto);

ggAcf(residuals(prop_survived_week_5_monthly_ac_model.auto));
ggPacf(residuals(prop_survived_week_5_monthly_ac_model.auto));
Box.test(residuals(prop_survived_week_5_monthly_ac_model.auto),
         lag=36, fitdf=1, type="Ljung");

# prop_survived_week_5_monthly_ac_model = Arima(prop_survived_week_5_monthly_ac_ts,
#                                               order = c(0,1,1),
#                                               include.drift = FALSE);
# summary(prop_survived_week_5_monthly_ac_model);
# 
# ggAcf(residuals(prop_survived_week_5_monthly_ac_model));
# ggPacf(residuals(prop_survived_week_5_monthly_ac_model));
# Box.test(residuals(prop_survived_week_5_monthly_ac_model),
#          lag=36, fitdf=1, type="Ljung");

## So it looks like our ARIMA(0,1,1) model without drift works just about
## as well as the same model _with_ drift. Based on the input data, which does
## appear to contain a shift in the mean around 2013, we include the drift term.

prop_survived_week_5_monthly_ac_fc = forecast(
  prop_survived_week_5_monthly_ac_model.auto, h=3);

autoplot(prop_survived_week_5_monthly_ac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=prop_survived_week_5_monthly[
        type == 'autocreate' &
          reg_month %in% c('201709', '201710', '201711')]$prop_surviving),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 8)) +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for proportion of autocreated surviving editors in week 5");

## Out of curiousity, is the peak in September in the recent year something that
## is not following the seasonal trend? If we use STL to decomposition the time
## series, what do the remainders look like?
autoplot(stl(ts(
  prop_survived_week_5_monthly[
    type == 'create' & reg_date < '2017-12-01'][order(reg_month)]$prop_surviving,
  start=c(2009,1), end=c(2017,11), frequency=12),
  s.window = 'periodic'));

## Build a forecast model for non-autocreated accounts.

prop_survived_week_5_monthly_nac_ts = ts(
  prop_survived_week_5_monthly[
    type == 'create' & reg_date < '2017-09-01'][order(reg_month)]$prop_surviving,
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(prop_survived_week_5_monthly_nac_ts);

## Let's investigate seasonality:
tsdisplay(prop_survived_week_5_monthly_nac_ts);

## Peaks at 12 and 24 in the ACF, suggests 12-month cycles.
tsdisplay(diff(prop_survived_week_5_monthly_nac_ts, differences = 12));

## Taking the yearly cycle out makes it look fairly stationary. It's also
## no longer clear that the variance is clearly larger in the recent years.

## Does not appear to be seasonal, and appears stationary.
adf.test(diff(prop_survived_week_5_monthly_nac_ts, differences = 12),
         alternative = 'stationary');

## Also looks like an ARIMA(0,0,3)(0,1,0)[12] model could work for this.

prop_survived_week_5_monthly_nac_model.auto = auto.arima(
  prop_survived_week_5_monthly_nac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(prop_survived_week_5_monthly_nac_model.auto);

ggAcf(residuals(prop_survived_week_5_monthly_nac_model.auto));
ggPacf(residuals(prop_survived_week_5_monthly_nac_model.auto));
Box.test(residuals(prop_survived_week_5_monthly_nac_model.auto),
         lag=36, fitdf=5, type="Ljung");

## The automatic model passes all the tests, but do we really need MA(3) and
## seasonal AR(2)? Note the auto model only uses I(1) for the seasonal part.

prop_survived_week_5_monthly_nac_model = Arima(
  prop_survived_week_5_monthly_nac_ts,
  order = c(0,0,3),
  seasonal = list(
    order = c(2,1,0),
    frequency = 12),
  method="ML");
summary(prop_survived_week_5_monthly_nac_model);
ggAcf(residuals(prop_survived_week_5_monthly_nac_model));
ggPacf(residuals(prop_survived_week_5_monthly_nac_model));
Box.test(residuals(prop_survived_week_5_monthly_nac_model), lag=36,
         fitdf=6, type="Ljung");

## We definitely need MA(3), otherwise we get a spike in the residual AFC and much
## worse training set errors. AR(2) in the sesonal part provides better training
## set statistics, particulary the RMSE. I'll go with that then.

prop_survived_week_5_monthly_nac_fc = forecast(
  prop_survived_week_5_monthly_nac_model.auto, h=3);

autoplot(prop_survived_week_5_monthly_nac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=prop_survived_week_5_monthly[
        type == 'create' &
          reg_month %in% c('201709', '201710', '201711')]$prop_surviving),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 5)) +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for proportion of non-autocreated surviving editors in week 5");

## The actual values are above the forecast for Sept and Nov, and above the 80% CI
## for October. Is this driven by article/draft creators, or something else?
## Looks like it's driven by a peak in early September for non-autocreated accounts,
## per the 2016-2017 plot. During ACTRIAL, the numbers are similar to 2016.

## What if we build a model on 2015 and 2016, to capture the spikes in recent years?

prop_survived_week_5_monthly_nac_ts = ts(
  prop_survived_week_5_monthly[
    type == 'create' & reg_date >= '2015-01-01' & reg_date < '2017-09-01'][order(reg_month)]$prop_surviving,
  start=c(2015,1), end=c(2017,8), frequency=12);
autoplot(prop_survived_week_5_monthly_nac_ts);

## Let's investigate seasonality and stationarity:
tsdisplay(prop_survived_week_5_monthly_nac_ts);
tsdisplay(diff(prop_survived_week_5_monthly_nac_ts));
tsdisplay(diff(prop_survived_week_5_monthly_nac_ts, differences = 12));
adf.test(diff(prop_survived_week_5_monthly_nac_ts, differences = 12),
         alternative = 'stationary');
adf.test(diff(prop_survived_week_5_monthly_nac_ts),
         alternative = 'stationary');

## It's stationary both when diffed, and when taking out the seasonal component.
## So we should examine ARIMA(0,1,2) and ARIMA(2,1,0) models, as well as seasonal
## ones.

prop_survived_week_5_monthly_nac_model.auto = auto.arima(
  prop_survived_week_5_monthly_nac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(prop_survived_week_5_monthly_nac_model.auto);

ggAcf(residuals(prop_survived_week_5_monthly_nac_model.auto));
ggPacf(residuals(prop_survived_week_5_monthly_nac_model.auto));
Box.test(residuals(prop_survived_week_5_monthly_nac_model.auto),
         lag=36, fitdf=1, type="Ljung");

## Not sure I like the 12-month lag in the ACF of the auto-generated model.

prop_survived_week_5_monthly_nac_model = Arima(
  prop_survived_week_5_monthly_nac_ts,
  order = c(0,0,3),
  seasonal = list(order = c(0,1,1),
                  frequency = 12));
summary(prop_survived_week_5_monthly_nac_model);

ggAcf(residuals(prop_survived_week_5_monthly_nac_model));
ggPacf(residuals(prop_survived_week_5_monthly_nac_model));
Box.test(residuals(prop_survived_week_5_monthly_nac_model),
         lag=24, fitdf=3, type="Ljung");

prop_survived_week_5_monthly_nac_fc = forecast(
  prop_survived_week_5_monthly_nac_model, h=3);

autoplot(prop_survived_week_5_monthly_nac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=prop_survived_week_5_monthly[
        type == 'create' &
          reg_month %in% c('201709', '201710', '201711')]$prop_surviving),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 5)) +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for proportion of non-autocreated surviving editors in week 5");

## Answer: Nope, survival in September and November is still outside the forecast.
## It's just much higher. :)

## How about we split each month at day 15, because that'll capture the start
## of ACTRIAL nicely as well, then switch to a frequency of length 24.

prop_survived_week_5[day(as_reg_date) < 15,
                     reg_bimonth := format(as_reg_date, "%Y%m01")];
prop_survived_week_5[day(as_reg_date) >= 15,
                     reg_bimonth := format(as_reg_date, "%Y%m15")];

prop_survived_week_5_bimonthly = prop_survived_week_5[
  , list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero)),
  by=c('reg_bimonth', 'as_create_type')];
prop_survived_week_5_bimonthly[
  , reg_date := as.Date(reg_bimonth, format="%Y%m%d")];
prop_survived_week_5_bimonthly[, prop_surviving := 100*num_survivors/num_editors];
prop_survived_week_5_bimonthly[, type := as_create_type];

ggplot(prop_survived_week_5_bimonthly[reg_date < '2017-12-01'],
       aes(x=reg_date, y=prop_surviving, group=type, colour=type)) +
  geom_line() +
  ggtitle('Proportion of surviving editors in week 5, bimonthly') +
  xlab('Registration month') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  geom_smooth(method='loess', span=0.2);

## Let's try to build a 24-cycle forecasting model.

prop_survived_week_5_bimonthly_nac_ts = ts(
  prop_survived_week_5_bimonthly[
    type == 'create' & reg_date < '2017-09-15']$prop_surviving,
  start=c(2009,1), end=c(2017,8), frequency=24);
autoplot(prop_survived_week_5_bimonthly_nac_ts);

## Let's investigate seasonality:
tsdisplay(prop_survived_week_5_bimonthly_nac_ts);

## Peaks at 24 and 48 in the ACF, suggests 12-month cycles.
tsdisplay(diff(prop_survived_week_5_monthly_ac_ts, differences = 24));

## Taking the yearly cycle out makes it look fairly stationary. It's also
## no longer clear that the variance is clearly larger in the recent years.

## Does not appear to be seasonal, and appears stationary.
adf.test(diff(prop_survived_week_5_monthly_ac_ts, differences = 24),
         alternative = 'stationary');

## Also looks like an ARIMA(0,0,5)(2,1,0)[12] model could work for this.

prop_survived_week_5_bimonthly_nac_model.auto = auto.arima(
  prop_survived_week_5_bimonthly_nac_ts,
  max.order = 10,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(prop_survived_week_5_bimonthly_nac_model.auto);

ggAcf(residuals(prop_survived_week_5_bimonthly_nac_model.auto));
ggPacf(residuals(prop_survived_week_5_bimonthly_nac_model.auto));
Box.test(residuals(prop_survived_week_5_bimonthly_nac_model.auto),
         lag=72, fitdf=8, type="Ljung");

## Model looks reasonably good, although somewhat complex compared to what
## we've seen previously.

prop_survived_week_5_bimonthly_nac_fc = forecast(
  prop_survived_week_5_bimonthly_nac_model.auto, h=4);

autoplot(prop_survived_week_5_bimonthly_nac_fc) +
  geom_line(
    data=data.frame(
      x=2017 + 1/3 + (c(0:3)/24),
      y=prop_survived_week_5_bimonthly[
        type == 'create' &
          reg_bimonth %in% c('20170915', '20171001', '20171015', '20171101')]$prop_surviving),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 5)) +
  xlab('Registration date') + ylab('Proportion in %') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for proportion of non-autocreated surviving editors in week 5");

## I'm not sure we're able to model this in a good way. There are trends in recent
## years that they don't appear to pick up very well. So, how does the first two
## months of ACTRIAL compare to 2014, 2015, and 2016?

prop_survived_week_5[
  (as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero))];
prop_survived_week_5[
  (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero))];

## Should use a t-test for this because variance means something in this dataset.
## Split by account type:
prop_survived_week_5[
  as_create_type == 'create' &
  ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')];
prop_survived_week_5[
  as_create_type == 'create' &
  (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')];

qplot(prop_survived_week_5[
  as_create_type == 'create' &
    ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
       (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')]$prop_survivors, geom = 'histogram');

qplot(prop_survived_week_5[
  as_create_type == 'create' &
    (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')]$prop_survivors, geom = 'histogram');

summary(prop_survived_week_5[
  as_create_type == 'create' &
    ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
       (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')]$prop_survivors);
summary(prop_survived_week_5[
  as_create_type == 'create' &
    (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')]$prop_survivors);

t.test(prop_survived_week_5[
  as_create_type == 'create' &
    ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
       (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')]$prop_survivors,
  prop_survived_week_5[
    as_create_type == 'create' &
      (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'),
    list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
         prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
    by=c('as_reg_date')]$prop_survivors
);

qplot(prop_survived_week_5[
  as_create_type == 'autocreate' &
  ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')]$prop_survivors, geom = 'histogram');
qplot(prop_survived_week_5[
  as_create_type == 'autocreate' &
  (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')]$prop_survivors, geom = 'histogram');

t.test(prop_survived_week_5[
  as_create_type == 'autocreate' &
    ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
       (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15')),
  list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
       prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
  by=c('as_reg_date')]$prop_survivors,
  prop_survived_week_5[
    as_create_type == 'autocreate' &
      (as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'),
    list(num_survivors=sum(n_editors), num_editors=sum(n_nonzero),
         prop_survivors = 100*sum(n_editors)/sum(n_nonzero)),
    by=c('as_reg_date')]$prop_survivors
);

## What's the proportion of newly registered accounts that make edits in the
## first 30 days, but don't make it to autoconfirmed status?
edited_not_autoconfirmed = merge(
  useractivity[as_num_edits_30 > 0, list(n_edited=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 10, list(n_autoconfirmed=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));

ggplot(edited_not_autoconfirmed[, list(prop_not_autoconfirmed = 1 - (sum(n_autoconfirmed)/sum(n_edited))),
                                by=list(as_reg_date)], aes(x=as_reg_date, y=prop_not_autoconfirmed)) +
         geom_line();
ggplot(edited_not_autoconfirmed[, list(prop_not_autoconfirmed = 1 - (sum(n_autoconfirmed)/sum(n_edited))),
                                by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_not_autoconfirmed,
           group=as_create_type, colour=as_create_type)) +
  geom_line();

## Let's do 2016 onwards
ggplot(edited_not_autoconfirmed[as_reg_date >= as.Date('2016-01-01'),
                                list(prop_not_autoconfirmed = 1 - (sum(n_autoconfirmed)/sum(n_edited))),
                                by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_not_autoconfirmed,
           group=as_create_type, colour=as_create_type)) +
  geom_line() + geom_smooth();

## H6: Diversity of participation
num_namespaces_30 = merge(
  useractivity[, list(avg_namespaces_0=mean(as_num_namespaces_30)),
               by=list(as_reg_date)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_namespaces_nonzero=mean(as_num_namespaces_30)),
               by=list(as_reg_date)],
  by=c('as_reg_date')
);
num_namespaces_30_by_type = merge(
  useractivity[, list(avg_namespaces_0=mean(as_num_namespaces_30)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_namespaces_nonzero=mean(as_num_namespaces_30)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')
);
num_namespaces_30_by_type[, type := as_create_type];

num_pages_30 = merge(
  useractivity[, list(avg_pages_0=geo.mean.plus.one(as_num_pages_30)),
               by=list(as_reg_date)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_pages_nonzero=geo.mean.plus.one(as_num_pages_30)),
               by=list(as_reg_date)],
  by=c('as_reg_date')
);
num_pages_30_by_type = merge(
  useractivity[, list(avg_pages_0=geo.mean.plus.one(as_num_pages_30)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_pages_nonzero=geo.mean.plus.one(as_num_pages_30)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')
);
num_pages_30_by_type[, type := as_create_type];

ggplot(num_namespaces_30, aes(x=as_reg_date, y=avg_namespaces_0)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days");

ggplot(num_namespaces_30, aes(x=as_reg_date, y=avg_namespaces_nonzero)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days");

ggplot(num_pages_30, aes(x=as_reg_date, y=avg_pages_0)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days");

ggplot(num_pages_30, aes(x=as_reg_date, y=avg_pages_nonzero)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days");

## Split by type of account creation
ggplot(num_namespaces_30_by_type,
       aes(x=as_reg_date, y=avg_namespaces_0,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days");

## avg_num_ns_nonzero_30_by_type_2009-2017.png
ggplot(num_namespaces_30_by_type,
       aes(x=as_reg_date, y=avg_namespaces_nonzero,
           group=type, colour=type)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days");

## restrict the plot to 2016-2017 and add trend lines:
ggplot(num_namespaces_30_by_type[as_reg_date >= '2016-01-01' &
                                   as_reg_date < '2017-11-15'],
       aes(x=as_reg_date, y=avg_namespaces_nonzero,
           group=type, colour=type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Month') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited in first 30 days 2016-2017") +
  geom_smooth(method='loess', span=0.2);

ggplot(num_pages_30_by_type,
       aes(x=as_reg_date, y=avg_pages_0,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days");

## avg_num_pages_nonzero_30_by_type_2009-2017.png
ggplot(num_pages_30_by_type,
       aes(x=as_reg_date, y=avg_pages_nonzero,
           group=type, colour=type)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days");

## restrict the plot to 2016-2017 and add trend lines
ggplot(num_pages_30_by_type[as_reg_date >= '2016-01-01' &
                              as_reg_date < '2017-11-15'],
       aes(x=as_reg_date, y=avg_pages_nonzero,
           group=type, colour=type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Month') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in first 30 days 2016-2017") +
  geom_smooth(method='loess', span=0.2);

## Note: H6 states "The diversity of participation done by accounts that reach
## autoconfirmed status in the first 30 days is unchanged." We'll have to limit
## this to autoconfirmed accounts.

num_namespaces_ac_30_by_type = useractivity[
  as_num_edits_30 >= 10,
  list(avg_namespaces_autoconfirmed=mean(as_num_namespaces_30)),
  by=list(as_reg_date, as_create_type)];
num_pages_ac_30_by_type = useractivity[
  as_num_edits_30 >= 10,
  list(avg_pages_autoconfirmed=geo.mean.plus.one(as_num_pages_30)),
  by=list(as_reg_date, as_create_type)];
num_namespaces_ac_30_by_type[, type := as_create_type];
num_pages_ac_30_by_type[, type := as_create_type];

ggplot(num_namespaces_ac_30_by_type,
       aes(x=as_reg_date, y=avg_namespaces_autoconfirmed,
           group=type, colour=type)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited by autoconfirmed accounts in first 30 days");

## restrict the plot to 2016-2017 and add trend lines:
ggplot(num_namespaces_ac_30_by_type[as_reg_date >= '2016-01-01' &
                                   as_reg_date < '2017-11-15'],
       aes(x=as_reg_date, y=avg_namespaces_autoconfirmed,
           group=type, colour=type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(limits = c(0,4)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Month') + ylab('Number of namespaces') +
  ggtitle("Average number of namespaces edited by autoconfirmed accounts in first 30 days 2016-2017") +
  geom_smooth(method='loess', span=0.2);

ggplot(num_pages_ac_30_by_type,
       aes(x=as_reg_date, y=avg_pages_autoconfirmed,
           group=type, colour=type)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of pages edited by autoconfirmed accounts in first 30 days");

## restrict the plot to 2016-2017 and add trend lines
ggplot(num_pages_ac_30_by_type[as_reg_date >= '2016-01-01' &
                              as_reg_date < '2017-11-15'],
       aes(x=as_reg_date, y=avg_pages_autoconfirmed,
           group=type, colour=type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Month') + ylab('Number of pages') +
  ggtitle("Average number of pages edited in by autoconfirmed accounts first 30 days 2016-2017") +
  geom_smooth(method='loess', span=0.2);

## These are both count variables, so we can approach it in two ways.
## 1: compare 2014-2016 with ACTRIAL (we use 2014-2016 because the graphs
##    indicate that there are lower means and less variation in earlier years).
## 2: train forecasting models.
## As before, let's do both.

## First, number of namespaces:
qplot(
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
               ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                  (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                  (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_namespaces_30,
  geom='histogram');

## We note that this is skewed and will prefer a Mann-Whitney U test. According to
## the following blog post, a t-test will still work just fine due to the big N.
## http://blog.minitab.com/blog/adventures-in-statistics-2/choosing-between-a-nonparametric-test-and-a-parametric-test

wilcox.test(
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
               ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                  (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                  (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_namespaces_30,
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
               as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_namespaces_30);

t.test(
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
                 ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_namespaces_30,
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
                 as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_namespaces_30);

wilcox.test(
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                 ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_namespaces_30,
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                 as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_namespaces_30);

t.test(
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                 ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_namespaces_30,
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                 as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_namespaces_30);

## Number of pages edited:
wilcox.test(
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
                 ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_pages_30,
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
                 as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_pages_30);

## Log-transform for the t-test due to skewness:
t.test(
  log2(
    useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
                 ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_pages_30),
  log2(
    useractivity[as_num_edits_30 >= 10 & as_create_type == 'autocreate' &
                 as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_pages_30));

wilcox.test(
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                 ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                    (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                    (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_pages_30,
  useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                 as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_pages_30);

## Log-transform for the t-test due to skewness:
t.test(
  log2(
    useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                   ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                      (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                      (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_pages_30),
  log2(
    useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                   as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_pages_30));

## Checking skewness just to be sure:
qplot(useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                     ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                        (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                        (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_pages_30,
      geom='histogram', binwidth=1);
qplot(useractivity[as_num_edits_30 >= 10 & as_create_type == 'create' &
                     as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_pages_30,
      geom='histogram', binwidth=1);

## Number of accounts in each set:
length(useractivity[as_num_edits_30 >= 10 > 0 & as_create_type == 'create' &
                      ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
                         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
                         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))]$as_num_pages_30);
length(useractivity[as_num_edits_30 >= 10 > 0 & as_create_type == 'create' &
                      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_pages_30);

## Given that we found a significant difference, let's switch to monthly data
## to see if there is something in the time series. Let's focus on the number
## of pages, that's arguably more interesting.

useractivity[, as_reg_month := format(as_reg_date, "%Y%m")];

num_pages_ac_30_by_type_monthly = useractivity[
  as_num_edits_30 >= 10,
  list(avg_pages_autoconfirmed=geo.mean.plus.one(as_num_pages_30)),
  by=list(as_reg_month, as_create_type)];
num_pages_ac_30_by_type_monthly[, type := as_create_type];
num_pages_ac_30_by_type_monthly[
  , reg_date := as.Date(paste0(as_reg_month, "01"), format="%Y%m%d")];

ggplot(num_pages_ac_30_by_type_monthly[reg_date < '2017-12-01'],
       aes(x=reg_date, y=avg_pages_autoconfirmed,
           group=type, colour=type)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,8)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Registration month') + ylab('Geometric mean number of pages') +
  ggtitle("Monthly average number of pages edited by autoconfirmed accounts first 30 days") +
  geom_smooth(method='loess', span=0.2);


## As for other hypotheses, we have missing data for March and April of 2011
## and will interpolate.

num_pages_ac_30_by_type_monthly_ac_ts = ts(
  c(num_pages_ac_30_by_type_monthly[type == 'autocreate' &
                                   as_reg_month <= '201102']$avg_pages_autoconfirmed,
    c(NA, NA),
    num_pages_ac_30_by_type_monthly[type == 'autocreate' &
                                   as_reg_month >= '201105' &
                                   as_reg_month < '201709']$avg_pages_autoconfirmed),
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(num_pages_ac_30_by_type_monthly_ac_ts);

## We'll use na.interp to interpolate, which uses an STL decomposition because
## our time series is periodic.
num_pages_ac_30_by_type_monthly_ac_ts = na.interp(num_pages_ac_30_by_type_monthly_ac_ts);
autoplot(num_pages_ac_30_by_type_monthly_ac_ts);

## Let's investigate seasonality:
tsdisplay(num_pages_ac_30_by_type_monthly_ac_ts);

## Not clear that it is seasonal, but it is definitely not stationary.
## We diff to see if that improves things.
tsdisplay(diff(num_pages_ac_30_by_type_monthly_ac_ts));

## Does not appear to be seasonal, and appears stationary.
adf.test(diff(num_pages_ac_30_by_type_monthly_ac_ts),
         alternative = 'stationary');

## Also looks like an ARIMA(0,1,2) model is the first candidate for this.

num_pages_ac_30_by_type_monthly_ac_model.auto = auto.arima(
  num_pages_ac_30_by_type_monthly_ac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(num_pages_ac_30_by_type_monthly_ac_model.auto);

ggAcf(residuals(num_pages_ac_30_by_type_monthly_ac_model.auto));
ggPacf(residuals(num_pages_ac_30_by_type_monthly_ac_model.auto));
Box.test(residuals(num_pages_ac_30_by_type_monthly_ac_model.auto),
         lag=36, fitdf=3, type="Ljung");

# num_pages_ac_30_by_type_monthly_ac_model = Arima(
#   num_pages_ac_30_by_type_monthly_ac_ts, order = c(1,1,2));
# summary(num_pages_ac_30_by_type_monthly_ac_model);
# 
# ggAcf(residuals(num_pages_ac_30_by_type_monthly_ac_model));
# ggPacf(residuals(num_pages_ac_30_by_type_monthly_ac_model));
# Box.test(residuals(num_pages_ac_30_by_type_monthly_ac_model),
#          lag=36, fitdf=2, type="Ljung");

## Not sure we can improve on the auto model, so let's use that.

num_pages_ac_30_by_type_monthly_ac_fc = forecast(
  num_pages_ac_30_by_type_monthly_ac_model.auto, h=3);

autoplot(num_pages_ac_30_by_type_monthly_ac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=num_pages_ac_30_by_type_monthly[
        type == 'autocreate' &
          as_reg_month %in% c('201709', '201710', '201711')]$avg_pages_autoconfirmed),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 8)) +
  xlab('Registration date') + ylab('Average number of pages edited') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for average pages edited by autoconfirmed autocreated accounts");

## Now for non-autocreated accounts
num_pages_ac_30_by_type_monthly_nac_ts = ts(
  num_pages_ac_30_by_type_monthly[type == 'create' &
                                    as_reg_month < '201709']$avg_pages_autoconfirmed,
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(num_pages_ac_30_by_type_monthly_nac_ts);

## Let's investigate seasonality:
tsdisplay(num_pages_ac_30_by_type_monthly_nac_ts);

## Not clear that it is seasonal, but it is definitely not stationary.
## We diff to see if that improves things.
tsdisplay(diff(num_pages_ac_30_by_type_monthly_nac_ts));

## That's clearly seasonal, let's remove the seasonal component and
## check again:
tsdisplay(diff(num_pages_ac_30_by_type_monthly_nac_ts, differences = 12));

## Appear to be seasonal, and appears stationary.
adf.test(diff(num_pages_ac_30_by_type_monthly_ac_ts, differences = 12),
         alternative = 'stationary');

## Suggests an ARIMA (3,x,3)(0,1,1)[12] model.

num_pages_ac_30_by_type_monthly_nac_model.auto = auto.arima(
  num_pages_ac_30_by_type_monthly_nac_ts,
  max.order = 6,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(num_pages_ac_30_by_type_monthly_nac_model.auto);

ggAcf(residuals(num_pages_ac_30_by_type_monthly_nac_model.auto));
ggPacf(residuals(num_pages_ac_30_by_type_monthly_nac_model.auto));
Box.test(residuals(num_pages_ac_30_by_type_monthly_nac_model.auto),
         lag=36, fitdf=4, type="Ljung");

## Tests look good for the autocreated model.

# num_pages_ac_30_by_type_monthly_nac_model = Arima(
#   num_pages_ac_30_by_type_monthly_nac_ts, order = c(0,1,2),
#   seasonal = list(order = c(1,0,1), frequency = 12),
#   include.drift = TRUE);
# summary(num_pages_ac_30_by_type_monthly_nac_model);
# 
# ggAcf(residuals(num_pages_ac_30_by_type_monthly_nac_model));
# ggPacf(residuals(num_pages_ac_30_by_type_monthly_nac_model));
# Box.test(residuals(num_pages_ac_30_by_type_monthly_nac_model),
#          lag=36, fitdf=5, type="Ljung");

## I failed to improve on the auto-generated model.

num_pages_ac_30_by_type_monthly_nac_fc = forecast(
  num_pages_ac_30_by_type_monthly_nac_model.auto, h=3);

autoplot(num_pages_ac_30_by_type_monthly_nac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=num_pages_ac_30_by_type_monthly[
        type == 'create' &
          as_reg_month %in% c('201709', '201710', '201711')]$avg_pages_autoconfirmed),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 6)) +
  xlab('Registration date') + ylab('Average number of pages edited') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for average pages edited by autoconfirmed non-autocreated accounts");

## If we train on data from 2015 onwards, do we capture the peaks in September?
num_pages_ac_30_by_type_monthly_nac_ts = ts(
  num_pages_ac_30_by_type_monthly[type == 'create' &
                                    as_reg_month >= '201501' &
                                    as_reg_month < '201709']$avg_pages_autoconfirmed,
  start=c(2015,1), end=c(2017,8), frequency=12);
autoplot(num_pages_ac_30_by_type_monthly_nac_ts);

## Let's investigate seasonality:
tsdisplay(num_pages_ac_30_by_type_monthly_nac_ts);
tsdisplay(diff(num_pages_ac_30_by_type_monthly_nac_ts, differences = 12));

## Not clear that it's seasonal, but it is stationary?
adf.test(num_pages_ac_30_by_type_monthly_ac_ts,
         alternative = 'stationary');

## Not stationary, check diff:
tsdisplay(diff(num_pages_ac_30_by_type_monthly_nac_ts));
adf.test(diff(num_pages_ac_30_by_type_monthly_ac_ts),
         alternative = 'stationary');

## Stationary when diffing. Suggests ARIMA(1,1,0) or ARIMA(0,1,1).

num_pages_ac_30_by_type_monthly_nac_model.auto = auto.arima(
  num_pages_ac_30_by_type_monthly_nac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(num_pages_ac_30_by_type_monthly_nac_model.auto);

ggAcf(residuals(num_pages_ac_30_by_type_monthly_nac_model.auto));
ggPacf(residuals(num_pages_ac_30_by_type_monthly_nac_model.auto));
Box.test(residuals(num_pages_ac_30_by_type_monthly_nac_model.auto),
         lag=36, fitdf=2, type="Ljung");

## Tests look good for the autocreated model.

num_pages_ac_30_by_type_monthly_nac_model = Arima(
  num_pages_ac_30_by_type_monthly_nac_ts, order = c(0,1,1),
  seasonal = list(order = c(1,1,0), frequency = 12));
summary(num_pages_ac_30_by_type_monthly_nac_model);

ggAcf(residuals(num_pages_ac_30_by_type_monthly_nac_model));
ggPacf(residuals(num_pages_ac_30_by_type_monthly_nac_model));
Box.test(residuals(num_pages_ac_30_by_type_monthly_nac_model),
         lag=36, fitdf=1, type="Ljung");

## AICc and BIC is much lower for ARIMA(0,1,1). Good results when adding a seasonal
## component. No drift due to differencing in both parts of the model.

num_pages_ac_30_by_type_monthly_nac_fc = forecast(
  num_pages_ac_30_by_type_monthly_nac_model, h=3);

autoplot(num_pages_ac_30_by_type_monthly_nac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=num_pages_ac_30_by_type_monthly[
        type == 'create' &
          as_reg_month %in% c('201709', '201710', '201711')]$avg_pages_autoconfirmed),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 6)) +
  xlab('Registration date') + ylab('Average number of pages edited') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for average pages edited by autoconfirmed non-autocreated accounts");

## H7: Average number of edits in first 30 days is reduced.
num_edits_30 = merge(
  useractivity[, list(avg_edits_0=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_edits_nonzero=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date)],
  by=c('as_reg_date')
);
num_edits_30_by_type = merge(
  useractivity[, list(avg_edits_0=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_num_edits_30 >= 1,
               list(avg_edits_nonzero=geo.mean.plus.one(as_num_edits_30)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type')
);

ggplot(num_edits_30,
       aes(x=as_reg_date, y=avg_edits_0)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of edits') +
  ggtitle("Average number of edits in first 30 days");

ggplot(num_edits_30,
       aes(x=as_reg_date, y=avg_edits_nonzero)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  xlab('Year') + ylab('Number of edits') +
  ggtitle("Average number of edits in first 30 days");

ggplot(num_edits_30_by_type,
       aes(x=as_reg_date, y=avg_edits_0,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of pages') +
  ggtitle("Average number of edits in first 30 days");

## avg_num_edits_nonzero_30_by_type_2009-2017.png
num_edits_30_by_type[, type := as_create_type];
ggplot(num_edits_30_by_type,
       aes(x=as_reg_date, y=avg_edits_nonzero,
           group=type, colour=type)) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Year') + ylab('Number of edits') +
  ggtitle("Average number of edits in first 30 days for accounts making edits");

ggplot(num_edits_30_by_type[as_reg_date >= '2016-01-01' &
                              as_reg_date < '2017-11-15'],
       aes(x=as_reg_date, y=avg_edits_nonzero,
           group=type, colour=type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(limits = c(0, 5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Month') + ylab('Number of edits') +
  ggtitle("Average number of edits in first 30 days for accounts making edits, 2016-2017") +
  geom_smooth(method='loess', span=0.2);

## Based on the graph, it appears this number is not affected by ACTRIAL.
## We'll test both looking at ACTRIAL as a whole, and forecasting models.

qplot(
  useractivity[as_create_type == 'autocreate' & as_num_edits_30 >= 1 &
                 as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15']$as_num_edits_30,
  geom = 'histogram');

## Skewed, as expected. We'll log-transform for the t-test, and also use the
## Mann-Whitney test on non-transformed data as well.

## Autocreated accounts:
t.test(
  log2(useractivity[
    as_create_type == 'autocreate' & as_num_edits_30 >= 1 &
      ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))
    ]$as_num_edits_30),
  log2(useractivity[
    as_create_type == 'autocreate' & as_num_edits_30 >= 1 &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'
    ]$as_num_edits_30)
);
wilcox.test(
  useractivity[
    as_create_type == 'autocreate' & as_num_edits_30 >= 1 &
      ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))
    ]$as_num_edits_30,
  useractivity[
    as_create_type == 'autocreate' & as_num_edits_30 >= 1 &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'
    ]$as_num_edits_30
);

## Non-autocreated accounts:
t.test(
  log2(useractivity[
    as_create_type == 'create' & as_num_edits_30 >= 1 &
      ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))
    ]$as_num_edits_30),
  log2(useractivity[
    as_create_type == 'create' & as_num_edits_30 >= 1 &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'
    ]$as_num_edits_30)
);
wilcox.test(
  useractivity[
    as_create_type == 'create' & as_num_edits_30 >= 1 &
      ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
         (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
         (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))
    ]$as_num_edits_30,
  useractivity[
    as_create_type == 'create' & as_num_edits_30 >= 1 &
      as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'
    ]$as_num_edits_30
);

length(useractivity[
  as_create_type == 'create' & as_num_edits_30 >= 1 &
    ((as_reg_date >= '2014-09-15' & as_reg_date < '2014-11-15') |
       (as_reg_date >= '2015-09-15' & as_reg_date < '2015-11-15') |
       (as_reg_date >= '2016-09-15' & as_reg_date < '2016-11-15'))
  ]$as_num_edits_30);
length(useractivity[
  as_create_type == 'create' & as_num_edits_30 >= 1 &
    as_reg_date >= '2017-09-15' & as_reg_date < '2017-11-15'
  ]$as_num_edits_30);

## Switch to monthly data and investigate trends.
num_edits_30_by_type_monthly = useractivity[
  as_num_edits_30 > 0,
  list(geo_mean_edits=geo.mean(as_num_edits_30)),
  by=list(as_reg_month, as_create_type)];
num_edits_30_by_type_monthly[, type := as_create_type];
num_edits_30_by_type_monthly[
  , reg_date := as.Date(paste0(as_reg_month, "01"), format="%Y%m%d")];

ggplot(num_edits_30_by_type_monthly[reg_date < '2017-12-01'],
       aes(x=reg_date, y=geo_mean_edits,
           group=type, colour=type)) +
  geom_line() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits = c(0,3)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Registration month') + ylab('Geometric mean number of edits') +
  ggtitle("Monthly average number of edits in the first 30 days for accounts making edits") +
  geom_smooth(method='loess', span=0.2);

## As for other hypotheses, we have missing data for March and April of 2011
## and will interpolate.

num_edits_30_monthly_ac_ts = ts(
  c(num_edits_30_by_type_monthly[type == 'autocreate' &
                                   as_reg_month <= '201102'
                                 ]$geo_mean_edits,
    c(NA, NA),
    num_edits_30_by_type_monthly[type == 'autocreate' &
                                   as_reg_month >= '201105' &
                                   as_reg_month < '201709'
                                 ]$geo_mean_edits),
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(num_edits_30_monthly_ac_ts);

## We'll use na.interp to interpolate, which uses an STL decomposition because
## our time series is periodic.
num_edits_30_monthly_ac_ts = na.interp(num_edits_30_monthly_ac_ts);
autoplot(num_edits_30_monthly_ac_ts);

## Let's investigate seasonality:
tsdisplay(num_edits_30_monthly_ac_ts);
tsdisplay(diff(num_edits_30_monthly_ac_ts, differences = 12));

## ## Not clear that it is seasonal, so we'll leave that for now.
## Test for stationarity.
adf.test(num_edits_30_monthly_ac_ts,
         alternative = 'stationary');

## Appears to be non-stationary, we'd prefer to see a smaller P-value.
## Let's diff to check:
tsdisplay(diff(num_edits_30_monthly_ac_ts));
adf.test(diff(num_edits_30_monthly_ac_ts),
         alternative = 'stationary');

## Appears be to be stationary after a first difference. The PACF suggests
## an AR(3) model, the ACF suggest maybe an MA(2) model. We'll investigate.

num_edits_30_monthly_ac_model.auto = auto.arima(
  num_edits_30_monthly_ac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(num_edits_30_monthly_ac_model.auto);

ggAcf(residuals(num_edits_30_monthly_ac_model.auto));
ggPacf(residuals(num_edits_30_monthly_ac_model.auto));
Box.test(residuals(num_edits_30_monthly_ac_model.auto),
         lag=36, fitdf=2, type="Ljung");

## The auto-generated model passes all the tests, but has a spike at lag 9.
## Maybe try a seasonal model? Or allow drift?

num_edits_30_monthly_ac_model = Arima(
  num_edits_30_monthly_ac_ts, order = c(0,1,2),
  include.drift = TRUE);
summary(num_edits_30_monthly_ac_model);

ggAcf(residuals(num_edits_30_monthly_ac_model));
ggPacf(residuals(num_edits_30_monthly_ac_model));
Box.test(residuals(num_edits_30_monthly_ac_model),
         lag=36, fitdf=2, type="Ljung");

## Adding drift appears to improve the model, seasonality not so much.
## We'll therefore choose the model with drift.

num_edits_30_monthly_ac_fc = forecast(
  num_edits_30_monthly_ac_model, h=3);

autoplot(num_edits_30_monthly_ac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=num_edits_30_by_type_monthly[
        type == 'autocreate' &
          as_reg_month %in% c('201709', '201710', '201711')]$geo_mean_edits),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 3)) +
  xlab('Registration date') + ylab('Average number of edits') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for average edits by autocreated accounts in first 30 days");

## Now for non-autocreated accounts:

num_edits_30_monthly_nac_ts = ts(
    num_edits_30_by_type_monthly[type == 'create' &
                                   as_reg_month < '201709'
                                 ]$geo_mean_edits,
  start=c(2009,1), end=c(2017,8), frequency=12);
autoplot(num_edits_30_monthly_nac_ts);

autoplot(stl(num_edits_30_monthly_nac_ts, s.window = 'periodic'));

## Let's investigate seasonality:
tsdisplay(num_edits_30_monthly_nac_ts);
tsdisplay(diff(num_edits_30_monthly_nac_ts, differences = 12));

## ## Not clear that it is seasonal, so we'll leave that for now.
## Test for stationarity.
adf.test(num_edits_30_monthly_ac_ts,
         alternative = 'stationary');

## Test suggests its stationary, but the ACF suggests it's not.
## Let's diff to check:
tsdisplay(diff(num_edits_30_monthly_ac_ts));
adf.test(diff(num_edits_30_monthly_ac_ts),
         alternative = 'stationary');

## Appears be to be stationary after a first difference. The PACF suggests
## an AR(3) model, the ACF suggest maybe an MA(2) model, similar to what we saw
## for autocreated accounts. We'll investigate various options.

num_edits_30_monthly_nac_model.auto = auto.arima(
  num_edits_30_monthly_nac_ts,
  stepwise = FALSE, approximation = FALSE, parallel = TRUE);
summary(num_edits_30_monthly_nac_model.auto);

ggAcf(residuals(num_edits_30_monthly_nac_model.auto));
ggPacf(residuals(num_edits_30_monthly_nac_model.auto));
Box.test(residuals(num_edits_30_monthly_nac_model.auto),
         lag=36, fitdf=2, type="Ljung");

## The auto-generated model passes all the tests, but has a spike at lag 11.
## Let's try some other options, e.g. a non-sesonal model with drift.
## Looks like non-seasonal models w/drift perform much worse.
## A seasonal model w/drift performs worse too. Let's keep the auto-model.

# num_edits_30_monthly_nac_model = Arima(
#   num_edits_30_monthly_ac_ts, order = c(1,1,1),
#   seasonal = list(order = c(2,0,0),
#                   frequency = 12),
#   include.drift = TRUE);
# summary(num_edits_30_monthly_nac_model);
# 
# ggAcf(residuals(num_edits_30_monthly_nac_model));
# ggPacf(residuals(num_edits_30_monthly_nac_model));
# Box.test(residuals(num_edits_30_monthly_nac_model),
#          lag=36, fitdf=2, type="Ljung");

num_edits_30_monthly_nac_fc = forecast(
  num_edits_30_monthly_nac_model.auto, h=3);

autoplot(num_edits_30_monthly_nac_fc) +
  geom_line(
    data=data.frame(
      x=2017+c(8:10)/12,
      y=num_edits_30_by_type_monthly[
        type == 'create' &
          as_reg_month %in% c('201709', '201710', '201711')]$geo_mean_edits),
    aes(x=x, y=y, colour='actual')) +
  scale_y_continuous(limits=c(0, 3)) +
  xlab('Registration date') + ylab('Average number of edits') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("ACTRIAL forecast for average edits by non-autocreated accounts in first 30 days");

