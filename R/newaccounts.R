## Plot of historical data on the number of new accounts registered.

library(data.table);
library(ggplot2);
library(forecast);
library(tseries);

# 'datasets/account_registration_counts.tsv'

newaccounts = data.table(read.table('https://tools.wmflabs.org/actrial/datasets/account_registration_counts.tsv',
                                    sep='\t', stringsAsFactors=FALSE, header=TRUE));
newaccounts[, na_date := as.Date(na_date)];

## Calculate totals for columns that don't have it
newaccounts[na_newusers == 0,
            na_newusers := na_autocreate + na_byemail + na_create + na_create2];

## Split the dataset up into separate sets so R can group them
newaccounts_split = rbind(newaccounts[, list(na_date, n_accounts=na_newusers,
                                             count='total')],
                          newaccounts[, list(na_date, n_accounts=na_autocreate,
                                             count='autocreate')],
                          newaccounts[, list(na_date, n_accounts=na_byemail,
                                             count='byemail')],
                          newaccounts[, list(na_date, n_accounts=na_create,
                                             count='create')],
                          newaccounts[, list(na_date, n_accounts=na_create2,
                                             count='create2')]);

ggplot(newaccounts_split, aes(x=na_date, y=n_accounts, group=count,
                              colour=count)) + geom_line() + scale_y_log10() +
  xlab('Year') + ylab('Number of accounts created (log-10 scale)') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y');

## Let's make a plot from 2009 onwards with all four groups, without the total,
## and with a linear Y-axis.
newaccounts_split = rbind(newaccounts[, list(na_date, n_accounts=na_autocreate,
                                             count='autocreate')],
                          newaccounts[, list(na_date, n_accounts=na_byemail,
                                             count='byemail')],
                          newaccounts[, list(na_date, n_accounts=na_create,
                                             count='create')],
                          newaccounts[, list(na_date, n_accounts=na_create2,
                                             count='create2')]);
newaccounts_split = newaccounts_split[na_date >= '2009-01-01'];
ggplot(newaccounts_split, aes(x=na_date, y=n_accounts, group=count,
                              colour=count)) + geom_line() +
  ggtitle("Number of accounts created per day by type") +
  xlab('Year') + ylab('Number of accounts created') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette);

## I also want a separate plot of "byemail" and "create2" because they're a couple
## of orders of magnitude below the others:
ggplot(newaccounts_split[count %in% c('create2', 'byemail')],
       aes(x=na_date, y=n_accounts, group=count, colour=count)) + geom_line() +
  xlab('Year') + ylab('Number of accounts created') +
  ggtitle("Number of accounts created per day by type") +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_colour_manual(values=cbbPalette[c(2,4)]);

## I also want separate plots of the sum of all non-autocreated, with a trend line
ggplot(newaccounts_split[count != "autocreate", list(n_accounts=sum(n_accounts)),
                         by=na_date],
       aes(x=na_date, y=n_accounts)) + geom_line() +
  xlab('Year') + ylab('Number of accounts created') +
  ggtitle("Total number of non-autocreated accounts created") +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  geom_smooth(method='loess', span=0.1)

## …and a separate plot of the autocreated with a trend line
ggplot(newaccounts_split[count == "autocreate"],
       aes(x=na_date, y=n_accounts)) + geom_line() +
  xlab('Year') + ylab('Number of accounts created') +
  ggtitle("Total number of autocreated accounts created") +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  geom_smooth(method='loess', span=0.1)

## We're mainly interested in looking at 2009 onwards, and comparing number of
## autocreated articles to the rest. Lets plot those two
newaccounts_autocreate = rbind(
  newaccounts[na_date >= '2009-01-01',
              list(na_date, n_accounts=na_create+na_create2+na_byemail,
                   type='create')],
  newaccounts[na_date >= '2009-01-01',
              list(na_date, n_accounts=na_autocreate, type='autocreate')]);
ggplot(newaccounts_autocreate, aes(x=na_date, y=n_accounts, group=type,
                                   colour=type)) + geom_line() + xlab('Year') +
  ylab('Number of accounts created') + scale_x_date(date_breaks='1 year',
                                                    date_labels = '%Y');

## Calculate proportion of autocreated accounts:
newaccounts[na_autocreate > 0, prop_autocreate := na_autocreate/na_newusers];
ggplot(subset(newaccounts, na_date >= '2009-01-01'),
              aes(x=na_date, y=100*prop_autocreate)) + geom_line() +
  xlab('Year') + scale_x_date(date_breaks='1 year', date_labels='%Y') +
  ylab('Percent of total accounts autocreated');

## Give me some summary statistics and boxplots of the number of accounts
## created for others, and that get their passwords through email.
## These appear to have been fairly stable since 2014 onwards.
summary(newaccounts[na_date >= '2014-01-01']$na_byemail);
summary(newaccounts[na_date >= '2014-01-01']$na_create2);

ggplot(newaccounts_split[na_date >= '2014-01-01' 
                         & count %in% c('create2', 'byemail')],
       aes(count, n_accounts)) + geom_boxplot() + xlab('Type of account creation') +
  ylab('Number of accounts created');

## I need a plot of the number of autocreated vs others historically, and from
## 2016 onwards with a vertical line showing when ACTRIAL started.
ggplot(newaccounts_autocreate[na_date < '2018-01-01'],
       aes(x=na_date, y=n_accounts, group=type,
           colour=type)) +
  geom_line() +
  xlab('Year') + ylab('Number of accounts created') +
  ggtitle('Accounts registered per day') +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  geom_smooth(method='loess', span=0.1) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate'));

## I also want specifically a plot of the number of autocreated vs others
## for 2016 onwards.
ggplot(newaccounts_autocreate[na_date >= '2016-01-01' & na_date < '2018-01-01'],
       aes(x=na_date, y=n_accounts, group=type,
colour=type)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed', alpha=0.75) +
  geom_line() +
  xlab('Year') + ylab('Number of accounts created') +
  ggtitle('Accounts registered in 2016 and 2017') +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_smooth(method='loess', span=0.2) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate'));

## Now, can we plot this on a monthly basis:
newaccounts_autocreate[, na_month := format(na_date, "%Y%m")];
newaccounts_by_month = newaccounts_autocreate[
  , list(n_accounts = sum(n_accounts)), by=c('type', 'na_month')];
newaccounts_by_month[
  , na_date := as.Date(paste0(na_month, "01"), format="%Y%m%d")];

ggplot(newaccounts_by_month,
       aes(x=na_date, y=n_accounts, group=type,
           colour=type)) +
  geom_line() +
  xlab('Year') + ylab('Number of accounts') +
  ggtitle('Accounts registered per month') +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 3.5e+05),
    breaks=c(0,1e+05,2e+05, 3e+05),
                     labels=c("0", "100k", "200k", "300k")) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate'));

## We have 0 autocreated accounts in March and April 2011. Based on the data in
## February and April, those months also appear underreported. Let's calculate
## the means from 2009 and 2010 and scale Feb & March according to January 2011,
## and April and May according to June 2011.
newaccounts_by_month[
  na_month == '201102' & type == 'autocreate',
  n_accounts := 
    round((newaccounts_by_month[type == 'autocreate' & na_month == '201101']$n_accounts/
             ((newaccounts_by_month[type == 'autocreate' & na_month == '201001']$n_accounts +
                 newaccounts_by_month[type == 'autocreate' & na_month == '200901']$n_accounts)/2))*
            ((newaccounts_by_month[type == 'autocreate' & na_month == '201002']$n_accounts +
                newaccounts_by_month[type == 'autocreate' & na_month == '200902']$n_accounts)/2), 0)]
newaccounts_by_month[
  na_month == '201103' & type == 'autocreate',
  n_accounts := 
    round((newaccounts_by_month[type == 'autocreate' & na_month == '201102']$n_accounts/
             ((newaccounts_by_month[type == 'autocreate' & na_month == '201002']$n_accounts +
                 newaccounts_by_month[type == 'autocreate' & na_month == '200902']$n_accounts)/2))*
            ((newaccounts_by_month[type == 'autocreate' & na_month == '201003']$n_accounts +
                newaccounts_by_month[type == 'autocreate' & na_month == '200903']$n_accounts)/2), 0)]
newaccounts_by_month[
  na_month == '201104' & type == 'autocreate',
  n_accounts := 
    round((newaccounts_by_month[type == 'autocreate' & na_month == '201103']$n_accounts/
             ((newaccounts_by_month[type == 'autocreate' & na_month == '201003']$n_accounts +
                 newaccounts_by_month[type == 'autocreate' & na_month == '200903']$n_accounts)/2))*
            ((newaccounts_by_month[type == 'autocreate' & na_month == '201004']$n_accounts +
                newaccounts_by_month[type == 'autocreate' & na_month == '200904']$n_accounts)/2), 0)]
newaccounts_by_month[
  na_month == '201105' & type == 'autocreate',
  n_accounts := 
    round((newaccounts_by_month[type == 'autocreate' & na_month == '201104']$n_accounts/
             ((newaccounts_by_month[type == 'autocreate' & na_month == '201004']$n_accounts +
                 newaccounts_by_month[type == 'autocreate' & na_month == '200904']$n_accounts)/2))*
            ((newaccounts_by_month[type == 'autocreate' & na_month == '201005']$n_accounts +
                newaccounts_by_month[type == 'autocreate' & na_month == '200905']$n_accounts)/2), 0)]

## Let's make timeseries data out of the created and autocreated monthly data
## and see how it works out.
autocreated_by_month_ts = ts(newaccounts_by_month[
  type == 'autocreate' & na_date <= '2018-01-01']$n_accounts,
                             start=c(2009,1), end=c(2017,12), frequency=12);
autoplot(autocreated_by_month_ts);

created_by_month_ts = ts(newaccounts_by_month[
  type == 'create' & na_date <= '2018-01-01']$n_accounts,
  start=c(2009,1), end=c(2017,12), frequency=12);
autoplot(created_by_month_ts) + ggtitle('Non-autocreated accounts registered') +
  xlab("Year") + ylab("Number of acconts") +
  scale_y_continuous(limits=c(0, 4e+05), breaks=c(0:4)*1e+05,
                     labels=c('0', '100k', '200k', '300k', '400k'));


## Decomposition, let's try the additive one first:
decomp_add = stl(autocreated_by_month_ts, s.window='periodic');
autoplot(decomp_add);

decomp_mult = decompose(autocreated_by_month_ts, "multiplicative");
autoplot(decomp_mult);

## I'm not certain that this needs a multiplicative decomposition. If we keep
## it additive, then the remainder has some large swings during the account
## alignment project, but they return back to normal afterwards.

## Similar decomposition of the number of non-autoconfirmed accounts created,
## I suspect that one is more clearly additive.
decomp_add = stl(created_by_month_ts, s.window='periodic');
autoplot(decomp_add);

decomp_mult = decompose(created_by_month_ts, "multiplicative");
autoplot(decomp_mult);

## The result is similar to the decomposition of the autocreated account timeseries.

## Plot the autocorrelation function:
ggAcf(autocreated_by_month_ts) +
  ggtitle('AFC for autocreated accounts registered per month')

## This autocorrelation function does not decay, suggesting the time series is
## nonstationary.

autocreated_diff_1 = diff(autocreated_by_month_ts, differences=1);
autoplot(autocreated_diff_1);

## Check the autocorrelation:
ggAcf(autocreated_diff_1);

autocreated_diff_2 = diff(autocreated_diff_1, differences=2);
Acf(autocreated_diff_2);

Pacf(autocreated_diff_1);

auto.arima(autocreated_by_month_ts);
auto.arima(autocreated_by_month_ts, ic="bic");

## The BIC suggests a much simpler model where the non-seasonal part is
## both non-autoregressive and non-moving average. AICc suggests having a
## model based on first-order AR and MA. Both variants agree on the seasonal
## part of the model: second-order autoregressive with a 12-month period.

autocreated_actrial_ts = window(autocreated_by_month_ts, start=c(2017,9));
autocreated_preactrial_ts = window(autocreated_by_month_ts, end=c(2017,8));

## We forecast for Sept, Oct, and Nov, capturing the first two whole months
## of the trial.
autocreated_model = auto.arima(autocreated_preactrial_ts, ic="bic");
autocreated_actrial_fc = forecast(autocreated_model, h=3);

autoplot(autocreated_actrial_fc) +
  geom_line(aes(x=2017+c(8:10)/12, y=autocreated_actrial_ts[1:3],
                colour='actual')) +
  scale_y_continuous(limits=c(0, 1.2e+05),
                   breaks=c(0:12)*1e+04,
                   labels=c("0", "10k", "20k", "30k", "40k", "50k", "60k",
                            "70k", "80k", "90k", "100k", "110k", "120k")) +
  xlab('Year') + ylab('Number of accounts registered') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("Autocreated account registration forecast for ACTRIAL")

## Do a similar analysis of the non-autocreated accounts.
ggAcf(created_by_month_ts) +
  ggtitle('AFC for non-autocreated accounts registered per month')

ggPacf(created_by_month_ts);

## This looks also non-stationary (as we'd expect). Let's diff:
created_diff_1 = diff(created_by_month_ts, differences=1);
autoplot(created_diff_1);

## Augmented Dickey-Fuller test for stationarity:
adf.test(created_by_month_ts, alternative='stationary')
## Note that the formal test suggests the data is non-stationary.
## At the same time, auto.arima suggests a simle model that appears to work well.

## Check the autocorrelation:
ggAcf(created_diff_1);

## That looks much nicer than the autocreated plot. Suggests a fairly
## straightforward ARIMA model?
auto.arima(created_by_month_ts);
auto.arima(created_by_month_ts, ic="bic");

## The results appear more straightforward than the previous ones.
## Note that this model does not have the integration built in, instead
## we shift the mean and the values differ around that. Since the AIC, AICc,
## BIC, and log-likelihood are so close, I go with the simpler model.

created_actrial_ts = window(created_by_month_ts, start=c(2017,9));
created_preactrial_ts = window(created_by_month_ts, end=c(2017,8));

created_model = auto.arima(created_preactrial_ts, ic="bic");
created_actrial_fc = forecast(created_model, h=3);

autoplot(created_actrial_fc) +
  geom_line(aes(x=2017+c(8:10)/12, y=created_actrial_ts[1:3],
                colour='actual')) +
  scale_y_continuous(limits=c(0, 3.5e+05),
                     breaks=c(0,1e+05,2e+05, 3e+05),
                     labels=c("0", "100k", "200k", "300k")) +
  xlab('Year') + ylab('Number of accounts registered') +
  scale_color_manual(values=c(actual="red")) +
  guides(level=guide_legend(title="CI"),
         colour=guide_legend(title="")) +
  ggtitle("Non-autocreated account registration forecast for ACTRIAL");
