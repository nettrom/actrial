## Plot of historical data on the number of new accounts registered.

source('R/util.R');

newaccounts = data.table(read.table('datasets/newaccounts-20170925.tsv',
                                    sep='\t', stringsAsFactors=FALSE, header=TRUE));
newaccounts[, na_date := as.Date(na_date)];

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
