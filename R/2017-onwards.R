## We want to get some ideas about what's currently going on with our statistics,
## so let's make some plots that are only showing 2017, with a line for when
## ACTRIAL started.

## H2: Proportion of accounts with non-zero edits in the first 30 days.
## We split this by account type (autocreated/non-autocreated).
ggplot(prop_nonzero_30[as_reg_date >= '2017-01-01',
                       list(prop_nonzero = 100*sum(n_editors)/sum(n_registrations)),
                       by=list(as_reg_date, type=as_create_type)],
       aes(x=as_reg_date, y=prop_nonzero,
           group=type, colour=type)) + geom_line() +
  ggtitle('Proportion of accounts with non-zero edits by type') +
  xlab('Month') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate'));

## H3: Proportion of accounts with non-zero edits that reached autoconfirmed
## status in the first 30 days, by type of account creation:
ggplot(prop_autoconfirmed_30[as_reg_date >= '2017-01-01',
  list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_editors)),
  by=list(as_reg_date, type=as_create_type)],
  aes(x=as_reg_date, y=prop_autoconfirmed,
      group=type, colour=type)) + geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks=seq(0, 50, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Month') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts with non-zero edits autoconfirmed in 30 days");

## H5: Proportion of surviving editors:
ggplot(prop_survived_week_5[as_reg_date >= '2017-01-01'
  , list(prop_survived = 100*sum(n_editors)/sum(n_nonzero)),
                            by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_survived,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20), breaks=seq(0, 20, 5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Month') + ylab('Proportion in %') +
  ggtitle("Proportion of surviving editors in week 5");

## Maybe I want this from 2016 onwards, because then I can see if there were
## seasonal trends in 2016.

# H1: Account registrations
ggplot(newaccounts_autocreate[na_date >= '2016-01-01'],
       aes(x=na_date, y=n_accounts, group=type,
                                   colour=type)) + geom_line() +
  xlab('Month') + ylab('Number of accounts created') + 
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7500)) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  geom_smooth(method='loess', span=0.2) +
  ggtitle("Number of accounts created per day");


# H2:
ggplot(prop_nonzero_30[as_reg_date >= '2016-01-01',
                       list(prop_nonzero = 100*sum(n_editors)/sum(n_registrations)),
                       by=list(as_reg_date, type=as_create_type)],
       aes(x=as_reg_date, y=prop_nonzero,
           group=type, colour=type)) + geom_line() +
  ggtitle('Proportion of accounts with non-zero edits by type from 2016 onwards') +
  xlab('Month') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  geom_smooth(method='loess', span=0.2) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate'));

# H3:
ggplot(prop_autoconfirmed_30[as_reg_date >= '2016-01-01',
                             list(prop_autoconfirmed = 100*sum(n_autoconfirmed)/sum(n_editors)),
                             by=list(as_reg_date, type=as_create_type)],
       aes(x=as_reg_date, y=prop_autoconfirmed,
           group=type, colour=type)) + geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), breaks=seq(0, 30, 10)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  geom_smooth(method='loess', span=0.2) +
  xlab('Month') + ylab('Proportion in %') +
  ggtitle("Proportion of accounts with non-zero edits autoconfirmed in 30 days");

## H5: Proportion of surviving editors:
ggplot(prop_survived_week_5[as_reg_date >= '2016-01-01'
                            , list(prop_survived = 100*sum(n_editors)/sum(n_nonzero)),
                            by=list(as_reg_date, as_create_type)],
       aes(x=as_reg_date, y=prop_survived,
           group=as_create_type, colour=as_create_type)) + geom_line() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_vline(xintercept=as.Date('2017-09-14'), linetype='dashed') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15), breaks=seq(0, 15, 5)) +
  scale_colour_manual(values=cbbPalette, breaks=c('create', 'autocreate')) +
  xlab('Month') + ylab('Proportion in %') +
  geom_smooth(method='loess', span=0.2) +
  ggtitle("Proportion of surviving editors in week 5");

## Number of patrol actions
ggplot(patrol_summary[log_date >= as.IDate('2016-01-01')],
       aes(x=log_date, y=ps_num_patrol_actions)) + geom_line() +
  ggtitle('Patrol actions per day') +
  xlab('Year') + ylab('Number of patrol actions') +
  scale_y_continuous(expand = c(0,0), limits=c(0, 2000)) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_smooth(method='loess', span=0.1);

## Number of active patrollers
ggplot(patrol_summary[log_date >= as.IDate('2016-01-01')],
       aes(x=log_date, y=ps_num_patrollers)) + geom_line() +
  ggtitle('Active patrollers per day') +
  xlab('Year') + ylab('Number of active patrollers') +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 175)) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  geom_smooth(method='loess', span=0.1);

## Proportion of patrol actions done by the top 25%
ggplot(top_patrollers[log_date >= as.IDate('2016-01-01'),
                      list(log_date, prop_top=100*top25_actions/total_actions)],
       aes(x=log_date, y=prop_top)) + geom_line() +
  ggtitle('Prop. of all patrol actions done by most active quartile since 2016') +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=seq(0,100,10)) +
  xlab('Month') + ylab('Proportion in percent') + 
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed') +
  geom_smooth(method='loess', span=0.1);
