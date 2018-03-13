## Various statistics for the final report.

## 1: Avg number of registrations Sept-Nov:
mean(newaccounts_by_month[type == 'create' &
                            na_month %in% c('201709', '201710', '201711'),
                          list(num_regs=sum(n_accounts)), by=na_month]$num_regs);
mean(newaccounts_by_month[type == 'create' &
                            na_month %in% c('201609', '201610', '201611'),
                          list(num_regs=sum(n_accounts)), by=na_month]$num_regs);
100*(153662.7-150989.7)/153662.7;

## Reduction in the NPP backlog
(14353-12371)/14353;

## Graph of retention by month, non-autocreated accounts:
ggplot(prop_survived_week_5_monthly[type == 'create' &
                                      reg_date < '2017-12-01'],
       aes(x=reg_date, y=prop_surviving)) +
  geom_line() +
  ggtitle('Proportion of surviving editors in week 5') +
  xlab('Registration month') + ylab('Proportion in %') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  geom_smooth(method='loess', span=0.2);

## Graph of number of non-autocreated accounts created per month
ggplot(newaccounts_by_month[type == 'create'],
       aes(x=na_date, y=n_accounts)) +
  geom_line() +
  xlab('Year') + ylab('Number of accounts') +
  ggtitle('Accounts registered per month') +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  scale_y_continuous(limits=c(0, 3.5e+05),
                     breaks=c(0,1e+05,2e+05, 3e+05),
                     labels=c("0", "100k", "200k", "300k"));




