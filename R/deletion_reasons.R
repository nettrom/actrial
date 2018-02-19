## Examining reasons for page deletions for the Main, User, and Draft namespaces.

library(data.table);
library(ggplot2);
library(zoo);

deletion_reasons = fread('datasets/deletion_reasons.tsv');

deletion_reasons[, log_date := as.IDate(dr_date)];

## For each namespace, plot the PROD/AFD/other stats

ggplot() +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_r2, colour='R2')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_r3, colour='R3')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_x1, colour='X1')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_prod, colour='PROD')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_afd, colour='AfD')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_other, colour='other')) +
  facet_grid(dr_namespace ~ .) +
  scale_y_log10();

## Usage of General reasons
ggplot() +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g1, colour='G1')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g2, colour='G2')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g3, colour='G3')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g4, colour='G4')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g5, colour='G5')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g6, colour='G6')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g7, colour='G7')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g8, colour='G8')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g9, colour='G9')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g10, colour='G10')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g11, colour='G11')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g12, colour='G12')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_g13, colour='G13')) +
  facet_grid(dr_namespace ~ .) +
  scale_y_log10();

## Usage of Article reasons
ggplot() +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_a1, colour='A1')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_a2, colour='A2')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_a3, colour='A3')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_a5, colour='A5')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_a7, colour='A7')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_a9, colour='A9')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_a10, colour='A10')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_a11, colour='A11')) +
  facet_grid(dr_namespace ~ .) +
  scale_y_log10();

## Usage of User reasons
ggplot() +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_u1, colour='U1')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_u2, colour='U2')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_u3, colour='U3')) +
  geom_line(data=deletion_reasons, aes(x=log_date, y=1+num_u5, colour='U5')) +
  facet_grid(dr_namespace ~ .) +
  scale_y_log10();

## What are the usages of R2, R3, and X1 by namespace?
deletion_reasons[, list(R2=sum(num_r2), R3=sum(num_r3), X1=sum(num_x1)),
                 by=dr_namespace]

## Notes: there are no deletions in the Draft namespace prior to 2013.
## U1-5 are rarely used outside of the User namespace. Rarely enough to ignore them.
## A1-11 are rarely used outside of Main. Some of them pop up a few times on single
## days, it appears, but suggests we should still ignore them.
## Suggests we should make a plot specifically of Draft, including all G-reasons,
## AFD, and "other".
## PROD is not used in the User namespace, AFD and Other is.
## R2 and R3 are used mainly in Main, but also slightly in the other namespaces.
## X1 flags once in User, but otherwise only in Main.

## Note also that the explanation for G6 (https://en.wikipedia.org/wiki/WP:G6)
## suggests that it's not used for deleting _articles_. Similar for G8.

## So...
## Article namespace is G + A + AFD + PROD + other.
## User namespace is G + U + AFD + other.
## Draft namespace is G + AFD + other.
## And note that General does not include G6 or G8.

get_g = function(dataset, namespace) {
  ## For the given dataset of deletions and namespace, split out all
  ## deletions with G-reasons into a new dataset.
  subset = dataset[dr_namespace == namespace];
  rbind(subset[, list(dr_date=log_date, reason='G1', num_deletions=num_g1)],
        subset[, list(dr_date=log_date, reason='G2', num_deletions=num_g2)],
        subset[, list(dr_date=log_date, reason='G3', num_deletions=num_g3)],
        subset[, list(dr_date=log_date, reason='G4', num_deletions=num_g4)],
        subset[, list(dr_date=log_date, reason='G5', num_deletions=num_g5)],
        subset[, list(dr_date=log_date, reason='G7', num_deletions=num_g7)],
        subset[, list(dr_date=log_date, reason='G9', num_deletions=num_g9)],
        subset[, list(dr_date=log_date, reason='G10', num_deletions=num_g10)],
        subset[, list(dr_date=log_date, reason='G11', num_deletions=num_g11)],
        subset[, list(dr_date=log_date, reason='G12', num_deletions=num_g12)],
        subset[, list(dr_date=log_date, reason='G13', num_deletions=num_g13)])
}

get_a = function(dataset, namespace) {
  ## For the given dataset of deletions and namespace, split out all
  ## deletions with A-reasons into a new dataset.
  subset = dataset[dr_namespace == namespace];
  rbind(subset[, list(dr_date=log_date, reason='A1', num_deletions=num_a1)],
        subset[, list(dr_date=log_date, reason='A2', num_deletions=num_a2)],
        subset[, list(dr_date=log_date, reason='A3', num_deletions=num_a3)],
        subset[, list(dr_date=log_date, reason='A5', num_deletions=num_a5)],
        subset[, list(dr_date=log_date, reason='A7', num_deletions=num_a7)],
        subset[, list(dr_date=log_date, reason='A9', num_deletions=num_a9)],
        subset[, list(dr_date=log_date, reason='A10', num_deletions=num_a10)],
        subset[, list(dr_date=log_date, reason='A11', num_deletions=num_a11)])
}

get_u = function(dataset, namespace) {
  ## For the given dataset of deletions and namespace, split out all
  ## deletions with U-reasons into a new dataset.
  subset = dataset[dr_namespace == namespace];
  rbind(subset[, list(dr_date=log_date, reason='U1', num_deletions=num_u1)],
        subset[, list(dr_date=log_date, reason='U2', num_deletions=num_u2)],
        subset[, list(dr_date=log_date, reason='U3', num_deletions=num_u3)],
        subset[, list(dr_date=log_date, reason='U5', num_deletions=num_u5)])
}

main_deletions = rbind(
  get_g(deletion_reasons, 0),
  get_a(deletion_reasons, 0),
  deletion_reasons[dr_namespace == 0,
                   list(dr_date=log_date, reason='PROD', num_deletions=num_prod)],
  deletion_reasons[dr_namespace == 0,
                   list(dr_date=log_date, reason='AfD', num_deletions=num_afd)],
  deletion_reasons[dr_namespace == 0,
                   list(dr_date=log_date, reason='other',
                        num_deletions=num_other+num_u1+num_u2+num_u3+num_u5)]
  
);

user_deletions = rbind(
  get_g(deletion_reasons, 2),
  get_u(deletion_reasons, 2),
  deletion_reasons[dr_namespace == 2,
                   list(dr_date=log_date, reason='AfD', num_deletions=num_afd)],
  deletion_reasons[dr_namespace == 2,
                   list(dr_date=log_date, reason='other',
                        num_deletions=num_other+num_prod+num_a1+num_a2+num_a3+num_a5+num_a7+num_a9+num_a10+num_a11)]
);

draft_deletions = rbind(
  get_g(deletion_reasons, 118),
  deletion_reasons[dr_namespace == 118,
                   list(dr_date=log_date, reason='AfD', num_deletions=num_afd)],
  deletion_reasons[dr_namespace == 118,
                   list(dr_date=log_date, reason='other',
                        num_deletions=num_other+num_prod+num_u1+num_u2+num_u3+num_u5+num_a1+num_a2+num_a3+num_a5+num_a7+num_a9+num_a10+num_a11)]
);

## Looking at the various reasons to reduce the number of dimensions in order to
## make things look sane:
draft_deletions[
  , list(num_deletions=sum(num_deletions),
         percent=100*sum(num_deletions)/sum(draft_deletions$num_deletions)), by=reason][order(num_deletions)];

## One criterium is not used: G9 (office actions).
## That leaves us with 12 reasons. The dataset covers four years, meaning that
## anything under ~1450 deletions is less than once a day. AfD is used slightly
## less than that on average, but is well above the next one. Let's combine
## the lesser used ones (patent nonsense, recreation of a deleted page)
## into "other".
draft_deletions = rbind(
  draft_deletions[reason %in% c('G1', 'G4', 'G9', 'other'),
                  list(reason='other', num_deletions=sum(num_deletions)),
                  by=dr_date],
  draft_deletions[!(reason %in% c('G1', 'G4', 'G9', 'other'))]
);

draft_deletions[
  , num_deletions.ma7 := rollapply(num_deletions,
                                   width=7,
                                   FUN=mean,
                                   na.rm=TRUE,
                                   fill=0,
                                   align='right'),
  by=list(reason)];
draft_deletions[
  , num_deletions.ma28 := rollapply(num_deletions,
                                   width=28,
                                   FUN=mean,
                                   na.rm=TRUE,
                                   fill=0,
                                   align='right'),
  by=list(reason)];
draft_deletions[
  , num_deletions.md7 := rollapply(num_deletions,
                                   width=7,
                                   FUN=median,
                                   na.rm=TRUE,
                                   fill=0,
                                   align='right'),
  by=list(reason)];
draft_deletions[
  , num_deletions.md28 := rollapply(num_deletions,
                                    width=28,
                                    FUN=median,
                                    na.rm=TRUE,
                                    fill=0,
                                    align='right'),
  by=list(reason)];

## Let's order the reasons by number of deletions
draft_deletions$reason = ordered(draft_deletions$reason,
                                 c('AfD', 'G10', 'G5', 'misc', 'G2', 'G3', 'G7',
                                   'G12', 'G11', 'other', 'G13'));

ggplot(draft_deletions,
       aes(x=dr_date, y=1 + num_deletions.ma28,
           fill=reason)) +
  geom_area(alpha=.9) +
  scale_y_log10() +
  xlab('Date') + ylab('Number of deletions') +
  scale_fill_brewer(palette='Set3') +
  ggtitle('Draft deletions, 28-day moving average');

ggplot(draft_deletions,
       aes(x=dr_date, y=1 + num_deletions.md28,
           fill=reason)) +
  geom_area(alpha=.9) +
  scale_y_log10() +
  xlab('Date') + ylab('Number of deletions') +
  scale_fill_brewer(palette='Set3') +
  ggtitle('Draft deletions, 28-day moving median')

## We are particularly interested in the past year to see if something
## happened around ACTRIAL
ggplot(draft_deletions[dr_date >= '2017-01-01'],
       aes(x=dr_date, y=1 + num_deletions.ma28,
           fill=reason)) +
  geom_area() +
  scale_y_log10() +
  xlab('Date') + ylab('Number of deletions (log-scale)') +
  scale_fill_brewer(palette='Set3') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  ggtitle('Draft deletions in 2017, 28-day moving average')

## Overall number of deletions in draft namespace
# ggplot(draft_deletions[, list(num_deletions=sum(num_deletions)), by=dr_date],
#        aes(x=dr_date, y=1+num_deletions)) +
#   geom_line() +
#   scale_y_log10() +
#   xlab('Date') + ylab('Number of deletions (log-scale)') +
#   ggtitle('Total Draft deletions per day')

total_draft_deletions = draft_deletions[
  , list(num_deletions=sum(num_deletions)), by=dr_date][
  , list(log_date=dr_date, num_deletions=num_deletions,
         num_deletions.ma7=rollapply(num_deletions,
                                     width=7,
                                     FUN=mean,
                                     na.rm=TRUE,
                                     fill=0,
                                     align='right'),
         num_deletions.ma28=rollapply(num_deletions,
                                     width=28,
                                     FUN=mean,
                                     na.rm=TRUE,
                                     fill=0,
                                     align='right'),
         num_deletions.md7=rollapply(num_deletions,
                                     width=7,
                                     FUN=median,
                                     na.rm=TRUE,
                                     fill=0,
                                     align='right'),
         num_deletions.md28=rollapply(num_deletions,
                                     width=28,
                                     FUN=median,
                                     na.rm=TRUE,
                                     fill=0,
                                     align='right')
         )];

## While the moving averages and medians are neat, it is arguably easier
## to use LOESS for smoothing a single line. Remember that!

ggplot(total_draft_deletions, aes(x=log_date, y=1+num_deletions)) +
  geom_line() +
  scale_y_log10(
    expand = c(0,0),
    limits = c(1, 15000),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + 
  annotation_logticks(sides='l') +
  theme(panel.grid.minor = element_blank()) + 
  xlab('Date') + ylab('Number of deletions') +
  ggtitle('Draft deletions per day') +
  geom_smooth(method='loess', span=0.2);

## More specifically for Jan 2017 onwards:
ggplot(total_draft_deletions[log_date >= '2017-01-01'],
       aes(x=log_date, y=1+num_deletions)) +
  geom_line() +
  scale_y_log10(
    expand = c(0,0),
    limits = c(1, 15000),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + 
  annotation_logticks(sides='l') +
  theme(panel.grid.minor = element_blank()) +
  xlab('Date') + ylab('Number of deletions') +
  ggtitle('Draft deletions per day, 2017 onwards') +
  geom_smooth(method='loess', span=0.2) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.75);

## What's the distribution of deletions of the top five reasons?

ggplot(draft_deletions[reason %in% c('G13', 'other', 'G11', 'G12', 'G7')],
       aes(x=dr_date, y=1 + num_deletions.ma28,
           colour=reason)) +
  geom_line(alpha=0.8) +
  scale_y_log10() +
  xlab('Date') + ylab('Number of deletions') +
  scale_colour_brewer(palette='Set3') +
  ggtitle('Draft deletions for 5 most common reasons')

for(r in unique(draft_deletions$reason)) {
  print(r);
  print(summary(draft_deletions[reason == r]$num_deletions));
}

## A key result here is that while there's lots more Draft pages created,
## there does not appear to be a similar jump in the number of deletions.
## I'd be curious to know more about why. It also appears that deletions
## happen slightly irregularly, e.g. lots of deletion on one day, then few
## the next day, and so on.

## Let's compare number of deletions per day during ACTRIAL against the same
## time periods in 2015 and 2016:
qplot(log2(1 + total_draft_deletions[
  (log_date >= '2015-09-15' & log_date < '2015-12-01') |
    (log_date >= '2016-09-15' & log_date < '2016-12-01')]$num_deletions),
      geom='histogram', binwidth=0.5);

## While the plot isn't great, it does suggest that deletions across that period
## were roughly normal, with some peaks. Let's compare those two years against
## the same period during ACTRIAL.
summary(total_draft_deletions[
  (log_date >= '2015-09-15' & log_date < '2015-12-01') |
    (log_date >= '2016-09-15' & log_date < '2016-12-01')]$num_deletions);
summary(total_draft_deletions[
  (log_date >= '2017-09-15' & log_date < '2017-12-01')]$num_deletions);

## 2015 vs 2016:
summary(total_draft_deletions[
  (log_date >= '2015-09-15' & log_date < '2015-12-01')]$num_deletions);
summary(total_draft_deletions[
    (log_date >= '2016-09-15' & log_date < '2016-12-01')]$num_deletions);

## Testing ACTRIAL against pre-ACTRIAL period:
t.test(log2(1 + total_draft_deletions[
  (log_date >= '2015-09-15' & log_date < '2015-12-01') |
    (log_date >= '2016-09-15' & log_date < '2016-12-01')]$num_deletions),
  log2(1 + total_draft_deletions[
    (log_date >= '2017-09-15' & log_date < '2017-12-01')]$num_deletions)
);

## It's about 30 pages/day higher than previously (median 86.5 vs 113), and that's
## significant (note the log-transformed t-test).
## At the same time, the number of pages creates in the Draft namespace per day
## has increased 2-2.5 times what it was (7/14 day moving average suggests ~100
## per day prior to ACTRIAL, and 200-250 during ACTRIAL).

## Calculating the proportion of usage of each reason for the two years
## preceeding ACTRIAL.
draft_deletions_by_reason_pre_actrial = draft_deletions[
  year(dr_date) >= 2015 & year(dr_date) <= 2016
  & strftime(dr_date, "%m%d") >= '0915'
  & strftime(dr_date, "%m%d") < '1115',
  list(num_deletions=sum(num_deletions),
       deletions_per_day=round(sum(num_deletions)/(61*2), 3),
       prop=100*sum(num_deletions)/sum(draft_deletions[
         year(dr_date) >= 2015 & year(dr_date) <= 2016
         & strftime(dr_date, "%m%d") >= '0915'
         & strftime(dr_date, "%m%d") < '1115']$num_deletions)), by=reason];

## Calculating the proportion of usage of each reason during ACTRIAL.
draft_deletions_by_reason_actrial = draft_deletions[
  year(dr_date) == 2017
  & strftime(dr_date, "%m%d") >= '0915'
  & strftime(dr_date, "%m%d") < '1115',
  list(num_deletions=sum(num_deletions),
       deletions_per_day=sum(num_deletions)/61,
       prop=100*sum(num_deletions)/sum(draft_deletions[
         year(dr_date) == 2017
         & strftime(dr_date, "%m%d") >= '0915'
         & strftime(dr_date, "%m%d") < '1115']$num_deletions)), by=reason];

draft_deletions_pre_post = draft_deletions_by_reason_actrial[
  , list(reason=reason, post_per_day=deletions_per_day, post_prop=prop)][
    draft_deletions_by_reason_pre_actrial[
      , list(reason=reason, pre_per_day=deletions_per_day, pre_prop=prop)
    ], on='reason'
  ]
draft_deletions_pre_post[, delta_per_day := post_per_day - pre_per_day];
draft_deletions_pre_post[, delta_per_day_prop := 100*delta_per_day/pre_per_day];
draft_deletions_pre_post[, delta_prop := post_prop - pre_prop];

## Round it to two decimals (note that this also rounds delta calculations,
## which can look odd):
data.frame(lapply(draft_deletions_pre_post,
                  function(x) if(is.numeric(x)) round(x, 2) else x))

## Let's do a similar analysis for the User and Main namespaces
total_main_deletions = main_deletions[
  , list(num_deletions=sum(num_deletions)), by=dr_date];

ggplot(total_main_deletions, aes(x=dr_date, y=1+num_deletions)) +
  geom_line() +
  scale_y_log10(
    expand = c(0,0),
    limits = c(1, 25000),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) + 
  annotation_logticks(sides='l') +
  theme(panel.grid.minor = element_blank()) +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Number of deletions') +
  ggtitle('Main namespace deletions per day') +
  geom_smooth(method='loess', span=0.1);

## More specifically for Jan 2017 onwards:
ggplot(total_main_deletions[dr_date >= '2017-01-01'],
       aes(x=dr_date, y=1+num_deletions)) +
  geom_line() +
  scale_y_log10(
    expand = c(0,0),
    limits = c(1, 10000),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + 
  annotation_logticks(sides='l') +
  theme(panel.grid.minor = element_blank()) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of deletions') +
  ggtitle('Main namespace deletions per day since 2017') +
  geom_smooth(method='loess', span=0.1) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.75);

## OK, let's look at reasons for deletion. First, overall:
main_deletions[
  , list(num_deletions=sum(num_deletions),
         percent=100*sum(num_deletions)/sum(main_deletions$num_deletions)),
  by=reason][order(num_deletions)];

## Let's split them into two sets of two dozen:
main_deletions_top12 = main_deletions[
  reason %in% c('A7', 'G11', 'other','AfD', 'PROD', 'G3', 'G12',
                'G7', 'A3', 'A1', 'G10')];
main_deletions_bot12 = main_deletions[
  reason %in% c('G5', 'G2', 'A10', 'G1', 'G4',
                'A11', 'A9', 'A2', 'A5', 'G13', 'G9')];

## Now, order reasons in each one by the number of deletions
main_deletions_top12$reason = ordered(
  main_deletions_top12$reason,
  main_deletions_top12[, list(num_deletions=sum(num_deletions)),
                       by=reason][order(num_deletions)]$reason);
main_deletions_bot12$reason = ordered(
  main_deletions_bot12$reason,
  main_deletions_bot12[, list(num_deletions=sum(num_deletions)),
                       by=reason][order(num_deletions)]$reason);

## Then, plot them historically and for 2017:
ggplot(main_deletions_top12,
       aes(x=dr_date, y=1 + num_deletions,
           fill=reason)) +
  geom_area(alpha=.9) +
  scale_y_log10() +
  xlab('Date') + ylab('Number of deletions') +
  scale_fill_brewer(palette='Set3') +
  ggtitle('Main namespace deletions')

## We are particularly interested in the past year to see if something
## happened around ACTRIAL
ggplot(main_deletions_top12[dr_date >= '2017-01-01'],
       aes(x=dr_date, y=1 + num_deletions,
           fill=reason)) +
  geom_area() +
  scale_y_log10() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of deletions (log-scale)') +
  scale_fill_brewer(palette='Set3') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  ggtitle('Main namespace deletions in 2017')

ggplot(main_deletions_bot12[dr_date >= '2017-01-01'],
       aes(x=dr_date, y=1 + num_deletions,
           fill=reason)) +
  geom_area() +
  scale_y_log10() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of deletions (log-scale)') +
  scale_fill_brewer(palette='Set3') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  ggtitle('Main namespace deletions in 2017')

## There are interesting things going on, but I should smoothen the timeseries.
## I'll apply the 28-day moving median as before, as that works well in case of
## any outliers.
main_deletions_top12[
  , num_deletions.md28 := rollapply(num_deletions,
                                    width=28,
                                    FUN=median,
                                    na.rm=TRUE,
                                    fill=0,
                                    align='right'),
  by=list(reason)];
main_deletions_bot12[
  , num_deletions.md28 := rollapply(num_deletions,
                                    width=28,
                                    FUN=median,
                                    na.rm=TRUE,
                                    fill=0,
                                    align='right'),
  by=list(reason)];

## Let's focus on 2017 for now, as we'll have to either aggregate per week
## or per month for the historic ones.

ggplot(main_deletions_top12[dr_date >= '2017-01-01'],
       aes(x=dr_date, y=1 + num_deletions.md28,
           fill=reason)) +
  geom_area() +
  scale_y_log10() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of deletions (log-scale)') +
  scale_fill_brewer(palette='Set3') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  ggtitle('Main namespace deletions in 2017, 28-day moving median');

ggplot(main_deletions_bot12[dr_date >= '2017-01-01' & !(reason %in% c('G9', 'G13'))],
       aes(x=dr_date, y=1 + num_deletions.md28,
           fill=reason)) +
  geom_area() +
  scale_y_log10() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of deletions (log-scale)') +
  scale_fill_brewer(palette='Set3') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  ggtitle('Main namespace deletions in 2017, 28-day moving median');

## Investigating how the distribution has changed pre-ACTRIAL vs ACTRIAL.
## Due to the delay of deletions in AfD/PROD, I propose we use a two-month
## period, Sept 15 to Nov 15. This gives us a roughly two-month truncation
## of the right side of the graph. Note that this time span covers 61 days.

year = function(date) {
  as.POSIXlt(date)$year + 1900;
}

## Calculating the proportion of usage of each reason for the five years
## preceeding ACTRIAL.
main_deletions[year(dr_date) >= 2012 & year(dr_date) <= 2016
               & strftime(dr_date, "%m%d") >= '0915'
               & strftime(dr_date, "%m%d") < '1115',
               list(num_deletions=sum(num_deletions),
                    deletions_per_day=round(sum(num_deletions)/(61*5), 3),
                    prop=100*sum(num_deletions)/sum(main_deletions[year(dr_date) >= 2012 & year(dr_date) <= 2016
                                                & strftime(dr_date, "%m%d") >= '0915'
                                                & strftime(dr_date, "%m%d") < '1115']$num_deletions)), by=reason];

## Calculating the proportion of usage of each reason during ACTRIAL.
main_deletions[year(dr_date) == 2017
               & strftime(dr_date, "%m%d") >= '0915'
               & strftime(dr_date, "%m%d") < '1115',
               list(num_deletions=sum(num_deletions),
                    deletions_per_day=sum(num_deletions)/61,
                    prop=100*sum(num_deletions)/sum(main_deletions[year(dr_date) == 2017
                                                                   & strftime(dr_date, "%m%d") >= '0915'
                                                                   & strftime(dr_date, "%m%d") < '1115']$num_deletions)), by=reason];

## Based on the proportions and changes, I want to calculate and compare two
## rates. Number of deletions per day prior to and during ACTRIAL, and number
## of speedy deletions per day priort to and during ACTRIAL.

## First, total number of deletions.

## We start by looking at whether the rate per day is roughly Normal in log-space
qplot(log2(1 + total_main_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions),
  geom='histogram', binwidth=0.25);

qplot(log2(1 + total_main_deletions[
  (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions),
  geom='histogram', binwidth=0.25);

## The distribution does not appear to be awkwardly skewed, and it does to
## some extent resemble a Normal. So, let's compare the two and do a t-test.
summary(total_main_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions);

summary(total_main_deletions[
  (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions);

## Testing ACTRIAL against pre-ACTRIAL period:
t.test(log2(1 + total_main_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions),
  log2(1 + total_main_deletions[
    (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions)
);

## What's the change in speedy deletions?
total_main_speedy_deletions = main_deletions[
  !(reason %in% c('PROD', 'AfD', 'other')),
  list(num_deletions=sum(num_deletions)),
  by=dr_date];

## Then we make the same check for a roughly Normal distribution:
qplot(log2(1 + total_main_speedy_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions),
  geom='histogram', binwidth=0.25);

qplot(log2(1 + total_main_speedy_deletions[
  (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions),
  geom='histogram', binwidth=0.25);

## The distribution does not appear to be awkwardly skewed, and it does to
## some extent resemble a Normal. So, let's compare the two and do a t-test.
summary(total_main_speedy_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions);

summary(total_main_speedy_deletions[
  (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions);

## Testing ACTRIAL against pre-ACTRIAL period:
t.test(log2(1 + total_main_speedy_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions),
  log2(1 + total_main_speedy_deletions[
    (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions)
);

## Now, let's look at deletions in the User namespace.

## First we calculate total number of deletions and plot that across time
## as well as around ACTRIAL:
total_user_deletions = user_deletions[
  , list(num_deletions=sum(num_deletions)), by=dr_date];

ggplot(total_user_deletions, aes(x=dr_date, y=1+num_deletions)) +
  geom_line() +
  scale_y_log10(
    expand = c(0,0),
    limits = c(1, 11000),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + 
  annotation_logticks(sides='l') +
  theme(panel.grid.minor = element_blank()) +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Number of deletions') +
  ggtitle('User namespace deletions per day') +
  geom_smooth(method='loess', span=0.1);

## More specifically for Jan 2017 onwards:
ggplot(total_user_deletions[dr_date >= '2017-01-01'],
       aes(x=dr_date, y=1+num_deletions)) +
  geom_line() +
  scale_y_log10(
    expand = c(0,0),
    limits = c(1, 11000),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + 
  annotation_logticks(sides='l') +
  theme(panel.grid.minor = element_blank()) +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of deletions') +
  ggtitle('User namespace deletions per day since 2017') +
  geom_smooth(method='loess', span=0.1) +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.75);

## Overall data on reasons for deletions
user_deletions[
  , list(num_deletions=sum(num_deletions),
         percent=100*sum(num_deletions)/sum(user_deletions$num_deletions)),
  by=reason][order(num_deletions)];

## Similarly as for the Draft namespace, we have several reasons that are used
## sparingly. We combine them with "other" to give us 12 categories in total,
## as that can easily be plotted.
user_deletions = rbind(
  user_deletions[reason %in% c('AfD', 'G1', 'G4', 'G9', 'U3', 'other'),
                  list(reason='other', num_deletions=sum(num_deletions)),
                  by=dr_date],
  user_deletions[!(reason %in% c('AfD', 'G1', 'G4', 'G9', 'U3', 'other'))]
);

## We apply a 28-day moving median as before to smoothen the timeseries
user_deletions[
  , num_deletions.md28 := rollapply(num_deletions,
                                    width=28,
                                    FUN=median,
                                    na.rm=TRUE,
                                    fill=0,
                                    align='right'),
  by=list(reason)];

## Order them by number of deletions:
user_deletions$reason = ordered(
  user_deletions$reason,
  user_deletions[, list(num_deletions=sum(num_deletions)),
                       by=reason][order(num_deletions)]$reason);

## Then plot the moving median historically and for 2017.
ggplot(user_deletions,
       aes(x=dr_date, y=1 + num_deletions.md28,
           fill=reason)) +
  geom_area(alpha=.9) +
  scale_y_log10() +
  scale_x_date(date_breaks='1 year', date_labels = '%Y') +
  xlab('Date') + ylab('Number of deletions') +
  scale_fill_brewer(palette='Set3') +
  ggtitle('User namespace deletions, 28-day moving median');

ggplot(user_deletions[dr_date >= '2017-01-01'],
       aes(x=dr_date, y=1 + num_deletions.md28,
           fill=reason)) +
  geom_area() +
  scale_y_log10() +
  scale_x_date(date_breaks='1 month', date_labels = '%m') +
  xlab('Date') + ylab('Number of deletions (log-scale)') +
  scale_fill_brewer(palette='Set3') +
  geom_vline(xintercept=as.IDate('2017-09-14'), linetype='dashed',
             alpha=0.35) +
  ggtitle('User namespace deletions in 2017, 28-day moving median');

## Run a similar analysis as for the other two namespaces when it comes to
## overall deletion rate and whether it changed during ACTRIAL. Again, we
## reuse the five years prior to ACTRIAL as the rate of deletions in User
## has been fairly stable during that time. There are some spikes, which does
## concern us, but it might also be that those are outliers and won't affect us
## too much. The histogram should give us more information.

## We start by looking at whether the rate per day is roughly Normal in log-space
qplot(log2(1 + total_user_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions),
  geom='histogram', binwidth=0.25);

qplot(log2(1 + total_user_deletions[
  (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions),
  geom='histogram', binwidth=0.25);

## The distribution in User is right-skewed, and more so than for Main and Draft.
## This is somewhat concerning, but at the same time, we do not have many outliers.
## Let's look at the summary statistics:

summary(total_user_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions);

summary(total_user_deletions[
  (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions);

## The right-skewed distribution shows up in the mean being higher than the
## median, as expected. (It's higher than the third quartile during ACTRIAL).
## The IQR suggests our Normal assumption is not too far off from reality.

## Testing ACTRIAL against pre-ACTRIAL period:
t.test(log2(1 + total_user_deletions[
  (dr_date >= '2012-09-15' & dr_date < '2012-11-15') |
    (dr_date >= '2013-09-15' & dr_date < '2013-11-15') |
    (dr_date >= '2014-09-15' & dr_date < '2014-11-15') |
    (dr_date >= '2015-09-15' & dr_date < '2015-11-15') |
    (dr_date >= '2016-09-15' & dr_date < '2016-11-15')]$num_deletions),
  log2(1 + total_user_deletions[
    (dr_date >= '2017-09-15' & dr_date < '2017-11-15')]$num_deletions)
);

## Geometric means:
## Pre-ACTRIAL:
2^7.146060;
## ACTRIAL:
2^7.331234;

## Geometric mean suggests an increase of 20 deletions per day during ACTRIAL,
## which is marginally significant (t=-1.9968, df=74.975, p = 0.049).

## Lastly, let's look at changes in reasons for deletion:
## Calculating the proportion of usage of each reason for the two years
## preceeding ACTRIAL.
user_deletions_by_reason_pre_actrial = user_deletions[
  year(dr_date) >= 2014 & year(dr_date) <= 2016
  & strftime(dr_date, "%m%d") >= '0915'
  & strftime(dr_date, "%m%d") < '1115',
  list(num_deletions=sum(num_deletions),
       deletions_per_day=round(sum(num_deletions)/(61*3), 3),
       prop=100*sum(num_deletions)/sum(user_deletions[
         year(dr_date) >= 2014 & year(dr_date) <= 2016
         & strftime(dr_date, "%m%d") >= '0915'
         & strftime(dr_date, "%m%d") < '1115']$num_deletions)), by=reason];

## Calculating the proportion of usage of each reason during ACTRIAL.
user_deletions_by_reason_actrial = user_deletions[
  year(dr_date) == 2017
  & strftime(dr_date, "%m%d") >= '0915'
  & strftime(dr_date, "%m%d") < '1115',
  list(num_deletions=sum(num_deletions),
       deletions_per_day=sum(num_deletions)/61,
       prop=100*sum(num_deletions)/sum(user_deletions[
         year(dr_date) == 2017
         & strftime(dr_date, "%m%d") >= '0915'
         & strftime(dr_date, "%m%d") < '1115']$num_deletions)), by=reason];

user_deletions_pre_post = user_deletions_by_reason_actrial[
  , list(reason=reason, post_per_day=deletions_per_day, post_prop=prop)][
    user_deletions_by_reason_pre_actrial[
      , list(reason=reason, pre_per_day=deletions_per_day, pre_prop=prop)
      ], on='reason'
    ]
user_deletions_pre_post[, delta_per_day := post_per_day - pre_per_day];
user_deletions_pre_post[, delta_per_day_prop := 100*delta_per_day/pre_per_day];
user_deletions_pre_post[, delta_prop := post_prop - pre_prop];

## Round it to two decimals (note that this also rounds delta calculations,
## which can look odd):
data.frame(lapply(user_deletions_pre_post,
                  function(x) if(is.numeric(x)) round(x, 2) else x))
