## Preliminary analysis of whether new accounts start out by creating an article

library(data.table);
library(ggplot);

## This analysis assumes that `useractivity` from useractivity.R is loaded
## into memory.

## Q1: Overall, what percentage of users who make at least one edit in their
##     first 30 days started out by creating an article?

100*length(useractivity[as_num_edits_30 > 0]$as_userid)/length(useractivity$as_userid);
100*length(useractivity[as_created_article == 1]$as_userid)/length(useractivity[as_num_edits_30 > 0]$as_userid);

## Q2: Has the overall percentage of users who create an article changed over time?
create_prop_overall = merge(useractivity[as_num_edits_30 > 0,
                                         list(n_editors=sum(.N)),
                                         by=list(as_reg_date, as_create_type)],
                            useractivity[as_created_article == 1,
                                         list(n_creators=sum(.N)),
                                         by=list(as_reg_date, as_create_type)],
                            by=c('as_reg_date', 'as_create_type'));

ggplot(create_prop_overall[
  , list(prop_creators = sum(n_creators)/sum(n_editors)), by=as_reg_date],
  aes(x=as_reg_date, y=100*prop_creators)) + geom_line() +
  ggtitle('Proportion of accounts creating articles') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  xlab('Year') + ylab('Proportion in %');

## What's going on in this graph? We've seen the same pattern in the article
## creation graph, is there something in the historic data that we've gathered
## that results in this dip between Q1 2012 and end of Q2 2014?
## We know that the number of accounts created in that period was fairly stable,
## so it's not altered by that. There's not a significant change in the proportion
## of accounts that made edits across the same time (e.g. a huge increase).
## Thus, I suspect it's related to the plateau in article creations, which
## corresponds in time with this dip.

## What's the diff in number of account creators per day between Jan 1, 2014
## and July 1, 2014?
create_prop_overall[as_reg_date == '2014-01-01', sum(n_creators)];
create_prop_overall[as_reg_date == '2014-07-01', sum(n_creators)];
## It's 19 vs 130. That's roughly the same difference as the diff in average number
## of created articles (975 vs 1,100). Looks like something's going on there then.

## Q2: Difference in proportions between how the account was created.

## Sub-part 1: difference in proportion of users making edits between types
## of account creations:
## Sub-part 2: difference in proportion of users creating articles between
## types of account creations:

create_prop_by_type = merge(
  merge(useractivity[, list(n_accounts=sum(.N)), by=as_create_type],
        useractivity[as_num_edits_30 > 0, list(n_editors=sum(.N)), by=as_create_type],
        by='as_create_type'),
  useractivity[as_created_article == 1, list(n_creators=sum(.N)), by=as_create_type],
  by='as_create_type');

## Overall proportions by type:
create_prop_by_type[, list(as_create_type,
                           prop_editors=100*n_editors/n_accounts,
                           prop_creators=100*n_creators/n_editors)];

## How has the proportion by type changed over time? We can just reuse the overall
## plot and facet it by creation type. Note that in this case, we've gone back
## and collapsed the creation types and recalculated `create_prop_overall`.

ggplot(create_prop_overall[
  , list(prop_creators = sum(n_creators)/sum(n_editors)), by=list(as_reg_date, as_create_type)],
  aes(x=as_reg_date, y=100*prop_creators)) + geom_line() +
  ggtitle('Proportion of accounts creating articles') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
  xlab('Year') + ylab('Proportion in %') + facet_grid(as_create_type ~ .);

## Proportion of article creators among autocreated accounts in first half of 2017:
create_prop_overall[as_reg_date >= '2017-01-01' & as_create_type == 'autocreate',
                    list(prop_creators=100*sum(n_creators)/sum(n_editors))];
## And for other types of accounts:
create_prop_overall[as_reg_date >= '2017-01-01' & as_create_type == 'create',
                    list(prop_creators=100*sum(n_creators)/sum(n_editors))];

## Q3: If they create an article, does it survive?
create_pop_articlesurvival = merge(
  useractivity[as_created_article == 1, list(n_creators=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  useractivity[as_created_article == 1 & as_article_survived == 1,
               list(n_article_survivors=sum(.N)),
               by=list(as_reg_date, as_create_type)],
  by=c('as_reg_date', 'as_create_type'));

## First overall, then check by type
create_pop_articlesurvival[
  , list(prop_survival=100*sum(n_article_survivors)/sum(n_creators))];

## 75.8% seems surprisingly high, let's look at how it goes across time:
ggplot(create_pop_articlesurvival[
  , list(prop_survivors = 100*sum(n_article_survivors)/sum(n_creators)), by=list(as_reg_date)],
  aes(x=as_reg_date, y=prop_survivors)) + geom_line() +
  ggtitle('Proportion of created articles surviving') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105), breaks=seq(0,100, 10)) +
  xlab('Year') + ylab('Proportion in %');

## Okay, so that might explain why our article creation numbers are way off.
## If no articles were deleted before mid-2014, it means our article creation
## data doesn't contain deleted articles before then. We now know what to look
## for when fixing our data problem.

## While we wait to gather new data, let's look at article survival from Q3
## 2014 onwards:
create_pop_articlesurvival[as_reg_date >= '2014-07-01'
  , list(prop_survival=100*sum(n_article_survivors)/sum(n_creators))];
create_pop_articlesurvival[
  as_reg_date >= '2014-07-01',
  list(prop_survival=100*sum(n_article_survivors)/sum(n_creators)),
  by=as_create_type];

ggplot(create_pop_articlesurvival[
  as_reg_date >= '2014-07-01',
  list(prop_survivors = 100*sum(n_article_survivors)/sum(n_creators)),
  by=list(as_reg_date, as_create_type)],
  aes(x=as_reg_date, y=prop_survivors)) + geom_line() +
  ggtitle('Proportion of created articles surviving') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105), breaks=seq(0,100, 10)) +
  xlab('Year') + ylab('Proportion in %') + facet_grid(as_create_type ~ .)

## Q4: How does article creation affect editor survival?

## We look at: Number of accounts that edited in their first week and either
## created an article, or did not, and whether they edited in the fifth week.

create_cont_matrix = matrix(
  c(length(useractivity[as_reg_date >= '2014-07-01' & as_num_edits_week_1 > 0 & as_created_article == 0]$as_userid),
    length(useractivity[as_reg_date >= '2014-07-01' & as_num_edits_week_5 > 0 & as_created_article == 0]$as_userid),
    length(useractivity[as_reg_date >= '2014-07-01' & as_num_edits_week_1 > 0 & as_created_article == 1]$as_userid),
    length(useractivity[as_reg_date >= '2014-07-01' & as_num_edits_week_5 > 0 & as_created_article == 1]$as_userid)),
  ncol=2, byrow=TRUE);
rownames(create_cont_matrix) = c('created article = 0', 'created article = 1');
colnames(create_cont_matrix) = c('edited week 1', 'edited week 5');

## Overall proportions:
100*create_cont_matrix/sum(create_cont_matrix)

## Survival proportion:
100*create_cont_matrix/rowSums(create_cont_matrix)

## Let's use the Chi-square goodness-of-fit test to test
## if the second row's significantly different from expected:
chisq.test(create_cont_matrix[2,],
           p = create_cont_matrix[1,]/sum(create_cont_matrix[1,]));

create_cont_matrix_autocreated = matrix(
  c(length(useractivity[as_reg_date >= '2014-07-01' & as_num_edits_week_1 > 0
                        & as_created_article == 0 & as_create_type == 'autocreate']$as_userid),
    length(useractivity[as_reg_date >= '2014-07-01' & as_num_edits_week_5 > 0
                        & as_created_article == 0 & as_create_type == 'autocreate']$as_userid),
    length(useractivity[as_reg_date >= '2014-07-01' & as_num_edits_week_1 > 0
                        & as_created_article == 1 & as_create_type == 'autocreate']$as_userid),
    length(useractivity[as_reg_date >= '2014-07-01' & as_num_edits_week_5 > 0
                        & as_created_article == 1 & as_create_type == 'autocreate']$as_userid)),
  ncol=2, byrow=TRUE);
rownames(create_cont_matrix_autocreated) = c('created article = 0', 'created article = 1');
colnames(create_cont_matrix_autocreated) = c('edited week 1', 'edited week 5');

## How many autocreated accounts in total?
sum(create_cont_matrix_autocreated);

## Overall proportions:
100*create_cont_matrix_autocreated/sum(create_cont_matrix_autocreated)

## Survival proportion:
100*create_cont_matrix_autocreated/rowSums(create_cont_matrix_autocreated)

## Goodness-of-fit test as before:
chisq.test(create_cont_matrix_autocreated[2,],
           p = create_cont_matrix_autocreated[1,]/sum(create_cont_matrix_autocreated[1,]));

## Q4: If they created an article, how does article deletion affect editor survival?

create_prop_editorsurvival = merge(
  rbind(
    useractivity[as_created_article == 1 & as_article_survived == 0 & as_num_edits_week_1 > 0,
                 list(survived='no', n_creators=sum(.N)),
                 by=list(as_reg_date, as_create_type)],
    useractivity[as_created_article == 1 & as_article_survived == 1 & as_num_edits_week_1 > 0,
                 list(survived='yes', n_creators=sum(.N)),
                 by=list(as_reg_date, as_create_type)]),
    rbind(
      useractivity[as_created_article == 1 & as_article_survived == 0 & as_num_edits_week_5 > 0,
                   list(survived='no', n_survivors=sum(.N)),
                   by=list(as_reg_date, as_create_type)],
      useractivity[as_created_article == 1 & as_article_survived == 1 & as_num_edits_week_5 > 0,
                   list(survived='yes', n_survivors=sum(.N)),
                   by=list(as_reg_date, as_create_type)]),
    by=c('as_reg_date', 'as_create_type', 'survived'));

## Restrict it to 2014-01-07 onwards
create_prop_editorsurvival = create_prop_editorsurvival[as_reg_date >= '2014-07-01'];

ggplot(create_prop_editorsurvival[,
  list(prop_survivors = 100*sum(n_survivors)/sum(n_creators)),
  by=list(as_reg_date, as_create_type, survived)],
  aes(x=as_reg_date, y=prop_survivors)) + geom_line() +
  ggtitle('Proportion of surviving editors among article creators') +
  scale_x_date(date_breaks='1 years', date_labels = '%Y') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105), breaks=seq(0,100, 10)) +
  xlab('Year') + ylab('Proportion in %') + 
  facet_grid(as_create_type ~ survived);

## 2x2 contingency table as before, first overall:
create_cont_matrix_editorsurvival_overall = matrix(
  c(
    create_prop_editorsurvival[
        survived == 'no',
        list(n=sum(n_creators-n_survivors))]$n,
    create_prop_editorsurvival[
        survived == 'no',
        list(n=sum(n_survivors))]$n,
    create_prop_editorsurvival[
        survived == 'yes',
        list(n=sum(n_creators-n_survivors))]$n,
      create_prop_editorsurvival[
        survived == 'yes',
        list(n=sum(n_survivors))]$n),
  ncol=2, byrow=TRUE);
rownames(create_cont_matrix_editorsurvival_overall) = c('article deleted', 'article survived');
colnames(create_cont_matrix_editorsurvival_overall) = c('edited in week 1', 'edited in week 5');

## Overall proportions
create_cont_matrix_editorsurvival_overall/sum(create_cont_matrix_editorsurvival_overall);

## Proportions by row
create_cont_matrix_editorsurvival_overall/rowSums(create_cont_matrix_editorsurvival_overall);

## Chi-square test:
chisq.test(create_cont_matrix_editorsurvival_overall[2,],
           p = create_cont_matrix_editorsurvival_overall[1,]/sum(create_cont_matrix_editorsurvival_overall[1,]));

## Autocreated accounts
create_cont_matrix_editorsurvival_autocreated = matrix(
  c(
    create_prop_editorsurvival[
      survived == 'no' & as_create_type == 'autocreate',
      list(n=sum(n_creators-n_survivors))]$n,
    create_prop_editorsurvival[
      survived == 'no' & as_create_type == 'autocreate',
      list(n=sum(n_survivors))]$n,
    create_prop_editorsurvival[
      survived == 'yes' & as_create_type == 'autocreate',
      list(n=sum(n_creators-n_survivors))]$n,
    create_prop_editorsurvival[
      survived == 'yes' & as_create_type == 'autocreate',
      list(n=sum(n_survivors))]$n),
  ncol=2, byrow=TRUE);
rownames(create_cont_matrix_editorsurvival_autocreated) = c('article deleted', 'article survived');
colnames(create_cont_matrix_editorsurvival_autocreated) = c('edited in week 1', 'edited in week 5');

## Proportions by row
create_cont_matrix_editorsurvival_autocreated/rowSums(create_cont_matrix_editorsurvival_autocreated);

## Chi-square test:
chisq.test(create_cont_matrix_editorsurvival_autocreated[2,],
           p = create_cont_matrix_editorsurvival_autocreated[1,]/sum(create_cont_matrix_editorsurvival_autocreated[1,]));

## I want to sample users from several categories. Common for them is:
## 1: created an article in their first edit
## 2: surviving editor (made at least one edit in week 5)
##
## I want to sample users across a fairly limited amount of time in order to
## reduce time-sensitive effects. Therefore, I choose to sample across Q1 & Q2 2016.
## (Using a shorter timespan is troublesome because of low N for autocreated users)
##
## Let's first look at users who had their article deleted vs those that did not,
## and for users who were autocreated vs those that were not. That's four groups:

useractivity[as_reg_date >= '2016-01-01' & as_reg_date < '2016-07-01'
             & as_num_edits_week_1 > 0 & as_num_edits_week_5 > 0
             & as_created_article == 1,
             .SD[sample(.N, 12)], by=c('as_create_type', 'as_article_survived')];

## Sampled users:
user_sample = c(28487841, 27692393, 28359982, 28377309, 28673201, 27858707,
                28002644, 28568609, 27222348, 28355584, 28621887, 28653829,
                27452764, 27623704, 28039679, 28449418, 27511025, 28533247,
                28168525, 28434043, 27740181, 28423844, 27322712, 28275475,
                27629998, 27646891, 28518722, 28608400, 28663958, 28586418,
                28392062, 28220757, 27504119, 27781766, 27475701, 27732668,
                27592599, 28444109, 27923306, 27935532, 27646268, 28613067,
                27855410, 27786066, 28630920, 28067357, 28262256, 27592583);



