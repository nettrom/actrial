## Exploring when newly registered accounts create articles and drafts, and
## if they tend to create multiple drafts/articles.

library(ggplot2);
library(data.table);

## Read in the datasets:
draft_creations_30 = fread('datasets/draft_creations_30days.tsv');
article_creations_30 = fread('datasets/article_creations_30days.tsv');

## Add a column for namespace, join the two datasets and delete the original ones.
draft_creations_30[, event_page_namespace := 118];
article_creations_30[, event_page_namespace := 0];

page_creations_30 = rbind(draft_creations_30, article_creations_30);
rm(draft_creations_30);
rm(article_creations_30);

## Q1: If an account creates an article or draft within the first 30 days,
##     how old is the account when that happens?
ggplot(page_creations_30, aes(1 + event_user_age)) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Account age when creating article or draft (within 30 days)") +
  scale_x_log10(
    "Account age",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month")) +
  facet_grid(event_page_namespace ~ ., scales="free_y");

## What proportion of articles/drafts were created within 24 hours after registration?
100*length(page_creations_30[event_page_namespace == 0
                             & event_user_age < 24*60*60]$event_user_id)/length(page_creations_30[event_page_namespace == 0]$event_user_id);
100*length(page_creations_30[event_page_namespace == 118
                             & event_user_age < 24*60*60]$event_user_id)/length(page_creations_30[event_page_namespace == 118]$event_user_id);

## Summary statistics of account age
summary(page_creations_30[event_page_namespace == 0]$event_user_age);
summary(page_creations_30[event_page_namespace == 118]$event_user_age);

## Q2: If an account creates an article or draft within the first 30 days,
##     will they create only one article, or multiple ones?

## How many unique user accounts have created articles/drafts in our dataset?
creations_per_user = page_creations_30[, list(n_creations=sum(.N)), by=event_user_id];

## Number of users
length(creations_per_user$event_user_id);

## Proportion of users who created more than one article or draft
100*length(creations_per_user[n_creations > 1]$event_user_id)/length(creations_per_user$event_user_id);

## Number of users who created a given number of articles/drafts, up to 25:
creations_per_user[, list(n_users = sum(.N)), by=n_creations][order(n_creations)][n_creations < 25];

ggplot(creations_per_user[n_creations > 1 & n_creations < 10], aes(n_creations)) +
  geom_histogram(binwidth=1, colour='black', fill='white');

## How has this developed during ACTRIAL? We gathered a dataset from July 21 to
## November 1, 2017, and can make a similar analysis.
current_creations = fread('datasets/current_creations.tsv');

## Turn the timestamp into date & time:
current_creations[,
                  c("creation_date", "creation_time") := IDateTime(strptime(rev_timestamp, format='%Y-%m-%d %H:%M:%S', tz="UTC"))];

current_creations[, is_actrial := "f"];
current_creations[creation_date >= '2017-09-15', is_actrial := "t"];

actrial_labels = c("f" = "Pre-ACTRIAL", "t"="ACTRIAL");

ggplot(current_creations, aes(1 + performer_account_age)) + 
  geom_histogram(binwidth = 0.1, colour="black", fill='white') +
  ggtitle("Account age when creating article or draft (within 30 days)") +
  scale_x_log10(
    "Account age",
    breaks=c(60, 15*60, 60*60, 24*60*60, 7*24*60*60, 30*24*60*60),
    labels=c("minute", "15 min.", "hour", "day", "week", "month")) +
  facet_grid(page_namespace ~ is_actrial, scales="free_y",
             labeller=labeller(is_actrial = actrial_labels));

current_creations_per_user = current_creations[, list(n_creations=sum(.N)),
                                               by=list(is_actrial, performer_user_id)];
current_creations_per_user[, list(n_users = sum(.N)), by=list(is_actrial, n_creations)][order(n_creations)][n_creations < 25];

ggplot(current_creations_per_user, aes(n_creations)) +
  geom_histogram(binwidth=1, colour='black', fill='white') +
  facet_grid(is_actrial ~ ., scales="free_y");
