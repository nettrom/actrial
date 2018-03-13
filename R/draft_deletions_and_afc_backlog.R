## Examining how draft deletions correlate with reductions in the AfC backlog.

library(data.table);
library(ggplot2);

## Read in the datasets
draft_deletions = fread('datasets/draft_deletions_by_day.tsv');
afc_backlog = fread('datasets/AfC-backlog-historic.csv');
moves_into_main = fread('datasets/moves_into_main.tsv');

## We have daily data of draft deletions, and weekly or daily data of
## the AfC backlog. Parse the dates/timestamps and create date & time columns.
draft_deletions[, event_date := as.Date(log_date)];
afc_backlog[, c("event_date", "event_time") := IDateTime(
  as.POSIXct(timestamp, format='%Y-%m-%dT%H:%M:%S', tz='UTC'))];
moves_into_main[, event_date := as.Date(mim_date)];

## Plot the two
ggplot() +
  geom_line(data=draft_deletions, aes(x=event_date, y=num_draft_deletions,
                                      colour='Draft deletions')) +
  geom_line(data=afc_backlog, aes(x=event_date, y=size, colour="AfC Backlog")) +
  geom_line(data=moves_into_main[event_date >= '2015-01-01'],
            aes(x=event_date, y=mim_num_moves_draft, colour='Moves from Draft')) +
  ylim(0, 3000);

## They don't look to be very much correlated at all? What if we also take
## moves from the Draft namespace into consideration?
## So… here's what's going on: the Afc backlog reflects the size of
## "Category:Pending AfC submissions". At the moment, the distribution
## of pages in that category by namespace is:
## User (ns=2): 7
## Wikipedia (ns=4): 2
## Category (ns=14): 4
## Draft (ns=118): 2,661

## The number of non-redirect pages in the Draft namespace at the moment is: 35,227

## A draft moves into the category when a user submits it for review, it moves out
## of the category when an AfC reviewer reviews it. It does not get deleted or
## moved into a different namespace.

## So, we're measuring apples and oranges. Instead, we'll have to look at drafts
## by day, and the categories are either found on the Draft page itself, or on
## the talk page if it's moved into main. Question is, how do we handle deletions?
## Split it into two/three measures: submitted drafts, deleted drafts, moves?
## AfC switched from monthly to daily tracking of submissions in Sept 2008, btw.
## Note that these categories should count all drafts submitted for review on a given
## day, tracking drafts submitted multiple times correctly.
## Note that these categories also track drafts in User space.
## Note that we should be able to track deletions of stale drafts in user space
## by looking for the G13 CSD note.
## Side note: it looks like a lot of drafts get accepted immediately. What
## proportion of drafts go through multiple submissions before being accepted?
## (count number of "AfC submissions by date" categories).
