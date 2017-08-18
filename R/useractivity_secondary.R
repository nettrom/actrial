## Further analysis of things that come up in the user activity analysis

## Spike in proportion of autocreated accounts with non-zero edits in late 2014,
## these reach autoconfirmed status within 30 days.
## 1: What date was that? It's somewhere in the fall of 2014.
prop_autoconfirmed_30[as_reg_date >= '2014-07-01' & as_reg_date < '2015-01-01'
                      & as_create_type == 'autocreate'
                      & (n_autoconfirmed/n_editors) > 0.4];

prop_autoconfirmed_30[as_reg_date >= '2014-09-01' & as_reg_date < '2014-10-01'
                      & as_create_type == 'autocreate'];

## Two dates: September 15 & 17, 2014, their user IDs:
useractivity[as_reg_date %in% c(as.Date('2014-09-15'), as.Date('2014-09-17'))
             & as_create_type == 'autocreate'
             & as_autoconfirmed_30 == 1]$as_userid;

## 146 users in total, 77 (of 169 w/non-zero edits) on Sept 15,
## and 69 (of 158 w/non-zero edits) on Sept 17.

## 117 (80.1%) of the accounts went through the Wikipedia Adventure. That leaves
## us 29 other users, which is on par with what you'd expect per day.
