# imports
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
require(effsize)
library(effsize)

### read anes data
anes <- read_csv("w203_s22_lab1/assignment/part2/data/anes_timeseries_2020_csv_20220210.csv")

### Keep only variables of interest
keeps = c(
  "V201600",   # gender
  "V201507x",  # age
  "V201511x",  # education
  "V201549x",  # race
  "V201567",   # children
  "V201617x",  # family income
  "V202109x",  # turnout in 2020 election
  "V202110x",  # 2020 presi vote
  "V202111x",  # 2020 house vote
  "V202112x",  # 2020 senate vote
  "V202113x",  # 2020 gov vote
  "V201231x",  # party ID
  "V201019",   # pre. intend to register to vote
  "V202051",   # post. registered to vote
  "V202117",   # voting method
  "V202118",   # usual voting method
  "V202119",   # how difficult to vote
  "V202120a",  # registration issues?
  "V202120b",  # concern on ID card?
  "V202120c",  # diffculties with absentee ballot
  "V202120d",  # confusion on ballot/machine
  "V202120e",  # difficulty getting to polling place
  "V202120f",  # long wait time at polling place
  "V202120g",  # work schedule
  "V202120h",  # bad weather
  "V202120i",  # issue mailing ballot
  "V202120j",  # other issue
  "V202120k",  # no issues voting
  "V202121",   # how long the wait
  "V202122",   # how long to get to polling line
  "V202123",   # main reason not to vote
  "V202124"    # other reason not to vote
)

column_names = c(
  "gender",
  "age",
  "education",
  "race",
  "children",
  "income",
  "voted_2020",
  "v2020_presi",
  "v2020_house",
  "v2020_senate",
  "v2020_gov",
  "party",
  "pre_intent_to_vote",
  "post_registered_to_tove",
  "voting_method",
  "usual_voting_method",
  "diffvote",
  "diffvote_reg",
  "diffvote_idcard",
  "diffvote_aballot",
  "diffvote_confmach",
  "diffvote_gopolling",
  "diffvote_wait",
  "diffvote_work",
  "diffvote_weather",
  "diffvote_mail",
  "diffvote_other",
  "diffvote_none",
  "diffvote_wait_time",
  "diffvote_gopolling_time",
  "diffvote_mainreason",
  "diffvote_otherreason"
)

anes_filt = anes[keeps]
colnames(anes_filt) = column_names

### Create party column
anes_filt = anes_filt %>%
  mutate(party_self = case_when(
    party %in% c(1, 2, 3) ~ "Democrat",
    party %in% c(5, 6, 7) ~ "Republican",
    party %in% c(4)       ~ "Independent"
  ))

### Keep voters only
anes_voted = anes_filt %>% filter(voted_2020 == 1)

### WRS test
# Distribution check
anes_voted %>% 
  filter(diffvote >= 0 & 
           party_self %in% c("Republican", "Democrat") &
           !is.na(diffvote)) %>%
  ggplot() +
  aes(x=diffvote, fill=party_self, color=party_self) +
  geom_bar(alpha=0.5, position="dodge", width=0.5) +
  labs(title="Level of difficulty voting by self determined party")

# Create reps and demos vectore
reps = anes_voted %>% filter(diffvote >= 0 & 
                               party_self == "Republican" &
                               !is.na(diffvote)) %>% 
  select(diffvote) %>% pull(diffvote)

demos = anes_voted %>% filter(diffvote >= 0 & 
                                party_self == "Democrat" &
                                !is.na(diffvote)) %>% 
  select(diffvote) %>% pull(diffvote)

# Run double test
wilcox.test(reps, 
            demos, 
            alternative="two.sided",
            conf.level=0.95)

# Run greater test
wilcox.test(reps, 
            demos, 
            alternative="less",
            conf.level=0.975)

# Check test with alternate syntax
anes_voted_party_clean = anes_voted %>%
  filter(party_self %in% c("Republican", "Democrat") &
           diffvote >= 0)

wt = wilcox.test(diffvote ~ party_self, data = anes_voted_party_clean,
            alternative="two.sided",
            conf.int = TRUE,
            conf.level=0.95)

### Effect size
# Correlation effect size
z = qnorm(wt$p.value)
N = count(anes_voted_party_clean)
eff_corr = z/sqrt(N)

# Cohen's d
cohen.d(diffvote ~ party_self, data = anes_voted_party_clean)

# check counts
anes_voted %>% filter(party_self=="Democrat") %>% count(issue_voting)
anes_voted %>% filter(party_self=="Republican") %>% count(issue_voting)








