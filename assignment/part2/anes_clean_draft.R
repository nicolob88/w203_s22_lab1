# imports
library(dplyr)
library(readr)
library(ggplot2)

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


### How many people voted: 6450 voted, 1039 didn't, 791 not reported
# Freq table: 0. Not vote, 1. Vote, -2. Not reported
count(anes_filt, voted_2020)
anes_filt %>% ggplot() + aes(x=voted_2020) + geom_histogram(fill='blue')

# create voted tables
anes_voted = anes_filt %>% filter(voted_2020 == 1)
anes_novoted = anes_filt %>% filter(voted_2020 == 0)


### From those that voted, how many had diffculty doing so: Only ~12%
# 1 to 5 scale: 1 not difficult at all, 5 extremely difficult
count(anes_voted, diffvote)
anes_voted %>% filter (diffvote >= 0) %>% 
  ggplot() + aes(x=diffvote) + geom_histogram(fill='blue')


### Party frequencies
# 1: democrat, 4: indep, 7: republican
count(anes_voted, party)
anes_voted %>%  filter (party>=0) %>%
  ggplot() + aes(x=party) + geom_histogram(fill='blue')

# Party column 1,2,3 = Democrat, 5,6,7=Republican, 4 = None
anes_filt %>% 
  mutate(party_count = case_when(
    party %in% c(1, 2, 3) ~ "Democrat",
    party %in% c(5, 6, 7) ~ "Republican",
    party %in% c(4)       ~ "Independent"
  )) %>% count(party_count)



































