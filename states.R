library(pollstR)
library(foreign)
library(RCurl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)

setwd("/Users/christinezhang/Desktop/km_elex")

# https://github.com/fivethirtyeight/data/tree/master/pollster-ratings

silver.polls <- read.delim('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.tsv')

silver.ratings <- read.delim('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/pollster-ratings.tsv')

silver.data <- merge(silver.polls, silver.ratings, by.x = 'pollster', by.y = 'Pollster', all = T)

silver.data$poll.date <- as.Date(silver.data$polldate, '%m/%d/%Y')
silver.data$poll.month <- format(silver.data$poll.date, "%m/%d")

silver.data$election.date <- as.Date(silver.data$electiondate, '%m/%d/%Y')
silver.data$election.month <- format(silver.data$election.date, "%m/%d")

states.data <- filter(silver.data, type_simple == 'Pres-G' & location != 'US') 
head(states.data)
table(states.data$location)

View(states.data %>% 
  filter(location == 'IA') %>% 
  group_by(pollster, year, location) %>% 
  summarize(count = n()) %>%
  arrange(pollster, year))

states.data <- states.data %>%
  select(pollster, year, location, cand1_pct, cand1_actual, cand2_pct, cand2_actual, poll.date)

head(states.data)

states.m <- melt(states.data, id.vars = c('pollster', 'year', 'location', 'cand1_actual', 'cand2_actual', 'poll.date'))

head(states.m)

ggplot(states.m) +
  geom_point(aes(x = value, y = factor(year))) +
  facet_wrap(location~variable)

for (i in unique(states.m$location)) {
  
  ggplot(states.m[states.m$location == i, ]) +
    geom_point(aes(x = value, y = factor(year))) +
    facet_wrap(location~variable)
  
  ggsave()
  
}


ggplot(states.m[states.m$location == 'IA', ]) +
  geom_point(aes(x = value, y = factor(year), color = variable)) +
  facet_wrap(location~variable) +
  scale_color_manual(values = c('blue', 'red'))  +
  theme(legend.position = 'none') 
  