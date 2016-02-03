library(pollstR)
library(foreign)
library(RCurl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)


# https://github.com/fivethirtyeight/data/tree/master/pollster-ratings

silver.polls <- read.delim('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.tsv')

silver.ratings <- read.delim('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/pollster-ratings.tsv')

silver.data <- merge(silver.polls, silver.ratings, by.x = 'pollster', by.y = 'Pollster', all = T)

silver.data$poll.date <- as.Date(silver.data$polldate, '%m/%d/%Y')
silver.data$poll.month <- format(silver.data$poll.date, "%m/%d")

silver.data$election.date <- as.Date(silver.data$electiondate, '%m/%d/%Y')
silver.data$election.month <- format(silver.data$election.date, "%m/%d")

states.data <- filter(silver.data, type_simple == 'Pres-G' & location != 'US') 
states.data$cand1_diff <- states.data$cand1_actual-states.data$cand1_pct
states.data$cand2_diff <- states.data$cand2_actual-states.data$cand2_pct

head(states.data)
table(states.data$location)

View(states.data %>% 
  filter(location == 'IA') %>% 
  group_by(pollster, year, location) %>% 
  summarize(count = n()) %>%
  arrange(pollster, year))

states.data <- states.data %>%
  select(pollster, year, X538.Grade, location, cand1_diff, cand2_diff, cand1_pct, cand1_actual, cand2_pct, cand2_actual, poll.date)

head(states.data)


states.data <- states.data %>%
  select(pollster, year, X538.Grade, location, cand1_diff, cand2_diff, poll.date)


states.m <- melt(states.data, id.vars = c('X538.Grade','pollster', 'year', 'location', 'cand1_actual', 'cand2_actual', 'poll.date'))
states.m <- melt(states.data, id.vars = c('X538.Grade','pollster', 'year', 'location', 'poll.date'))

head(states.m)

qplot(data=states.m,year,value,color=variable)

ggplot(states.m) +
  geom_point(aes(x = value, y = factor(year),size=2,shape="line",color = location)) + 
  facet_wrap( ~variable)


View(states.data %>% 
       filter(location == 'IA') %>% 
       group_by(pollster, year, location) %>% 
       summarize(count = n()) %>%
       arrange(pollster, year))

states.avg1 <- states.m %>% filter(variable == 'cand1_diff') %>% group_by(location,year) %>% summarize(value = mean(value))
states.avg2 <- states.m %>% filter(variable == 'cand2_diff') %>% group_by(location,year) %>% summarize(value = mean(value))

states.range <- states.m %>% filter(variable == 'cand1_diff') %>% group_by(location,year) %>% mutate(range = max(value)-min(value)) #doesn't work yet


ggplot(states.avg2, aes(x= year, y= value, colour=location, label=location))+
  geom_text(aes(label=location),hjust=0, vjust=0)

ggplot(states.range, aes(x= year, y= value, colour=location, label=location))+
  geom_point() +geom_text(aes(label=location),hjust=0, vjust=0)




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
  