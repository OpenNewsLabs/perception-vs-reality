library(pollstR)
library(foreign)
library(RCurl)
library(dplyr)
library(ggplot2)
library(reshape2)

setwd("/Users/christinezhang/Desktop/km_elex")

# https://github.com/fivethirtyeight/data/tree/master/pollster-ratings

silver.polls <- read.delim('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.tsv')

silver.ratings <- read.delim('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/pollster-ratings.tsv')

silver.data <- merge(silver.polls, silver.ratings, by.x = 'pollster', by.y = 'Pollster', all = T)

silver.data$poll.date <- as.Date(silver.data$polldate, '%m/%d/%Y')
silver.data$poll.month <- format(silver.data$poll.date, "%m/%d")

silver.data$election.date <- as.Date(silver.data$electiondate, '%m/%d/%Y')
silver.data$election.month <- format(silver.data$election.date, "%m/%d")

head(silver.data)

pres.data <- filter(silver.data, type_simple == 'Pres-G' & location == 'US') 

counts <- pres.data %>%
  group_by(pollster) %>% 
  summarize(count = n()) %>%
  ungroup()

data <- merge(pres.data, counts, by = 'pollster')

head(data %>% filter(count >= 12))
  
table(data$X538.Grade)
levels(data$X538.Grade) 

data$Grade = factor(data$X538.Grade,levels=c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","F"),ordered=TRUE)
table(data$Grade)

data$X538.Grade.n <- as.numeric(data$Grade)
table(data$X538.Grade.n)

data <- data %>% filter(count >= 12)

head(data %>% group_by(pollster, X538.Grade, year) %>% summarize(number = n()))

data <- data %>% mutate(diff = election.date - poll.date)
str(data)

data$test <- as.numeric(data$diff)

ggplot(data = data) +
  geom_point(aes(color = Grade, x = as.numeric(diff), y = factor(year), size = 1/error)) +
  theme_bw() +
  facet_wrap(~pollster)

head(data)
names(data)
data.new <- data %>% select(pollster, year, poll.date, cand1_pct, cand2_pct, cand1_actual, cand2_actual, error, margin_actual, margin_poll)

data.new <- data.new %>% 
  arrange(pollster, year, desc(poll.date)) %>%
  group_by(pollster, year) %>%
  mutate(count = row_number()) 

data.new$recent <- ifelse(data.new$count == 1, 1, 0) # indicator for the most recent
data.recent <- data.new %>% filter(recent == 1 & pollster != 'Google Consumer Surveys') %>%
  select(-count, -recent)

write.csv(data.recent, 'github/national.csv', row.names = F)

data.m <- melt(data.new, id.vars = c('pollster', 'poll.date', 'year', 'cand1_actual', 'cand2_actual'))
head(data.m)

for (i in unique(data.m$year)) {

p <- ggplot(data = data.m %>% filter(year == i), aes(x = poll.date, color = variable, y = value)) +
  geom_line() +
  facet_wrap(~pollster) +
  scale_color_manual(values = c('blue', 'red')) +
  geom_line(aes(y = cand1_actual), linetype = 'dotted', color = 'blue') +
  geom_line(aes(y = cand2_actual), linetype = 'dotted', color = 'red') 

print(p)

ggsave(paste0(i,'p.png'), width = 12, height = 8)

}

data.m$error <- ifelse(data.m$variable == 'cand1_pct', value - cand1_actual
data.m$error.cand2 <- data.m$value - data.m$cand2_actual

head(data.m)


hist(as.Date(silver.data[silver.data$type_simple == 'Pres-G', ]$month, "%m/%d"), breaks = 'days') # presidential polls by date

head(silver.data)


polls <- pollstr_polls(state = 'ia')
head(polls$questions)
str(polls)
results <- read.csv()

us_polls <- pollstr_polls(state='ia')
str(us_polls)
table(us_polls$polls$start_date)
