rm(list=ls())

library(tidyverse)
library(lubridate)

bc <- read.csv('data/landings_bluecrab.csv', skip = 4, stringsAsFactors = FALSE) %>%
  rename(USdollars = X.) %>%
  mutate(Year = as.Date(ISOdate(Year, 12, 31)),
         State = as.character(State),
         Metric.Tons = as.numeric(Metric.Tons),
         Pounds = as.numeric(Pounds),
         USdollars = as.numeric(USdollars)) %>%
  group_by(Year) %>%
  group_by(State, add = TRUE) %>%
  summarise(metric_tons = sum(Metric.Tons), 
            pounds = sum(Pounds),
            dollars = sum(USdollars)) %>%
  mutate(Species = 'Blue Crab')

eo <- read.csv('data/landings_easternoyster.csv', skip = 4, stringsAsFactors = FALSE) %>%
  rename(USdollars = X.) %>%
  mutate(Year = as.Date(ISOdate(Year, 12, 31)),
         State = as.character(State),
         Metric.Tons = as.numeric(Metric.Tons),
         Pounds = as.numeric(Pounds),
         USdollars = as.numeric(USdollars)) %>%
  group_by(Year, State) %>%
  summarise(metric_tons = sum(Metric.Tons), 
            pounds = sum(Pounds),
            dollars = sum(USdollars)) %>%
  mutate(Species = 'Eastern Oyster')

rf <- read.csv('data/landings_rockfish.csv', skip = 4, stringsAsFactors = FALSE) %>%
  rename(USdollars = X.) %>%
  mutate(Year = as.Date(ISOdate(Year, 12, 31)),
         State = as.character(State),
         Metric.Tons = as.numeric(Metric.Tons),
         Pounds = as.numeric(Pounds),
         USdollars = as.numeric(USdollars)) %>%
  group_by(Year, State) %>%
  summarise(metric_tons = sum(Metric.Tons), 
            pounds = sum(Pounds),
            dollars = sum(USdollars)) %>%
  mutate(Species = 'Rockfish')

dat <- rbind(bc, eo, rf)

fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'))) +
  geom_smooth(aes(Year, metric_tons, color = State, linetype = Species)) + 
  geom_point(aes(Year, metric_tons, color = State, shape = Species)) +
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous(name = 'Metric Tons') +
  ggtitle('Blue Crab, Eastern Oyster, & Rockfish Landings (1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/landings_smoothed.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()

## plot US dollars
fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'))) +
  geom_line(aes(Year, dollars/1000000, color = State, linetype = Species)) + 
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous(name = 'Million Dollars (US$)') +
  ggtitle('Blue Crab, Eastern Oyster, & Rockfish Landings (1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/landings_dollars.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()
  
