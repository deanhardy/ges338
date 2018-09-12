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

mh <- read.csv('data/landings_menhaden.csv', skip = 4, stringsAsFactors = FALSE) %>%
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
  mutate(Species = 'Menhaden')

as <- read.csv('data/landings_shad.csv', skip = 4, stringsAsFactors = FALSE) %>%
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
  mutate(Species = 'American Shad')

dat <- rbind(bc, eo, rf, mh, as)



############################################
## BAY (Rockfish, Oyster, & Crabs)
############################################

fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'))) +
  geom_line(aes(Year, metric_tons, color = State, linetype = Species)) + 
  # geom_point(aes(Year, metric_tons, color = State, shape = Species)) +
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous(name = 'Metric Tons') +
  ggtitle('Blue Crab, Eastern Oyster, & Rockfish (Weight) Landings (1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/landings.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()

## BAY plot US dollars 
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

tiff('figures/dollars.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()



######################
## OYSTER
######################

fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'), Species == 'Eastern Oyster')) +
  geom_line(aes(Year, metric_tons, color = State)) + 
  # geom_line(aes(Year, filter(dat$metric_tons, State == 'Maryland', Species == 'Eastern Oyster'))) + 
  # geom_point(aes(Year, metric_tons, color = State, shape = Species)) +
  geom_vline(xintercept = as.Date('1959-01-01'), linetype = 'dashed') +
  geom_vline(xintercept = as.Date('1993-01-01'), linetype = 'dashed') +
  geom_text(x = as.Date('1950-01-01'), y = 11900, label = '<<< Derma hits in 1949',
            hjust = -0.1) +
  geom_text(x = as.Date('1959-01-01'), y = 11500, label = '<<< MSX hits in 1959',
            hjust = -0.05) +
  geom_text(x = as.Date('1983-01-01'), y = 5500, label = '<<< Drought drives decline in mid-80s',
            hjust = -0.05) +
  geom_text(x = as.Date('1992-01-01'), y = 4500, label = '<<< MD Oyster Roundtable in 1993',
            hjust = -0.05) +
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous(name = 'Metric Tons') +
  ggtitle('Eastern Oyster Landings (weight; 1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/oyster_landings.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()

## plot US dollars
fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'), Species == 'Eastern Oyster')) +
  # geom_line(aes(Year, dollars/1000000, linetype = Species)) + 
  geom_line(aes(Year, dollars/1000000, color = State, linetype = Species)) + 
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous(name = 'Million Dollars (US$)') +
  ggtitle('Eastern Oyster Commerical Landings (Dollars; 1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/oysters_dollars.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()



######################
## ROCKFISH
######################

fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'), Species == 'Rockfish')) +
  geom_line(aes(Year, metric_tons, color = State, linetype = Species)) + 
  # geom_point(aes(Year, metric_tons, color = State, shape = Species)) +
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous('Metric Tons') +
  ggtitle('Rockfish Commercial Landings (Weight; 1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/rockfish_landings.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()

## plot US dollars
fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'), Species == 'Rockfish')) +
  # geom_line(aes(Year, dollars/1000000, linetype = Species)) + 
  geom_line(aes(Year, dollars/1000000, color = State, linetype = Species)) + 
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous(name = 'Million Dollars (US$)') +
  ggtitle('Rockfish Commercial Landings (Dollars; 1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/rockfish_dollars.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()



######################
## MENHADEN
######################

fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'), Species == 'Menhaden')) +
  geom_line(aes(Year, metric_tons, color = State, linetype = Species)) + 
  # geom_point(aes(Year, metric_tons, color = State, shape = Species)) +
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous('Metric Tons') +
  ggtitle('Menhaden Commercial Landings (Weight; 1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/menhaden_landings.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()

## plot US dollars
fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'), Species == 'Menhaden')) +
  # geom_line(aes(Year, dollars/1000000, linetype = Species)) + 
  geom_line(aes(Year, dollars/1000000, color = State, linetype = Species)) + 
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous(name = 'Million Dollars (US$)') +
  ggtitle('Menhaden Commercial Landings (Dollars; 1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/menhaden_dollars.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()


######################
## AMERICAN SHAD
######################

fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'), Species == 'American Shad')) +
  geom_line(aes(Year, metric_tons, color = State, linetype = Species)) + 
  # geom_point(aes(Year, metric_tons, color = State, shape = Species)) +
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous('Metric Tons') +
  ggtitle('American Shad Commercial Landings (Weight; 1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/shad_landings.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()

## plot US dollars
fig <- ggplot(filter(dat, State %in% c('Maryland', 'Virginia'), Species == 'American Shad')) +
  # geom_line(aes(Year, dollars/1000000, linetype = Species)) + 
  geom_line(aes(Year, dollars/1000000, color = State, linetype = Species)) + 
  scale_x_date(name = 'Year',
               date_breaks = '10 years',
               date_labels = '%Y',
               expand = c(0,0)) +
  scale_y_continuous(name = 'Million Dollars (US$)') +
  ggtitle('American Shad Commercial Landings (Dollars; 1950-2016)') +
  theme(panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
fig

tiff('figures/shad_dollars.tiff', height = 7, width = 10, units = 'in', res = 300, compression = 'lzw')
fig
dev.off()
