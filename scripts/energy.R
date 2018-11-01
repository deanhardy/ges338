rm(list=ls())

library(tidyverse)

ST <- c('Pennsylvania', 'West Virginia','New York', 'Virginia', 'Maryland', 'Delaware', 'DC')
ST2 <- c('Pennsylvania', 'New York', 'Virginia', 'Maryland', 'West Virginia','Delaware', 'DC')

prod <- read.csv('data/energy_prod2016_eia.csv', skip = 2, stringsAsFactors = FALSE,
                 nrows = 57) %>%
  filter(State %in% ST) %>%
  gather(Source, Btu, 2:9) %>%
  mutate(Btu = as.numeric(Btu)) %>%
  mutate(State = factor(State, levels = ST))
  
cons <- read.csv('data/energy_cons2016_eia.csv', skip = 1, stringsAsFactors = FALSE)[, 1:6] %>%
  filter(State %in% ST)%>%
  gather(Source, Btu, 2:6) %>%
  mutate(Btu = as.numeric(Btu)) %>%
  mutate(State = factor(State, levels = ST2))

ggplot(filter(prod, Source != 'Total')) + 
  geom_col(aes(State, Btu, fill = Source)) + 
  ggtitle("Energy Production (2016) in the Chesapeaky Bay's 7 Jurisdictions") + 
  scale_y_continuous(name = 'Total Production (Trillion Btu)') + 
  scale_x_discrete(name = 'State')

ggplot(filter(cons, Source != 'Total')) + 
  geom_col(aes(State, Btu, fill = Source)) + 
  ggtitle("Energy Consumption (2016) in the Chesapeaky Bay's 7 Jurisdictions") + 
  scale_y_continuous(name = 'Total Consumption (Trillion Btu)') + 
  scale_x_discrete(name = 'State')
