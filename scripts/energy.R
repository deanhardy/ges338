rm(list=ls())

library(tidyverse)

ST <- c('Pennsylvania', 'West Virginia','New York', 'Virginia', 'Maryland', 'Delaware', 'DC')
ST2 <- c('Pennsylvania', 'New York', 'Virginia', 'Maryland', 'West Virginia','Delaware', 'DC')

## import data
prod <- read.csv('data/energy_prod2016_eia.csv', skip = 2, stringsAsFactors = FALSE, nrows = 57) %>%
  filter(State %in% ST) %>%
  gather(Source, Btu, 2:9) %>%
  mutate(Btu = as.numeric(Btu)) %>%
  mutate(State = factor(State, levels = ST))
  
cons <- read.csv('data/energy_cons2016_eia.csv', skip = 1, stringsAsFactors = FALSE)[, 1:6] %>%
  filter(State %in% ST)%>%
  gather(Source, Btu, 2:6) %>%
  mutate(Btu = as.numeric(Btu)) %>%
  mutate(State = factor(State, levels = ST2))

pcap <- read.csv('data/energy_percap_cons2016_eia.csv', stringsAsFactors = FALSE)[, 1:4] %>%
  filter(State %in% c('PA', 'NY', 'MD', 'WV', 'VA', 'DC', 'DE')) %>%
  rename(Btu = Consumption.per.Capita..Million.Btu) %>%
  select(State, Btu)

## plot data
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

ggplot(pcap) + 
  geom_col(aes(reorder(State, -Btu), Btu)) + 
  ggtitle("Per Capita Energy Consumption (2016) in the Chesapeaky Bay's 7 Jurisdictions") + 
  scale_y_continuous(name = 'Total Consumption (Million Btu per capita)') + 
  scale_x_discrete(name = 'State')

prod_sum <- prod %>%
  group_by(Source) %>%
  summarise(Btu_sum = sum(Btu, na.rm = TRUE)) %>%
  filter(!(Source %in% c('Biofuels', 'Other.Renewables', 'Total')))

ggplot(prod_sum) +
  geom_col(aes(reorder(Source, -Btu_sum), Btu_sum)) + 
  ggtitle("Energy Production (2016) in the Chesapeaky Bay's 7 Jurisdictions") + 
  scale_y_continuous(name = 'Total Production (Trillion Btu)') + 
  scale_x_discrete(name = 'Energy Sector')

cons_sum <- cons %>%
  group_by(Source) %>%
  summarise(Btu_sum = sum(Btu, na.rm = TRUE)) %>%
  filter(!(Source %in% c('Biofuels', 'Other.Renewables', 'Total')))

ggplot(cons_sum) +
  geom_col(aes(reorder(Source, -Btu_sum), Btu_sum)) + 
  ggtitle("Energy Consumption (2016) in the Chesapeaky Bay's 7 Jurisdictions") + 
  scale_y_continuous(name = 'Total Production (Trillion Btu)') + 
  scale_x_discrete(name = 'Consumption Sector')
         