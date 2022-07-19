# Requried File Installation
library(knitr)
library(magrittr)
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(gridExtra)
library(zoo)
library(psych)
library(lubridate)
library(grid)

# Part 1
# Import & View Uber Pickup Dataset
uber_nyc_enriched <- read_csv("Desktop/uber_nyc_enriched.csv")
View(uber_nyc_enriched)
str(uber_nyc_enriched)

# Part 2
# Univariate Plots
histogram <- function(varname, bs = NULL, bw = NULL){
  h <- ggplot(uber.spread, aes_string(varname)) + geom_histogram(bins = bs, binwidth = bw)
  return(h)
}

ggplot(uber, aes(pickups)) +
  geom_histogram() +
  scale_x_sqrt() +
  scale_y_sqrt()

# In Specific Borough
ggplot(uber_nyc_enriched, aes(pickups)) +
  geom_histogram(aes(fill = borough)) +
  scale_x_sqrt(breaks = c()) +
  scale_y_sqrt()

# Bivariate Plots
uber.spread <- uber.spread %>% 
  mutate(pickups = Bronx +Brooklyn + EWR + Manhattan + Queens + `Staten Island`
         + !is.na(Unknown)) %>% 
  mutate(day = day(pickup_dt)) %>%
  mutate(hour = hour(pickup_dt)) %>%
  mutate(week = week(pickup_dt)) %>% 
  mutate(wday = wday(pickup_dt, label = TRUE)) %>% 
  mutate(workday = ifelse(wday == 'Sat' | wday == 'Sun' | 
                            hday == 'Y', 'N', 'Y')) %>% 
  mutate(yday = yday(pickup_dt))

uber <- uber %>% 
  mutate(day = day(pickup_dt)) %>%
  mutate(hour = hour(pickup_dt)) %>%
  mutate(week = week(pickup_dt)) %>% 
  mutate(wday = wday(pickup_dt, label = TRUE)) %>% 
  mutate(workday = ifelse(wday == 'Sat' | wday == 'Sun' | 
                            hday == 'Y', 'N', 'Y')) %>% 
  mutate(yday = yday(pickup_dt))

pairs.panels(uber.spread %>% dplyr::select(Bronx:Unknown, spd:sd), cex = 0.4)

# Part 3
# Result Part

# Pickups Distribution per hour by Borough
uber_nyc_enriched$borough <- factor(uber_nyc_enriched$borough, levels = c('Manhattan', 'Brooklyn', 
                                                'Queens', 'Bronx', 
                                                'Staten Island', 'EWR'))

ggplot(subset(uber_nyc_enriched, !is.na(borough)), aes(pickups)) +
  geom_histogram(aes(fill = borough), bins = 40) +
  scale_x_sqrt() +
  facet_wrap(~ borough, ncol = 2, scales = 'free') +
  labs(title = 'Pickups Distribution per hour by Borough', 
       x = 'Pickups per hour', y = 'Count') +
  theme(plot.title = element_text(size = 22, hjust = 0.5), 
        legend.position = 'none', axis.title = element_text(size = 16))

# Demand through the week per borough
h1 <- ggplot(uber.spread, aes(x = wday, y = hour, fill = Manhattan)) +
  geom_tile() +
  scale_fill_distiller(palette = 'Spectral') +
  labs(title = 'Manhattan', x = 'Day', y = 'Time', fill = 'Pickups per hour') +
  theme(plot.title = element_text(hjust = 0.5))

h2 <- ggplot(uber.spread, aes(x = wday, y = hour, fill = Brooklyn)) +
  geom_tile() +
  scale_fill_distiller(palette = 'Spectral') +
  labs(title = 'Brooklyn', x = 'Day', y = 'Time', fill = 'Pickups per hour') +
  theme(plot.title = element_text(hjust = 0.5))

h3 <- ggplot(uber.spread, aes(x = wday, y = hour, fill = Queens)) +
  geom_tile() +
  scale_fill_distiller(palette = 'Spectral') +
  labs(title = 'Queens', x = 'Day', y = 'Time', fill = 'Pickups per hour') +
  theme(plot.title = element_text(hjust = 0.5))

h4 <- ggplot(uber.spread, aes(x = wday, y = hour, fill = Bronx)) +
  geom_tile() +
  scale_fill_distiller(palette = 'Spectral') +
  labs(title = 'Bronx', x = 'Day', y = 'Time', fill = 'Pickups per hour') +
  theme(plot.title = element_text(hjust = 0.5))

h5 <- ggplot(uber.spread, aes(x = wday, y = hour, fill = `Staten Island`)) +
  geom_tile() +
  scale_fill_distiller(palette = 'Spectral') +
  labs(title = 'Staten Island', x = 'Day', y = 'Time', fill = 'Pickups per hour') +
  theme(plot.title = element_text(hjust = 0.5))

h6 <- ggplot(uber.spread, aes(x = wday, y = hour, fill = EWR)) +
  geom_tile() +
  scale_fill_distiller(palette = 'Spectral') +
  labs(title = 'EWR', x = 'Day', y = 'Time', fill = 'Pickups per hour') +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(h1,h2,h3,h4,h5,h6, ncol = 2, 
             top = grid::textGrob("Demand through the week per borough", 
                                  gp = grid::gpar(fontsize=22)))
