# Load libraries
library(tidyr)
library(ggplot2)
library(plotly)
library(stringr)
library(dplyr)
library(scales)
library(maps)

# Load datasets
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

jail_juridiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

# Summary incaceration dataset

# Most black jail and most white jail
from_1999 <- incarceration %>%
  filter(year >= 1999)

# Counties in each state with most black rate
most_black_jails <- from_1999 %>%
  select(year, state, county_name, black_jail_pop_rate) %>%
  group_by(year, state) %>%
  filter(black_jail_pop_rate == max(black_jail_pop_rate, na.rm = TRUE)) %>%
  ungroup(year, state) 

wa_most_black_counties <- most_black_jails %>%
  filter(state == 'WA') %>%
  select(c(1,3))

wa_black_table <- table(wa_most_black_counties)
View(wa_black_table)

most_white_jails <- from_1999 %>%
  select(year, state, county_name, white_jail_pop_rate) %>%
  group_by(year, state) %>%
  filter(white_jail_pop_rate == max(white_jail_pop_rate, na.rm = TRUE)) %>%
  ungroup(year, state)

wa_most_white_counties <- most_white_jails %>%
  filter(state == "WA") %>%
  select(c(1,3))

wa_white_table <- table(wa_most_white_counties)
  
# Most admission year
admission <- incarceration %>%
  group_by(year,state) %>%
  select(year, state, contains("adm")) %>%
  filter(year > 1999) %>%
  summarize(total_jail_adm = sum(total_jail_adm, na.rm = TRUE))

# Average admission every year
ave_adm <- admission %>%
  group_by(year) %>%
  summarise(
    ave_jail_adm = round(mean(total_jail_adm, na.rm = TRUE))
  )

# State with the most yearly admission
max_admission <- admission %>%
  group_by(year) %>%
  filter(total_jail_adm == max(total_jail_adm, na.rm = TRUE)) %>%
  ungroup(year) %>%
  select(c(0:3)) %>%
  rename(max_jail_state_adm = total_jail_adm)

summary_table <- left_join(max_admission, ave_adm,  by = "year")

# Jail population from 1999
jail_pop_1999 <- incarceration %>%
  filter(year > 1999) %>%
  select(year,state, county_name, urbanicity, contains("jail_pop"))

urbanicity_and_jail <- jail_pop_1999 %>%
  group_by(year,state, urbanicity) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  filter(urbanicity != "")

total_jail_from_99 <- jail_pop_1999 %>%
  group_by(year) %>%
  summarise(total_pop = round(sum(total_jail_pop,na.rm = TRUE)))

summary_table <- right_join(summary_table, total_jail_from_99, by = "year")

# Take the data from 2000 and onward
from_1990s <- incarceration %>%
  filter(year >= 1990)

# Number of prisoners by race by year from 2000
by_races <- from_1990s %>%
  group_by(year, state) %>%
  summarise(
    White = sum(white_jail_pop, na.rm = TRUE),
    Black = sum(black_jail_pop, na.rm = TRUE),
    Latinx = sum(latinx_jail_pop, na.rm = TRUE),
    Aapi = sum(aapi_jail_pop, na.rm = TRUE),
    Native = sum(native_jail_pop, na.rm = TRUE),
  ) %>%
  gather(
    Race,
    Population,
    -state,
    -year
  ) %>%
  group_by(year, Race) %>%
  summarise(total_jail_population = sum(Population))
# Graph of number of prisoners by years
time_graph <- ggplot(by_races, aes(x = year, y = total_jail_population)) +
  geom_line(aes(color = Race)) + 
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  ylab("Jail population")
time_graph

# Variable graph 

wa_urbanicity_2016 <- urbanicity_and_jail %>%
  filter(year == "2016" & state == "WA") %>% 
  ungroup(year, state) %>%
  select(urbanicity,contains("jail_pop")) %>%
  select(urbanicity, c(9:14)) %>%
  rename(AAPI = aapi_jail_pop, 
         White = white_jail_pop,
         Black = black_jail_pop,
         Latin = latinx_jail_pop,
         Other = other_race_jail_pop, 
         Native = native_jail_pop) %>%
  gather (
    Race, 
    Population,
    -urbanicity
  )

variable_graph <- ggplot(wa_urbanicity_2016, aes(x = urbanicity, y = Population, fill = Race)) +
  geom_bar(position = "stack", stat = "identity", size = 1) +
  theme_minimal()
variable_graph
# Washington in most recent year

wa_recent <- incarceration %>%
  filter (year == max(year), state == "WA") %>%
  mutate (county_name = str_trim(str_replace(county_name, "County", ""))) %>%
  rename (county = county_name)
  
# Washington state map

wa_state_map <- map_data("county") %>%
  filter(region == "washington") %>%
  mutate(subregion = str_to_title(subregion)) %>%
  rename(county = subregion) %>%
  left_join(wa_recent, by = "county")

minimalist <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
   
wa_state_graph <- ggplot(wa_state_map, aes(long, lat, group = group)) + 
  geom_polygon(color = "white", aes(fill = total_jail_pop)) +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  minimalist
ggplotly(wa_state_graph)
  