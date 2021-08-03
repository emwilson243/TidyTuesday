### Tidy Tuesday - Post Offices ###
### Created by: Emily Wilson ###########
### Updated on: 2021-08=02 #############

### Load libraries ##############################################################################
library(tidyverse)
library(here)
library(maps)
library(gganimate)

### Get data ####################################################################################

postoffices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

### Start graphing! #############################################################################

states <- map_data("state") # get data to map the united states

postoffices <- postoffices %>% 
  select(state, established) %>% # only keep the state and the years post offices were established there
  group_by(state, established) %>% # group by those categories
  count(established) %>%# get count of how many post offices were established per state per year
  filter(established > 200) %>% # remove weird nonsensical values
  mutate(region = recode(state, # change state abbreviations to something that will play nicely with map data
                        AK = "alaska",
                        AL = "alabama",
                        AZ = "arizona",
                        AR = "arkansas",
                        CA = "california",
                        CO = "colorado",
                        CT = "connecticut",
                        DE = "delaware",
                        FL = "florida",
                        GA = "georgia",
                        HI = "hawaii",
                        IA = "iowa",
                        ID = "idaho",
                        IL = "illinois",
                        IN = "indiana",
                        KS = "kansas",
                        KY = "kentucky",
                        LA = "louisiana",
                        MA = "massachusetts",
                        MD = "maryland",
                        ME = "maine",
                        MI = "michigan",
                        MN = "minnesota",
                        MS = "mississippi",
                        MO = "missouri",
                        MT = "montana",
                        NE = "nebraska",
                        NV = "nevada",
                        NH = "new hampshire",
                        NJ = "new jersey",
                        NM = "new mexico",
                        NY = "new york",
                        NC = "north carolina",
                        ND = "north dakota",
                        OH = "ohio",
                        OK = "oklahoma",
                        OR  = "oregon",
                        PA = "pennsylvania",
                        RI = "rhode island",
                        SC = "south carolina",
                        SD = "south dakota",
                        TN = "tennessee",
                        TX = "texas",
                        UT = "utah",
                        VT = "vermont",
                        VA = "virginia",
                        WA = "washington",
                        WV = "west virginia",
                        WI = "wisconsin",
                        WY = "wyoming",
                        DC = "district of columbia")) %>% 
  right_join(states, by = "region") %>% # join post office data with map data
  select(region, established, n, long, lat, group, order) # only keep columns I want to use

office_plot <- ggplot() + # assign plot to object so I can change animation transitions later
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "white", color = "black") + # put down base layer for map, even when a state doesn't have a post office established in a given year
  geom_polygon(data = postoffices, aes(x = long, y = lat, group = group, fill = n)) + # put down map to fill with state data
  scale_fill_viridis_c() + # use viridis palette
  coord_fixed(1.3) + # fix map coordinates so it doesn't look weird
  theme_void() + # remove unecessary chart elements
  theme(plot.margin = unit(c(0.3, 0.2, 0.2, 0.7), "cm")) + # give more empty space for aesthetics
  transition_manual(established) + # transition by year
  labs(title = "Post offices established in each mainland state in {current_frame}", # add title
       caption = "Visualization: Emily Wilson (@emwilson243) | Source: Cameron Blevins & Richard W. Helbock", # add caption
       fill = "Amount") # change legend label

postoffice_plot <- animate(office_plot, fps = 4) # alter rate of change
anim_save("postoffices.gif", path = here("20210413_postoffices", "outputs")) # save to outputs folder





