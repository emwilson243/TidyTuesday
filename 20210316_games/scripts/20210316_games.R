### Tidy Tuesday - Video Games###
### Created by: Emily Wilson ###########
### Updated on: 2021-03-16 #############

### Load libraries #####################

library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(here)

### Load data ###########################################################################################################################
videogames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

games <- videogames %>% 
  filter(gamename %in% c("ARK: Survival Evolved", "Warframe", "PlanetSide 2", "PAYDAY 2", "Factorio", "Stellaris"),  # filter out only the ones I want to plot
         year == 2020) %>% # only use 2020 data
  mutate(month = as.factor(month)) # make month a factor so I can relevel

levels(games$month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") # relevel months

# Start plotting! #######################################################################################################################

  ggplot(games,
         aes(x = month, # set x as time
             y = gamename, # set y as the game
             size = avg, # size points by the average number of players at the same time
             color = gain)) + # color points by average gain or loss of players
  geom_point() + # set geom to point
  scale_color_viridis_c(option = "B") + # set color scale
  theme_minimal() + # get rid of all the theme bells and whistles
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + # change month labels
  coord_equal(1) + # make squares equal width/height
    labs(title = "Video Game Popularity", # add title
         subtitle = "Average simultaneous monthly players & differences in players compared to the previous month for selected games on Steam in 2020", # add subtitle
         caption = "Visualization: Emily Wilson (@emwilson243) | Source: SteamCharts", # add caption
         x = "Month", # change x axis title
         y = "Game", # change y axis title
         size = "Avg Players", # change legend title
         color = "Gain or Loss") + # change legend title
  theme(plot.margin = unit(c(1, 1, 1.5, 1), "cm"), # change plot margins (top, right, bottom, left)
        plot.caption = element_text(vjust = -17, hjust = 1.35), # space plot caption out as well
        panel.grid = element_line(color = "grey60"),
        legend.key.size = unit(0.5, "cm"),  # make legends smaller
        axis.title.x = element_text(vjust = -8), # space x axis title out a bit
        legend.title.align = 0.5, # center legend titles
        plot.title = element_text(vjust = 6, hjust = -0.277), # space title out, move left
        plot.subtitle = element_text(vjust = 6, hjust = 1.055), # space subtitle out, move left
        plot.background = element_rect(fill = "grey87")) + # fill plot background
  ggsave(here("20210316_games", "outputs", "games.png"), width = 11, height = 5.85, unit = "in") # save to outputs folder for this week


# could have used facet_grid(~year) to get this for multiple years- looked cool but ended up being way too "busy"

