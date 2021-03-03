### Tidy Tuesday - Super Bowl Ads ###
### Created by: Emily Wilson ###########
### Updated on: 2021-03-01 #############

### Load libraries #####################

library(tidyverse)
library(tidytuesdayR)
library(here)

### Load data ##########################

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

### Start graphing! ####################

commercials <- youtube %>% 
  select(year, funny:use_sex) %>%  # only select columns of interest
  pivot_longer(cols = funny:use_sex, # specify columns to pivot
               names_to = "variable", # specify where column names go
               values_to = "use") %>% # specify where values go
  mutate(use_bin = as.integer(use)) %>%  # convert true/false to binary
  group_by(year, variable) %>% # group by year and variable
  summarize (avg_use = mean(use_bin)) #average use of categories by year


  ggplot(commercials,
         aes(x = year,
             y = variable,
             fill = avg_use)) +
    geom_tile(color = "white", size = 0.5) + # assign sizes and margin colors to the titles
    coord_equal() + # make the side lengths of the tiles equal
    scale_fill_gradient(low = "white", # give low color scale value
                        high = "dodgerblue4", # give high color scale value
                        breaks = c(0, 1), # assign continuous fill scale breaks
                        labels = c("Never", "Always")) + # label low and high scale values
    theme(axis.ticks.y = element_blank(), # remove axis ticks
          panel.border = element_blank(), # remove panel border
          panel.background = element_blank(), # remove panel background
          legend.position = c(0.87, 1.21), # put legend on top of plot
          legend.background = element_blank(), # remove legend background
          legend.direction = "horizontal", # make legend horizontal
          plot.background = element_rect(fill = "grey90"), # fill plot background
          plot.margin = unit(c(1, 0.75, 0.5, 0.5), "cm"), # mess with plot margins for more space
          plot.caption = element_text(size = 6), # make caption text smaller
          axis.title.x = element_text(vjust = -1), # space out x axis label
          plot.title = element_text(vjust = 4,  # space out plot title
                                    size = 20,  # change title size
                                    hjust = -0.5), # move title left
          plot.subtitle = element_text(vjust = 7.5, # space out plot subtitle
                                       hjust = -0.445, # move to left
                                       size = 10)) +  # change subtitle size
    labs(title = "Super Bowl Commercials", # add title 
         subtitle = "Use of common commercial attributes by year", # add subtitle
         x = "Year", # capitalize x axis label
         y = "Topic", # capitalize y axis label
         caption = "Visualization: Emily Wilson\nSource: FiveThirtyEight", # add caption
         fill = element_blank()) + # capitalize legend title
  scale_x_continuous(expand = c(0, 0)) + # remove weird extra space from x axis
    scale_y_discrete(labels = c("Animals", "Celebrity", "Danger", "Funny", "Patriotic", "Shows Product Quickly", "Uses Sex")) + # label y axis with capitalization 
    ggsave(here("20210301_superbowl", "outputs", "commercial.png"), width = 8, height = 3.8, unit = "in") # save to outputs folder for this week
  