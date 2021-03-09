### Tidy Tuesday - Bechdel Test###
### Created by: Emily Wilson ###########
### Updated on: 2021-03-09 #############

### Load libraries #####################

library(tidyverse)
library(tidytuesdayR)
library(here)
library(calecopal)

### Load data ###########################################################################################################################
bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')

# Start plotting! #######################################################################################################################

pal <- cal_palette("seagrass")[c(2, 6, 3)]

bechdel %>% 
  mutate(rating = as.character(rating)) %>% # change rating to character so it's ranked and R will work with it latr
  filter(!rating == 0) %>%  # remove unscored movies
  group_by(year, rating) %>% 
  summarize(occur = n()) %>% # get total cases per rating per year
  mutate(freq = (occur / sum(occur)* 100)) %>% # get percentage of ratings for each year
  filter(year >= "1935" & year < "2021") %>% # change years included bc it's a little funky in the older years
ggplot(., 
       aes(x = year,
            y = freq,
            fill = rating)) +
    geom_area() +
  scale_fill_manual(values = pal) +
  labs(title = "The Bare Minimum: Over 80 Years of Bechdel Successes and Failures", # add title 
       subtitle = "Films including at least two named women (rating = 1) who talk to each other (rating = 2) about something other than a man (rating = 3).
       Films with a rating of three have passed the Bechdel test, allowing their female characters at least the bare minimum of depth.", # add subtitle
       x = "Year", # capitalize x axis label
       y = "Percent", # capitalize y axis label
       caption = "Visualization: Emily Wilson (@emwilson243) | Source: FiveThirtyEight", # add caption
       fill = "Rating") + # capitalize legend title
  scale_x_continuous(expand = c(0, 0)) + # remove weird extra space from x axis
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "ivory"), # fill plot background
        plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # mess with plot margins for more space
        plot.title = element_text(vjust = 10, hjust = 0.5), # space things out a bit, ceter title
        plot.subtitle = element_text(vjust = 8, hjust = 0.5, size = 10), # space things out a bit, center subtitle
        plot.caption = element_text(vjust = -13, hjust = 0.5), # space out and center plot caption
        axis.title.y = element_text(size = 12), # change y axis title size
        axis.title.x = element_text(vjust = -5, size = 12)) + # space out x axis label, change size
ggsave(here("20210309_bechdeltest", "outputs", "bechdel.png"), width = 10, height = 6, unit = "in") # save to outputs folder for this week
















