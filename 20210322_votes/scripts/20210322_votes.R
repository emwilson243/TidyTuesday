### Tidy Tuesday - Votes ###
### Created by: Emily Wilson ###########
### Updated on: 2021-03-22 #############

### Load libraries #####################

library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(here)
library(ggridges) # for making a ridgeline plot
library(magick) # add image
library(calecopal) # for colors

### Load data ###########################################################################################################################

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

votes <- issues %>% # join the data sets together
  left_join(roll_calls, "rcid") %>% 
  left_join(unvotes, "rcid")


# I'm sure there was an easier and much more efficient way to do this, but here's how I got the number of issues
# per topic per year

votes_graph <- votes %>% 
  mutate(year = year(date)) %>% # extract year from the date
  count(year, rcid, issue) # count occurrences of each issue by date (gets country out of the way to plot)


# Start Graphing! #########################################################################################################################

pal <- cal_palette("seagrass") # assign color palette


un <- image_read("https://pngimg.com/uploads/un/un_PNG5.png") %>% # read in a UN logo png
  image_resize("300x300") # make UN logo smaller


ggplot(votes_graph,
       aes(x = year,
           y = issue,
           fill = issue)) +
       geom_density_ridges() + # ridge geom
  scale_fill_discrete(type = pal) + # fill with preassigned palette
  scale_x_continuous(limits = c(1946, 2019), # set x limits
                     breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), # set x breaks
                     expand = c(0.03, 0.03)) + # resize some empty space
  scale_y_discrete(expand = c(0.05, 0.05)) + # resize some empty space
  theme(panel.background = element_blank(), # get rid of panel background
        legend.position = "none", # get rid of legend ( I just wanted different pretty colors)
        axis.title.x = element_text(vjust = -3, # space out x axis title
                                    size = 12, # make x axis title bigger
                                    face = "bold"), # make x axis title bold
        axis.title.y = element_text(vjust = 5, # space out y axis title
                                    size = 12, # make y axis title bigger
                                    face = "bold"), # make y axis title bold
        plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "cm"), # change plot margins (top, right, bottom, left))
        plot.caption = element_text(vjust = -8), # space out plot caption (down)
        plot.title = element_text(vjust = 2, # adjust vertical title position
                                  hjust = -0.735, # adjust horizontal title position
                                  size = 20, # make title text bigger
                                  face = "bold"), # make title bold
        plot.subtitle = element_text(vjust = 2, # adjust vertical subtitle position
                                     hjust = -1.19, # adjust horizontal subtitle position
                                     size = 12)) + # change subtitle size
  labs(title = "United Nations Voting Issues", # add title 
      subtitle = "Frequency of issues voted on by the UN each year from 1946 to 2019", # add subtitle
      caption = "Visualization: Emily Wilson (@emwilson243) | Source: Harvard's Dataverse", # add caption
      x = "Year", # capitalize x axis title
      y = "Issues") + # capitalize y axis title
  ggsave(here("20210322_votes", "outputs", "votesplot.png"), width = 10, height = 5, unit = "in") # save plot as image so magick will work


votesplot <- image_read(here("20210322_votes", "outputs", "votesplot.png")) # read in that plot image


out <- image_composite(votesplot, un, # composite the plot and logo images
                       offset = "+2650+35") # tell R where to put the logo

image_write(out, path = "~/Desktop/ /R Projects/Computer_Modeling_Hub/TidyTuesday/20210322_votes/outputs/20210322_votes.png", format = "png") # save final plot








