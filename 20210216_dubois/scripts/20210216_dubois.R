### Tidy Tuesday - DuBois Challenge ###
### Created by: Emily Wilson ###########
### Updated on: 2021-02-10 #############

### Load libraries #####################

library(tidyverse)
library(tidytuesdayR)
library(here)
library(extrafont)

### Load data ##########################

freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv') # read in and assign data using Tidy Tuesday package
view(freed_slaves) #look at freed slaves data

freed_slaves <- freed_slaves %>%
  pivot_longer(cols = 2:3) # pivot_longer so I can get values for freed and enslaved in the same column for a stacked area chart

### Start graphing! ####################
cols <- c("seagreen", "black") # set colors to use

ggplot(freed_slaves, #tell R what data to use
       aes(x = Year, y = value, fill = name)) + #assign axes
  geom_area() + # set up for an area plot
  scale_fill_manual(values = cols) + # color the areas
  labs(title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .\n \n \nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE .", # add title (with appropriate blank lines in between and double-spacing on punctuation)
       subtitle = "DONE BY ATLANTA UNIVERSITY .",  # add subtitle
       caption = "Vizualization: Emily Wilson (@emwilson243)\nSource: #TidyTuesday / #DuBoisChallenge") + # add caption
  theme(legend.position = "none", # remove legend
        plot.background = element_rect(fill = "antiquewhite", colour = "NA"),  # change background color to look all old-timey
        axis.line.y = element_blank(), # remove y axis line
        axis.text.y = element_blank(), # remove y axis text
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.title.y = element_blank(), # remove y axis title
        axis.title.x = element_blank(), # remove x axis title
        axis.ticks.x = element_blank(), # remove x axis ticks
        panel.border = element_rect(colour = "black", fill = "NA", size = 0.5), # make black border for panel
        panel.background = element_rect(fill = "seagreen"), # fill background for that 1800 point where things don't add up to 100% (just to match given chart)
        panel.grid.major = element_blank(), # remove major grid lines
        panel.grid.minor = element_blank(), # remove minor grid lines
        plot.margin = unit(c(3, 1, 0.05, 1), "cm"), # widen plot margin
        plot.title = element_text(hjust = 0.5, #center title
                                  vjust = 27), # space things out
        plot.subtitle = element_text(hjust = 0.5, # center subtitle
                                     vjust = 32, # space things out
                                     size = 8), # make smaller
        text = element_text(family = "Andale Mono", face = "bold"), # change font
        axis.text.x = element_text(size = 13, face = "bold")) +
  scale_x_continuous(limits = c(1790, 1870), expand = c(0,0),  # set limits and remove blank space
                     position = "top", # put x axis on top
                     breaks = c(1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870)) + # set breaks
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) + # set limits and remove blank space
  annotate("text", x = 1792, y = 93.5, label = "8%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1800, y = 90, label = "11%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1810, y = 88.5, label = "13.5%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1820, y = 88.5, label = "13%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1830, y = 87.5, label = "14%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1840, y = 88.5, label = "13%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1850, y = 89.5, label = "12%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1860, y = 90.5, label = "11%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1867, y =90, label = "100%", family = "Andale Mono", size = 4) + # add value annotation
  annotate("text", x = 1830, y = 96, label = "FREE - LIBRE",  family = "Andale Mono", size = 6) + # add top annotation
  annotate("text", x = 1830, y = 58, label = "SLAVES", color = "white", family = "Andale Mono", size = 9) + # add middle annotation (pt 1)
  annotate("text", x = 1830, y = 53.6, label = "ESCLAVES", color = "white", family = "Andale Mono", size = 9) + # add middle annotation (pt 2)
  ggsave(here("20210216_dubois", "outputs", "dubois.png"), width = 7.5, height = 9.8) # save to outputs folder for this week











