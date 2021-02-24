### Tidy Tuesday - Employed Status ###
### Created by: Emily Wilson ###########
### Updated on: 2021-02-23 #############

### Load libraries #####################

library(tidyverse)
library(tidytuesdayR)
library(here)
library(lubridate)
library(scales)
library(calecopal)


### Load data ##########################

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')


### Start graphing! ####################

earn_graph <- earn %>% 
  filter(!sex == "Both Sexes") %>% # remove "Both sexes" so only male and female remain
  filter(age %in% c("16 to 19 years", "20 to 24 years", "25 to 34 years", "24 to 54 years", "34 to 44 years", "45 to 54 years", 
                    "55 to 64 years", "65 years and over")) %>% # filter out weird other age classes that kept messing me up
  mutate(date = (yq(paste(year, quarter, sep = "-")))) %>% # set year and quarter into date format
  select(sex, age, date, median_weekly_earn) # select the columns I want to use

pal <- cal_palette("tidepool")[c(1, 6)] # set color palette I want to use
ban <- cal_palette("calochortus")[1] # set panel title background color
back <- cal_palette("coastaldune1")[4] # set plot background color

 ggplot(earn_graph, # choose data
         aes(x = date, # set date for x axis
             y = median_weekly_earn, # set weekly median income for y axis
             color= sex)) + # set color to sex
  geom_point(size = 1) + # set scatterplot and point size
   scale_color_manual(values = pal) + # set colors to use
  facet_wrap(~age, # facet wrap by age for multiple plots
             nrow = 1) + # set all plots in the same row for side-by-side comparison
    theme_bw() + # set theme
   scale_x_date(date_breaks = "3.5 years", # set scale breaks
                labels = date_format("%Y")) + # set label format
   labs(title = "Income Inequality by Sex", # add title 
        subtitle = "Median weekly earnings for women and men by age group", # add subtitle
        x = "Date", # capitalize x axis label
        y = "Median Weekly Income", # capitalize y axis label
        caption = "Visualization: Emily Wilson (@emwilson243) | Source: BLS", # add caption
        color = "Sex") + # capitalize legend title
   theme(strip.background = element_rect(fill = ban), # set plot label panel background to the color I wanted
         plot.background = element_rect(fill = back), # set plot background color
         legend.background = element_rect(colour = 'black', # give legend a black outline
                                          size = 0.25), # change thickness of legend outline
         legend.position = c(.08, .75), # move legend
         plot.margin = unit(c(0.6, 0.5, 0.5, 0.5), "cm"), # mess with plot margins for more space
         plot.title = element_text(vjust = 2.5), # space things out a bit
         plot.subtitle = element_text(vjust = 2), # space things out a bit
         plot.caption = element_text(vjust = -2), # space out plot caption
         axis.title.x = element_text(vjust = -2), # space out x axis label
         axis.text.x = element_text(size = 8), # make x label text smaller so they don't overlap
         axis.text.y = element_text(size = 8)) + # make y label text smaller to match x text
   ggsave(here("20210223_employedstatus", "outputs", "income.png"), width = 9, height = 4, unit = "in") # save to outputs folder for this week
    

























