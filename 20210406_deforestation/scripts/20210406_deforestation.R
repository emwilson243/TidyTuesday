### Tidy Tuesday - Deforestation ###
### Created by: Emily Wilson ###########
### Updated on: 2021-04-06 #############

### Load libraries #####################
library(tidyverse)
library(here)
library(ggstream) # for stream plot
library(ggtext) # for changing colors of certain words

### Load data ################################################################################################################################
brazil_loss <- # read in data
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv'
  )

# Start Graphing! ############################################################################################################################

defor <- brazil_loss %>%
  pivot_longer( # get data into long format
    cols = commercial_crops:small_scale_clearing, # choose which columns to convert
    names_to = "cause", # make column for current column names
    values_to = "loss" # make column for values
  ) %>% 
  select(year:loss) %>% # only select needed columns
  filter(
    cause %in% c( # only keep the causes I want; top five so they look better graphed
      "pasture",
      "small_scale_clearing",
      "commercial_crops",
      "fire",
      "selective_logging"
    )
  )

pal <- # put together color palette
  c("midnightblue",
    "darkolivegreen",
    "cadetblue",
    "steelblue4",
    "burlywood4")

ggplot(defor, aes(x = year, y = loss, fill = cause)) +
  geom_stream() + # use stream plot
  scale_fill_manual(values = pal) + # add color palette
  theme_void() + # remove basically everything
  labs(
    title = "**Loss of Brazilian Forest**", # add title
    subtitle = "Relative contributions to the loss of Brazilian forest in hectares due to <span style='color:midnightblue'>**production of commercial crops**</span>, <span style='color:darkolivegreen'>**fires**</span>, <span style='color:cadetblue'>**conversion to pastures**</span>, <span style='color:steelblue4'>**selective logging**</span>, and <span style='color:burlywood4'>**small-scale clearing**</span> from 2001 to 2013.", # add formatted subtitle
    caption = "Visualization: Emily Wilson (@emwilson243) | Source: Our World in Data", # add caption
    x = "Year" # capitalize x axis title
  ) +
  theme(
    axis.text.x = element_text(size = 10), # change axis text size
    axis.title.x = element_text(size = 14, vjust = -7), # change axis title size and vertical position
    plot.title = element_markdown( # set plot title as markdown element so I can bold it
      margin = margin(t = 0, b = 10), # mess with margins since vjust didn't work for some reason
      hjust = 0.5, # center title
      size = 26 # increase title size
    ),
    plot.subtitle = element_markdown(hjust = 0.5, size = 12), # set plot subtitle as markdown element so I can bold it and change word colors
    plot.caption = element_text(vjust = -17, hjust = 0.95), # adjust caption placement
    plot.margin = unit(c(1, -1, 1.5, -1), "cm"), # adjust plot margin for more room
    plot.background = element_rect(fill = "mintcream"), # make plot background color prettier
    legend.position = "none" # get rid of legend
  ) +
  scale_x_continuous(
    breaks = c(2002, 2004, 2006, 2008, 2010, 2012), # set scale breaks
    labels = c(2002, 2004, 2006, 2008, 2010, 2012) # label scale breaks
  ) +
  ggsave( # save to outputs folder for this week
    here("20210406_deforestation", "outputs", "deforestation.png"),
    width = 15.5,
    height = 8,
    unit = "in"
  ) 
