### Tidy Tuesday - Makeup Shades ###
### Created by: Emily Wilson ###########
### Updated on: 2021-03-30 #############

### Load libraries #####################
library(tidyverse)
library(here)
library(ggalluvial) # for  making alluvial plot
library(ggbeeswarm)

### Load data ###########################################################################################################################
allNumbers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allNumbers.csv') # import data


# Start Graphing! #########################################################################################################################

makeup_brands <- allNumbers %>% # get total brands with how many use light to dark shade sorting
  drop_na() %>% # remove NAs
  select(brand, lightness, lightToDark) %>% # select only columns I need
  group_by(brand, lightToDark) %>% # get brand and whether they sort from light to dark
  summarize(avgshade = mean(lightness)) # get average foundation color (I was curious)

makeup_brands[17, 1] <- "Hourglass" # fix messed up capitalization
makeup_brands[18, 1] <- "Huda Beauty" # fix messed up capitalization
makeup_brands[24, 1] <- "Kevyn Aucoin" # fix messed up capitalization
makeup_brands[35, 1] <- "Pat McGrath Labs" # fix messed up capitalization
makeup_brands[38, 1] <- "Sephora Collection" # fix messed up capitalization
makeup_brands[41, 1] <- "Smith & Cult" # fix messed up capitalization

makeup_brands$lightToDark <- gsub("TRUE", "Yes", makeup_brands$lightToDark) # change true to yes
makeup_brands$lightToDark <- gsub("FALSE", "No", makeup_brands$lightToDark) # change false to no

median <- median(allNumbers$lightness) # get median shade color

p1 <- ggplotGrob(ggplot(allNumbers, aes(x=lightness)) +
  geom_histogram(fill = "lightseagreen", bins = 50) + # lightness histogram of all shades included in the data set
  geom_vline(aes(xintercept = median(allNumbers$lightness)), linetype = "dashed") + # add vertical line at median
  theme_classic() + # good old classic theme
  scale_x_continuous(expand = c(0, 0.1)) + # expand the x axis a little
  scale_y_continuous(expand = c(0, 0)) + # get rid of weird y axis space
  labs(title = "Available Foundation Shades by Lightness", # add title
       caption = "0 indicates pure black shades, 1 indicates pure white shades", # add subtitle as "caption" to space text out
       x = "Lightness", # capitalize axis label
       y = "Frequency") + # capitalize axis label
  theme(plot.title = element_text(size = 10, hjust = -1, vjust = 1.5, face = "bold"), # resize, bold, and adjust spacing of title
        plot.caption = element_text(size = 8, hjust = 1.5, vjust = -0.6, face = "bold"), # resize, bold, and adjust spacing of caption
        plot.background = element_rect(color = "black"), # put black border around plot
        axis.title.x = element_text(vjust = -0.5)) + # adjust x axis spacing
  annotate("text", x = 0.46, y = 160, label = "Median = 0.67", size = 3)) # label median line with value


makeup_brands %>% 
  group_by(brand) %>% 
  ggplot(aes(axis1 = brand, axis2 = lightToDark)) + # alluvial plot!
  geom_alluvium(aes(fill = brand), color = "grey60") + # fill by brand, and make gray lines between streams
  scale_x_discrete(limits = c("Brand", "Light to Dark"), expand = c(0.1, 0.1)) + # I technically don't need an x axis since my theme removes it, but everything breaks if I take this out
  scale_fill_viridis_d() + # fill with viridis palette
  geom_stratum(color = "grey60") + # make stratum outlines grey
  geom_text(stat = "stratum", size = 3.5, aes(label = after_stat(stratum))) + # decide where the stratum breaks are
  theme_void() + # take out all unnecessary components with the void :)
  theme(legend.position = "none", # remove useless legend
        plot.margin = unit(c(-0.2, 0.5, -0.5, 0.5), "cm"), # mess with plot margin to fit everything
        plot.caption = element_text(vjust = 15, hjust = 0.96), # adjust position of caption
        panel.background = element_rect(fill = "white")) + # keeps background panel from being transparent when saved with ggsave
  labs(caption = "Visualization: Emily Wilson (@emwilson243) | Source: The Pudding") + # add caption
  annotation_custom(p1, xmin = 1.6, xmax = 2.17, ymin = 47.5, ymax = 62.5) + # add histogram to this plot
  annotate("text", x = 2, y = 46, label = "Sorts from Light to Dark", fontface = 2, size = 4) + # add label for sorting, since the x axis functions hate me today
  annotate("text", x = 1, y = 46, label = "Company", fontface = 2, size = 4) + # add label for company
  annotate("text", x = 1.16, y = 61.7, label = "Foundation Lightness", fontface = 2, size = 10) + # add, position, and format first part of title
  annotate("text", x = 1.07, y = 59, label = "& Shade Sorting", fontface = 2, size = 10) + # add, position, and format second part of title
  annotate("text", x = 1.195, y = 55, label = "Foundation shades available from different makeup brands (right);", size = 4) + # add, position, and format first line of "body" text
  annotate("text", x = 1.173, y = 54, label = "fewer foundations are available in darker shades compared to", size = 4) + # add, position, and format second line of "body" text
  annotate("text", x = 0.9, y = 53, label = "lighter ones.", size = 4) + # add, position, and format third line of "body" text
  annotate("text", x = 1.186, y = 51, label = "Many brands label their shades sequentially and may put lighter", size = 4) + # add, position, and format fourth line of "body" text
  annotate("text", x = 1.128, y = 50, label = "colors first, prioritizing them on store shelves (below).", size = 4) # add, position, and format last line of "body" text

# Changed 08/02/2021; ggsave update no longer allows it to be "added" to an object, so this line of code is freestanding now.
ggsave(here("20210330_makeup", "outputs", "makeup.png"), width = 9.3, height = 11.18, unit = "in") # save to outputs folder for this week

##########################################################################################################################################################################################

# Trying out scale_color_identity

brand_counts <- allNumbers %>% 
  select(brand) %>%
  count(brand) # getting how many products in different shades each brand offers so I can narrow it down to the top five brands

pop_brands <- allNumbers %>% 
  filter(brand %in% c("bareMinerals", "SEPHORA COLLECTION", "MAKE UP FOR EVER", "FENTY BEAUTY by Rihanna", "Lancôme")) %>% # only keep top five brands
  select(brand, hex, lightness) %>% # keep only the columns I want to use
  mutate(brand = as.factor(brand)) %>% # change brand to a factor so I can dictate levels
  mutate(brand = fct_relevel(brand, c("Lancôme", "FENTY BEAUTY by Rihanna", "MAKE UP FOR EVER", "SEPHORA COLLECTION", "bareMinerals"))) # rearrange factors so they rank correctly on graph

  ggplot(pop_brands, 
         aes(brand, # arrange by brand
             lightness, #organize points by lightness value
             color = hex), # assign color by hex code for each point
         size = 7) + #make points larger
  geom_quasirandom() + # get that cool violin plot effect with the points
  scale_color_identity() + # color each point by its hex code
    theme_minimal() + # remove extra graph elements
    theme(axis.line = element_blank(), # remove x axis line
          axis.title = element_blank(), # remove x axis title 
          axis.text.x = element_blank(), # remove x axis text
          axis.ticks.x = element_blank(), # remove x axis ticks
          axis.text.y = element_text(hjust = 0.5), # align y axis labels to middle
          panel.grid = element_blank(), # remove panel grid lines
          plot.title = element_text(hjust = -1, margin = margin(t=10), size = 25), # make the plot title larger and give it more space
          plot.subtitle = element_text(hjust = -1.55, margin = margin(t=10)), # give the plot subtitle more space
          plot.background = element_rect(fill = "white"), # make background white b/c not the default is transparent
          plot.margin = unit(c(0.3, 0.2, 0.2, 0.7), "cm")) + # give the plot elements more surrounding space
    coord_flip() + # switch x and y locations
    labs(title = "Foundation Shades from Top Brands", # add title
          subtitle = "Foundation shade availability for makeup brands with the greatest number of product options", # add subtitle
          caption = "Visualization: Emily Wilson (@emwilson243) | Source: The Pudding") # add caption

  ggsave(here("20210330_makeup", "outputs", "makeup2.png"), width = 9.3, height = 11.18, unit = "in") # save to outputs folder for this week
  