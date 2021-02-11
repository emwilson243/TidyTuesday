### Tidy Tuesday - Wealth Inequality ###
### Created by: Emily Wilson ###########
### Updated on: 2021-02-10 #############

### Load libraries #####################

library(tidyverse)
library(tidytuesdayR)
library(here)
library(calecopal)
library(cowplot)

### Load data ##########################

student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv') # read in and assign data using Tidy Tuesday package
view(student_debt) #look at the loan debt data

### Start graphing! ####################

pal <- cal_palette("tidepool")[c(1, 3, 6)] #load color palette I want to use

loan_debt <- ggplot(student_debt, #tell ggplot what data I want to use
       aes(x = year, 
       y = loan_debt, # this plot is for average family student loan debt
       fill = race)) + #I want colors filled by race
  geom_area(alpha = 0.75, # make the shading slightly less opaque
            size=0.5, #alter line size
            color = "black") + #set line color to black
  theme_classic() + # set classic theme
  theme(panel.grid = element_blank(),  # I hate those panel grids so much
        legend.position = c(0.2, 0.8)) + # change legend position
  scale_y_continuous(limits = c(0, 35000), expand = c(0,0)) + #get rid of that weird extra white space between the graph and the axes
  scale_x_continuous(limits = c(1989, 2016), expand = c(0,0)) + #pt. 2
  scale_fill_manual(values = pal) + #set colors to the palette I loaded earlier
  labs(title = "Average Family Student Loan Debt (ages 25-55)", # add title 
       x = "Year", # capitalize x axis label
       y = "Student Loan Debt (USD)", # capitalize y axis label
       fill = "Race") #capitalize legend title
  
       
percent_debt <- ggplot(student_debt, #tell ggplot what data I want to use
       aes(x = year, 
           y = loan_debt_pct, # this plot is for the percentage of families who have student loan debt
           group = race,
           color = race)) +
  geom_line() +
  theme_classic() + # set classic theme
  theme(panel.grid = element_blank(), # I hate those panel grids so much
        legend.position = c(0.2, 0.8)) + # change legend position
  scale_y_continuous(limits = c(0, 0.5), expand = c(0,0)) + #get rid of that weird extra white space between the graph and the axes
  scale_x_continuous(limits = c(1989, 2016), expand = c(0,0)) + #pt. 2
  scale_color_manual(values = pal) + #set colors to the palette I loaded earlier
  labs(title = "Prevalence of Families with Student Loan Debt", # add title 
       x = "Year", # capitalize x axis label
       y = "Percent of Families", # capitalize y axis label
       color = "Race")

plot <- plot_grid(loan_debt, percent_debt, scale = 0.9) # Make a plot with the average student loan and percentage of families with loan debt side-by-side

title <- ggdraw() + # apparently I can use this to add an overall title
  draw_label("The Rising Tide of Student Loan Debt (1989 - 2016)", #set overarching title for both graphs
             fontface = "bold", # bold the font to make the title stand out more
             size = 20, #set large title size to stand out
             x = 0, # seems to set the text to the left edge
             hjust = -0.03) #seems like this decides exactly how far away the text is from the left edge


caption <- ggdraw() + # going to try to use the same approach from the title for a combined caption for both graphs
  draw_label("Source: The Urban Institute | Visualization by Emily Wilson", # set credits
             # appears that not setting the x and hjust like above will just center it
             size = 9) #set smaller size for caption


plot_grid(title, plot, caption, ncol = 1, rel_heights = c(0.1, 1))

ggsave(here("20210210_wealthinequality", "outputs", "studentloanplot.png"), width = 12, height = 7)











