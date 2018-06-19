# R-assignment 2018
# Pom concentrations on beaches in Cape Town



# load libraries  ---------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(lubridate)


# load data ---------------------------------------------------------------

pom_data <- read_delim("pom_data.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE)

pom_metadata <- read_delim("pom_metadata.csv", 
                           +     ";", escape_double = FALSE, trim_ws = TRUE)



# visualisations --------------------------------------------------------------------

unique(pom_data$site)
# 6 sites: Fish Hoek, Hout Bay West, Hout Bay East, Muizenberg, Strandfontein and Strandfontein Resort

# quick visualisation 
ggplot(pom_data1, aes(x = pom)) +
  geom_density()

# visualisation 
hist(pom_data$pom)

# transform to long data
pom_data <- read_delim("pom_data.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE) %>%  
  gather(key = "variable", value = "value", -site, -date, -id, -area)


# visualising data 

ggplot(data = pom_data, aes(x = variable, y = value, fill = site)) +
  geom_boxplot(notch = TRUE)

# Panelled grouped histograms for the four POM variables
ggplot(data = pom_data, aes(x = value)) +
  geom_histogram(position = "dodge", # ommitting this creates a stacked histogram
                 colour = NA, bins = 20,
                 aes(fill = site)) +
  facet_wrap(~variable) +
  labs(title = "POM data",
       subtitle = "Grouped frequency histogram",
       x = "Weight (g)",
       y = "Count") +
  theme_pubclean()

##################################################################

# formulating a hypothesis
# HO: POM content of the soil is lower on cleared beaches than on non-cleared beaches. 
# H1: POM content of the soil is higher on cleared beaches than on non-cleared beaches.
# or : there is no difference in POM content between cleared and non-cleared sites. 

# filter the data
# notched boxplots -- see day2

# pom_data_sub <- pom_data %>% 
#  filter(variable == "pom")

# ggplot(data = pom_data_sub, aes(x = variable, y = value, fill = site)) +
#  geom_boxplot() +
# labs(y = "Grams (g)", x = "POM content", title = "POM") 

# cleared sites
pom_cleared <- pom_data %>% 
  filter(variable == "pom", area == "cleared")

ggplot(data = pom_cleared, aes(x = variable, y = value, fill = site)) +
  geom_boxplot(notch = TRUE) +
  labs(y = "Grams (g)", x = "POM content", title = "Cleared sites") 

# non-cleared sites 
pom_non_cleared <- pom_data %>% 
  filter(variable == "pom", area == "non_cleared")

ggplot(data = pom_non_cleared, aes(x = variable, y = value, fill = site)) +
  geom_boxplot(notch = TRUE) +
  labs(y = "Grams (g)", x = "POM content", title = "Non-cleared sites")


pom_data1 <- read_delim("pom_data.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE)

# line graph of change in POM 
ggplot(data = pom_data1, aes(x = pom , y = site)) +
  geom_point() +
  geom_line(aes(group = area, colour = site))


# binary ---------------------------------------------------------------------

# binary graph: cleared vs non-cleared
pom_supp <- pom_data1 %>% 
  filter(area  %in% c("cleared", "non_cleared"))

ggplot(data = pom_supp, aes(x = pom, y = site)) +
  geom_point(aes(colour = area)) +
  geom_line(aes(colour = area))

# the general trend seen in the graph above, is that POM content is higher in non-cleared areas, 
# for all sites except Strandfontein, 
# where POM content is higher in the cleared area than the non-cleared area
# general trend and null hypothesis = same same

# stats -------------------------------------------------------------------

# test for normality 
shapiro.test(pom_data$pom)

# W = 0.64061, p-value < 2.2e-16
# normal



