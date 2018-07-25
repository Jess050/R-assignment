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
                            ";", escape_double = FALSE, trim_ws = TRUE)

pom_data1 <- read_delim("pom_data.csv", ";", 
                        escape_double = FALSE, trim_ws = TRUE)

# visualisations --------------------------------------------------------------------

unique(pom_data$site)
# 6 sites: Fish Hoek, Hout Bay West, Hout Bay East, Muizenberg, Strandfontein and Strandfontein Resort

# quick visualisation 
ggplot(pom_data, aes(x = pom)) +
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

# # Panelled grouped histograms for the four POM variables
# ggplot(data = pom_data, aes(x = value)) +
#   geom_histogram(position = "dodge", # ommitting this creates a stacked histogram
#                  colour = NA, bins = 20,
#                  aes(fill = site)) +
#   facet_wrap(~variable) +
#   labs(title = "POM data",
#        subtitle = "Grouped frequency histogram",
#        x = "Weight (g)",
#        y = "Count") +
#   theme_pubclean()

# more visualisations ##################################################################

# formulating a hypothesis
# HO: POM content of the soil is lower on cleared beaches than on non-cleared beaches. 
# H1: POM content of the soil is higher on cleared beaches than on non-cleared beaches.
# or : there is no difference in POM content between cleared and non-cleared sites. 

# filter the data
# notched boxplots -- see day2


# pom content for each site, both cleared and non-cleared
ggplot(data = pom_data1, aes(x = site, y = pom, fill = area)) +
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian( ylim = c(0, 1.2)) +
  labs(title = "POM data",
       x = "Site",
       y = "POM content (g)") 

# total pom content
ggplot(data = pom_data1, aes(x = area, y = pom, fill = area)) +
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Total POM content",
       x = "Area",
       y = "POM content (g)")

# # cleared sites
# pom_cleared <- pom_data %>% 
#   filter(variable == "pom", area == "cleared")
# 
# ggplot(data = pom_cleared, aes(x = variable, y = value, fill = site)) +
#   geom_boxplot(notch = TRUE) +
#   labs(y = "Grams (g)", x = "POM content", title = "Cleared sites") 
# 
# # non-cleared sites 
# pom_non_cleared <- pom_data %>% 
#   filter(variable == "pom", area == "non_cleared")
# 
# ggplot(data = pom_non_cleared, aes(x = site, y = value, fill = site)) +
#   geom_boxplot(notch = TRUE) +
#   labs(y = "Grams (g)", x = "POM content", title = "Non-cleared sites")



# # line graph of change in POM 
# ggplot(data = pom_data1, aes(x = pom , y = site)) +
#   geom_point() +
#   geom_line(aes(group = area, colour = site))


# binary ---------------------------------------------------------------------

# binary graph: cleared vs non-cleared
# needed?
pom_supp <- pom_data1 %>% 
  filter(area  %in% c("cleared", "non_cleared"))

ggplot(data = pom_supp, aes(x = site, y = pom)) +
  geom_point(aes(colour = area)) +
  geom_line(aes(colour = area))


# the general trend seen in the graph above, is that POM content is higher in non-cleared areas, 
# for all sites except Strandfontein, 
# where POM content is higher in the cleared area than the non-cleared area
# general trend and null hypothesis = same same

# stats -------------------------------------------------------------------

library(pgirmess)

# test for normality 
shapiro.test(pom_data$pom)

# W = 0.64061, p-value < 2.2e-16
# normal

#t-test
t.test(pom ~ area, data = pom_data1)
# Welch Two Sample t-test
# 
# data:  pom by area
# t = 0.2213, df = 469.66, p-value = 0.825
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0727614  0.0912298
# sample estimates:
#   mean in group cleared mean in group non_cleared
# 0.3432177                 0.3339835
#  we do not reject (accept) the null hypothesis that there is a difference between cleared and non-cleared area.

# use anova for sites 


# Kruskal-Wallis
kruskal.test(pom ~ as.factor(area), data = pom_data1)
# Kruskal-Wallis rank sum test
# data:  pom by as.factor(area)
# Kruskal-Wallis chi-squared = 3.9436, df = 1, p-value = 0.04705

kruskal.test(pom ~ as.factor(site), data = pom_data1)
# Kruskal-Wallis rank sum test
# data:  pom by as.factor(site)
# Kruskal-Wallis chi-squared = 43.794, df = 5, p-value = 2.551e-08

pom_dat <-pom_data1 %>% 
  mutate(date = as.Date(date, "%d-%b-%y"))

#load package for non-parametric post-hoc test 
library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_0_21)









# perform Tukey post-hoc test
TukeyHSD(aov(pom ~ as.factor(site), data = pom_data1))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = pom ~ as.factor(site), data = pom_data1)
# 
# $`as.factor(site)`
#                                          diff        lwr       upr     p adj
# Hout Bay East-Fish Hoek            -0.0271128571 -0.1819090 0.1276833 0.9961215
# Hout Bay West-Fish Hoek             0.0445400000 -0.1255173 0.2145973 0.9755134
# Muizenberg-Fish Hoek               -0.0005706173 -0.1822011 0.1810598 1.0000000
# Strandfontein-Fish Hoek            -0.1891200000 -0.6193346 0.2410946 0.8077500
# Strandfontein resort-Fish Hoek     -0.2635200000 -0.6937346 0.1666946 0.4976703
# Hout Bay West-Hout Bay East         0.0716528571 -0.1008167 0.2441224 0.8422455
# Muizenberg-Hout Bay East            0.0265422399 -0.1573487 0.2104332 0.9984589
# Strandfontein-Hout Bay East        -0.1620071429 -0.5931810 0.2691667 0.8911811
# Strandfontein resort-Hout Bay East -0.2364071429 -0.6675810 0.1947667 0.6196290
# Muizenberg-Hout Bay West           -0.0451106173 -0.2420205 0.1517993 0.9865348
# Strandfontein-Hout Bay West        -0.2336600000 -0.6705450 0.2032250 0.6448698
# Strandfontein resort-Hout Bay West -0.3080600000 -0.7449450 0.1288250 0.3338170
# Strandfontein-Muizenberg           -0.1885493827 -0.6300680 0.2529692 0.8261022
# Strandfontein resort-Muizenberg    -0.2629493827 -0.7044680 0.1785692 0.5297795
# Strandfontein resort-Strandfontein -0.0744000000 -0.6634957 0.5146957 0.9991918


#look at confidence intervals 
plot(TukeyHSD(aov(pom ~ as.factor(site), data = pom_data1)))
# all site interactions cross over 0. 


TukeyHSD(aov(pom ~ as.factor(area), data = pom_data1))
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = pom ~ as.factor(area), data = pom_data1)
# 
# $`as.factor(area)`
# diff        lwr       upr     p adj
# non_cleared-cleared -0.009234203 -0.0910686 0.0726002 0.8246312

plot(TukeyHSD(aov(pom ~ as.factor(area), data = pom_data1)))






















