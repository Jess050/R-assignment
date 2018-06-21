# Jesse Smith, Kholofelo Sethebe, Joshua Adrian
# R-assignment June 2018
# Pom concentrations on beaches in Cape Town

# load libraries  ---------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(lubridate)

# load data ---------------------------------------------------------------

# pom_data <- read_delim("pom_data.csv", ";", 
#                        escape_double = FALSE, trim_ws = TRUE)
# 
# pom_metadata <- read_delim("pom_metadata.csv", 
#                             ";", escape_double = FALSE, trim_ws = TRUE)
# 
# pom_data1 <- read_delim("pom_data.csv", ";", 
#                         escape_double = FALSE, trim_ws = TRUE)

pom_data4 <- read_delim("pom_data4.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

# quick visualisations --------------------------------------------------------------------

unique(pom_data$site)
# 6 sites: Fish Hoek, Hout Bay West, Hout Bay East, Muizenberg, Strandfontein and Strandfontein Resort

# quick visualisation 
ggplot(pom_data4, aes(x = pom)) +
  geom_density()


# not neccessary coz only focusing on pom
# visualisation 
hist(pom_data4$pom)

# transform to long data
# pom_data4_long <- read_delim("pom_data4.csv", ";", 
#                        escape_double = FALSE, trim_ws = TRUE) %>%  
#   gather(key = "variable", value = "value", -site, -date, -id, -area)


# visualising data 

# ggplot(data = pom_data4_long, aes(x = variable, y = value, fill = site)) +
#   geom_boxplot(notch = TRUE)

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
ggplot(data = pom_data4, aes(x = site, y = pom, fill = area)) +
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian( ylim = c(0, 1.2)) +
  labs(title = "POM data",
       x = "Site",
       y = "POM content (g)") 

# total pom content
ggplot(data = pom_data4, aes(x = area, y = pom, fill = area)) +
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
# needed? dont think so.

# ggplot(data = pom_data4, aes(x = site, y = pom)) +
#   geom_point(aes(colour = area)) +
#   geom_line(aes(colour = area))


# the general trend seen in the graph above, is that POM content is higher in non-cleared areas, 
# for all sites except Strandfontein, 
# where POM content is higher in the cleared area than the non-cleared area
# general trend and null hypothesis = same same

# stats -------------------------------------------------------------------

library(pgirmess)

# test for normality 
shapiro.test(pom_data4$pom)

# W = 0.64527, p-value < 2.2e-16
# # if p < 0.05 significantly different from normal
# p > 0.05 not significantly different from normal (want?)
# == not normal 

#t-test
t.test(pom ~ area, data = pom_data4)

# Welch Two Sample t-test
# data:  pom by area
# t = 0.14332, df = 449.25, p-value = 0.8861
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.07862631  0.09099631
# sample estimates:
#   mean in group cleared mean in group non_cleared 
#           0.350979                  0.344794 
#  we do not reject (accept) the null hypothesis that there is a difference between cleared and non-cleared area.


# use anova for sites 


# Kruskal-Wallis
kruskal.test(pom ~ as.factor(area), data = pom_data4)

# Kruskal-Wallis rank sum test
# data:  pom by as.factor(area)
# Kruskal-Wallis chi-squared = 3.8332, df = 1, p-value = 0.05025

kruskal.test(pom ~ as.factor(site), data = pom_data4)

# Kruskal-Wallis rank sum test
# data:  pom by as.factor(site)
# Kruskal-Wallis chi-squared = 28.527, df = 3, p-value = 2.815e-06


# perform Tukey post-hoc test
TukeyHSD(aov(pom ~ as.factor(site), data = pom_data4))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = pom ~ as.factor(site), data = pom_data4)
# 
# $`as.factor(site)`
#                                    diff         lwr       upr     p adj
# Hout Bay East-Fish Hoek     -0.0271128571 -0.16880339 0.1145777 0.9605530
# Hout Bay West-Fish Hoek      0.0445400000 -0.11111959 0.2001996 0.8818238
# Muizenberg-Fish Hoek        -0.0005706173 -0.16682356 0.1656823 0.9999998
# Hout Bay West-Hout Bay East  0.0716528571 -0.08621478 0.2295205 0.6459611
# Muizenberg-Hout Bay East     0.0265422399 -0.14177985 0.1948643 0.9772759
# Muizenberg-Hout Bay West    -0.0451106173 -0.22534941 0.1351282 0.9171637

#look at confidence intervals 
plot(TukeyHSD(aov(pom ~ as.factor(site), data = pom_data4)))
# all site interactions cross over 0. 


TukeyHSD(aov(pom ~ as.factor(area), data = pom_data4))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = pom ~ as.factor(area), data = pom_data4)
# $`as.factor(area)`
#                         diff        lwr       upr     p adj
# non_cleared-cleared -0.006185 -0.0908165 0.0784465 0.8858719

plot(TukeyHSD(aov(pom ~ as.factor(area), data = pom_data4)))
# all site interactions cross over 0.





















