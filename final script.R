# Hons R-Assignment 2018
# Kholofelo Sethebe, Joshua Adrian and Jesse Smith 
# statistical analyses using POM data 

# load libraries  ---------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(lubridate)


# load  data  -------------------------------------------------------------


# pom_data <- read.csv2("pom_data_updated.csv", header = TRUE)
# not working (excel format problematic)

pom_data <- read_delim("pom_data_updated.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)


# analysing data --------------------------------------------------------

# quick visualisation 
ggplot(pom_data, aes(x = pom)) +
  geom_density()

# test for normality 
shapiro.test(pom_data$pom)
# W = 0.64458, p-value < 2.2e-16
# if p < 0.05 significantly different from normal
# p > 0.05 not significantly different from normal (want?)
# == not normal 


# formulating hypotheses and objectives  ----------------------------------------------

# hypotheses:
# HO: POM content of the soil is lower on cleared beaches than on non-cleared beaches. 
# H1: POM content of the soil is higher on cleared beaches than on non-cleared beaches.
# or 
# there is no significant difference in POM content between cleared and non-cleared sites. 
# there is a significant difference in POM content betweeen cleared and uncleared areas? 

# objectives:
# 1. does POM differ between sites? 
# 2. is POM content higher in uncleared areas than in cleared areas?
# 3. is POM content different over time?

# site  -------------------------------------------------------------------

# pom content for each site, both cleared and non-cleared
ggplot(data = pom_data, aes(x = site, y = pom, fill = area)) +
  geom_boxplot(outlier.shape = NA, notch = TRUE)+
  coord_cartesian( ylim = c(0, 1.2)) +
  labs(title = "POM content per site",
       x = "Site",
       y = "POM (g)")

# Kruskal-Wallis test
kruskal.test(pom ~ as.factor(site), data = pom_data)

# Kruskal-Wallis rank sum test
# 
# data:  pom by as.factor(site)
# Kruskal-Wallis chi-squared = 34.619, df = 4, p-value = 5.561e-07

# perform Tukey post-hoc test
TukeyHSD(aov(pom ~ as.factor(site), data = pom_data))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = pom ~ as.factor(site), data = pom_data)
# 
# $`as.factor(site)`
# diff        lwr       upr     p adj
# Hout Bay East-Fish Hoek     -0.0271128571 -0.1766278 0.1224021 0.9876703
# Hout Bay West-Fish Hoek      0.0445400000 -0.1197154 0.2087954 0.9463568
# Muizenberg-Fish Hoek        -0.0005706173 -0.1760043 0.1748631 1.0000000
# Strandfontein-Fish Hoek     -0.1891200000 -0.6046569 0.2264169 0.7241590
# Hout Bay West-Hout Bay East  0.0716528571 -0.0949325 0.2382382 0.7641247
# Muizenberg-Hout Bay East     0.0265422399 -0.1510749 0.2041594 0.9941018
# Strandfontein-Hout Bay East -0.1620071429 -0.5784705 0.2544562 0.8242792
# Muizenberg-Hout Bay West    -0.0451106173 -0.2353025 0.1450813 0.9667011
# Strandfontein-Hout Bay West -0.2336600000 -0.6556397 0.1883197 0.5523867
# Strandfontein-Muizenberg    -0.1885493827 -0.6150046 0.2379058 0.7452096


# incomplete ...
# use anova for sites
anova1 <- aov(pom~site,data= pom_data)

summary(anova1)
# Df Sum Sq Mean Sq F value Pr(>F)
# site          4   0.66  0.1639   0.759  0.552
# Residuals   476 102.77  0.2159   

# Terms:
#   site Residuals
# Sum of Squares    0.65558 102.76541
# Deg. of Freedom         4       476
# 
# Residual standard error: 0.4646437
# Estimated effects may be unbalanced

# area --------------------------------------------------------------------

#trial and error 

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

# total pom content
ggplot(data = pom_data, aes(x = area, y = pom, fill = area)) +
  geom_boxplot(outlier.shape = NA, notch = TRUE)+
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Total POM content",
       x = "Area",
       y = "POM (g)")

#t-test
t.test(pom ~ area, data = pom_data)

# Welch Two Sample t-test
# 
# data:  pom by area
# t = 0.23637, df = 459.6, p-value = 0.8132
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.07339485  0.09346519
# sample estimates:
#   mean in group cleared mean in group non_cleared 
# 0.3489259                 0.3388908 
#  we do not reject (accept) the null hypothesis that there is a difference between cleared and non-cleared area.


# Kruskal-Wallis
kruskal.test(pom ~ as.factor(area), data = pom_data)

# Kruskal-Wallis rank sum test
# 
# data:  pom by as.factor(area)
# Kruskal-Wallis chi-squared = 4.2674, df = 1, p-value = 0.03885

# perform Tukey post-hoc test
TukeyHSD(aov(pom ~ as.factor(month), data = pom_data))

# Fit: aov(formula = pom ~ as.factor(month), data = pom_data)
# 
# $`as.factor(month)`
#               diff        lwr        upr     p adj
# Feb-Apr -0.01954468 -0.2335829 0.19449350 0.9749139
# Mar-Apr -0.08948483 -0.1921009 0.01313125 0.1015684
# Mar-Feb -0.06994014 -0.2854967 0.14561646 0.7259956

# time  -------------------------------------------------------------------

# removing the site Strandfontein due to it only being sampled once 
time <- pom_data[-c(21:30), ]

# pom content per month
ggplot(data = time, aes(x = month, y = pom, fill = area)) +
  geom_boxplot(outlier.shape = NA, notch = TRUE)+
  coord_cartesian( ylim = c(0, 1.3)) +
  labs(title = "Monthly POM content",
       x = "Site",
       y = "POM (g)")
  
# Kruskal-Wallis
kruskal.test(pom ~ as.factor(month), data = time)

# Kruskal-Wallis rank sum test
# 
# data:  pom by as.factor(month)
# Kruskal-Wallis chi-squared = 17.931, df = 2, p-value = 0.0001278

# perform Tukey post-hoc test
TukeyHSD(aov(pom ~ as.factor(month), data = time))

#   Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = pom ~ as.factor(month), data = time)
# 
# $`as.factor(month)`
#              diff        lwr        upr     p adj
# Feb-Apr -0.01954468 -0.2349895 0.19590016 0.9752330
# Mar-Apr -0.08267323 -0.1873124 0.02196598 0.1523290
# Mar-Feb -0.06312854 -0.2807471 0.15449000 0.7740766


