# Hons R-Assignment 2018
# Kholofelo Sethebe, Joshua Adrian and Jesse Smith 
# statistical analyses using POM data 

# load libraries  ---------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(lubridate)
library(pgirmess)

# load  data  -------------------------------------------------------------


pom_data <- read.csv("updated.csv", sep = ";")

# pom_data <- read_delim("pom_data_updated.csv", 
#                         ";", escape_double = FALSE, trim_ws = TRUE)


# analysing data --------------------------------------------------------

# quick visualisation 
ggplot(pom_data, aes(x = pom)) +
  geom_density(aes(group = site, colour = site, fill = site),alpha = 0.3)

# test for normality
# shapiro.test(pom_data$pom)
# W = 0.64458, p-value < 2.2e-16
# if p < 0.05 significantly different from normal
# p > 0.05 not significantly different from normal (want?)
# == not normal 


# formulating hypotheses and objectives  ----------------------------------------------

# hypotheses:
# HO: POM content of the soil is lower on cleared beaches than on non-cleared beaches. 
# H1: POM content of the soil is higher on cleared beaches than on non-cleared beaches.
# or 
# H0: there is no significant difference in POM content between cleared and non-cleared sites. 
# H1: there is a significant difference in POM content betweeen cleared and uncleared areas? 

# objectives:
# 1. does POM differ between sites? 
# 2. is POM content higher in uncleared areas than in cleared areas?
# 3. is POM content different over time?

# summary -----------------------------------------------------------------

sum_pom <-  pom_data %>% 
  group_by(site) %>% 
  summarise(mn_pom = mean(pom), 
            med_pom = median(pom),
            min_pom = min(pom),
            max_pom = max(pom), 
            sd_pom = sd(pom))

# site  -------------------------------------------------------------------

# test for normality and homoscedasticity
pom_data %>% 
  group_by(site) %>% 
  summarise(pom_data_dist = as.numeric(shapiro.test(pom)[2]), 
            pom_data_var = var(pom))

# # A tibble: 5 x 3
#     site                 pom_data_dist pom_data_var
# <fct>                        <dbl>        <dbl>
#   1 Fish Hoek     0.000000000000000354       0.217 
# 2 Hout Bay East 0.000000000000000120       0.137 
# 3 Hout Bay West 0.000000000000799          0.147 
# 4 Muizenberg    0.00000000000000501        0.452 
# 5 Strandfontein 0.0000320                  0.0707
# not normal 


# pom content for each site, both cleared and non-cleared
ggplot(data = pom_data, aes(x = site, y = pom, fill = area)) +
  geom_boxplot( notch = TRUE)+
  geom_jitter(alpha = 0.3, color = "black")+
  labs(title = "POM content per site",
       x = "Site",
       y = "POM (g)") +
  theme_minimal()  

# Kruskal-Wallis test
kruskal.test(pom ~ as.factor(site), data = pom_data)

# Kruskal-Wallis rank sum test
# data:  pom by as.factor(site)
# Kruskal-Wallis chi-squared = 34.619, df = 4, p-value = 5.561e-07

# perform Kruskalmc post-hoc test
kruskalmc(pom ~ as.factor(site), data = pom_data)

# Multiple comparison test after Kruskal-Wallis 
# p.value: 0.05 
# Comparisons
# obs.dif critical.dif difference
# Fish Hoek-Hout Bay East      31.39452     45.85033      FALSE
# Fish Hoek-Hout Bay West      70.92667     50.37064       TRUE
# Fish Hoek-Muizenberg         31.22395     53.79859      FALSE
# Fish Hoek-Strandfontein      91.32333    127.42875      FALSE
# Hout Bay East-Hout Bay West  39.53214     51.08515      FALSE
# Hout Bay East-Muizenberg     62.61847     54.46816       TRUE
# Hout Bay East-Strandfontein 122.71786    127.71288      FALSE
# Hout Bay West-Muizenberg    102.15062     58.32434       TRUE
# Hout Bay West-Strandfontein 162.25000    129.40451       TRUE
# Muizenberg-Strandfontein     60.09938    130.77697      FALSE

# true is where the differences lie 



# area --------------------------------------------------------------------

# test for normality and homoscedasticity
pom_data %>% 
  group_by(area) %>% 
  summarise(pom_data_dist = as.numeric(shapiro.test(pom)[2]), 
            pom_data_var = var(pom))

# # A tibble: 2 x 3
# area                      pom_data_dist pom_data_var
# <fct>                             <dbl>        <dbl>
# 1 cleared     0.00000000000000000000538          0.176
# 2 non_cleared 0.0000000000000000000000531        0.256

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
  geom_boxplot( notch = TRUE)+
  geom_jitter(alpha = 0.3, color = "black") +
  labs(title = "Total POM content",
       x = "Area",
       y = "POM (g)") +
  theme_minimal()  

# wilcox test
compare_means(pom ~ area, data = pom_data, method = "wilcox")

# # A tibble: 1 x 8
# .y.   group1  group2           p  p.adj p.format p.signif method  
# <chr> <chr>   <chr>        <dbl>  <dbl> <chr>    <chr>    <chr>   
#   1 pom   cleared non_cleared 0.0389 0.0389 0.039    *        Wilcoxon

#means between area is significantly different 

#  we do not reject (accept) the null hypothesis that there is a difference between cleared and non-cleared area.


# time  -------------------------------------------------------------------

# test for normality and homoscedasticity
pom_data %>% 
  group_by(month) %>% 
  summarise(pom_data_dist = as.numeric(shapiro.test(pom)[2]), 
            pom_data_var = var(pom))

# # A tibble: 3 x 3
# month               pom_data_dist pom_data_var
# <fct>                       <dbl>        <dbl>
#   1 Apr    0.00000000000000000000150         0.219
# 2 "Feb " 0.0000797                         0.164
# 3 "Mar " 0.000000000000000000000215        0.216

# removing the site Strandfontein due to it only being sampled once 
time <- pom_data[-c(21:30), ]

# pom content per month
ggplot(data = time, aes(x = month, y = pom, fill = area)) +
  geom_boxplot( notch = TRUE) +
  geom_jitter(alpha = 0.3, color = "black") +
  labs(title = "Monthly POM content",
       x = "Site",
       y = "POM (g)") +
  theme_minimal()  


# Kruskal-Wallis
kruskal.test(pom ~ as.factor(month), data = time)

# Kruskal-Wallis rank sum test
# data:  pom by as.factor(month)
# Kruskal-Wallis chi-squared = 17.931, df = 2, p-value = 0.0001278

# perform Kruskalmc post-hoc test
kruskalmc(pom ~ as.factor(month), data = time)

# Multiple comparison test after Kruskal-Wallis 
# p.value: 0.05 
# Comparisons
# obs.dif critical.dif difference
# Apr-Feb   22.29425     64.05939      FALSE
# Apr-Mar   55.01370     31.11295       TRUE
# Feb -Mar  32.71944     64.70571      FALSE

# sig diff between apr - mar

# transect lengths  -------------------------------------------------------

meta <- read.csv2("metadata.csv") 

meta_long <- meta %>% 
  gather(key = "variable", value = "value", -site, -date, -month, -beach_cast_cleared, - beach_cast_noncleared)

# test fot normality

# meta_long %>% 
#   group_by(site) %>% 
#   summarise(meta_long = as.numeric(shapiro.test(value)[2]))
#??? Error in summarise_impl(.data, dots) : 
#   Evaluation error: sample size must be between 3 and 5000.

shapiro.test(meta_long$value)
# Shapiro-Wilk normality test
# data:  meta_long$value
# W = 0.97173, p-value = 0.03587

#test for homoscedasticity 
meta_long %>% 
  #group_by(site) %>% 
  summarise(meta_long_var = var(value))

# meta_long_var
# 1      18.90252

# # A tibble: 5 x 2
# site                meta_long_var
# <fct>                       <dbl>
#   1 Fish Hoek                    15.0
# 2 Hout Bay East                11.8
# 3 "Hout Bay West "             14.1
# 4 Muizenberg                   29.6
# 5 Strandfontein beach          18.0           


# total transect length
ggplot(data = meta_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot( notch = TRUE)+
  geom_jitter(alpha = 0.3, color = "black") +
  labs(title = "Total transect lengths",
       x = "area",
       y = "transect length (m)") +
  theme_minimal()  

# total transect length per site
ggplot(data = meta_long, aes(x = site, y = value, fill = variable)) +
  geom_boxplot( notch = TRUE)+ 
  geom_jitter(alpha = 0.3, color = "black")+
  labs(title = "transect lengths per site",
       x = "area",
       y = "transect length (m)") +
  theme_minimal() 

# wilcox test
compare_means(value ~ variable, data = meta_long, method = "wilcox")

# A tibble: 1 x 8
# .y.   group1           group2                   p p.adj p.format p.signif method  
# <chr> <chr>            <chr>                <dbl> <dbl> <chr>    <chr>    <chr>   
#   1 value cleared_transect non_cleared_transect 0.106 0.106 0.11     ns       Wilcoxon

# means not sig diff

## Cleared transects

# quick visualisation 
ggplot(meta, aes(x = cleared_transect)) +
  geom_density(aes(group = site, colour = site, fill = site),alpha = 0.3)

# test for normality and homoscedasticity - site
meta %>% 
  group_by(site) %>% 
  summarise(len_data_dist = as.numeric(shapiro.test(cleared_transect)[2]), 
            len_data_var = var(cleared_transect))

# test for normality and homoscedasticity - month
meta %>% 
  group_by(month) %>% 
  summarise(meta_dist = as.numeric(shapiro.test(cleared_transect)[2]), 
            meta_var = var(cleared_transect))

#  A tibble: 3 x 3
# month meta_dist meta_var
# <fct>     <dbl>    <dbl>
# 1 Apr     0.0713     16.4 
# 2 Feb     0.463       4.33
# 3 Mar     0.00812    18.8 

sum_cleared_trans <-  meta %>% 
  group_by(site) %>% 
  summarise(mn_cleared = mean(cleared_transect), 
            med_cleared = median(cleared_transect),
            min_cleared = min(cleared_transect),
            max_cleared = max(cleared_transect), 
            sd_cleared = sd(cleared_transect))


# transect lengths for each site, cleared 
ggplot(data = meta, aes(x = site, y = cleared_transect)) +
  geom_boxplot(aes(colour = site), notch = TRUE)+ 
  geom_jitter(alpha = 0.3, color = "black")+
  geom_point(data = sum_cleared_trans, size = 6, shape = 18,
             aes(y = mn_cleared), colour = "goldenrod") +
  labs(title = "cleared",
       x = "Site",
       y = "transect lengths (m)") +
  theme_minimal()  

# Kruskal-Wallis
kruskal.test(cleared_transect ~ as.factor(site), data = meta)

# Kruskal-Wallis rank sum test
# data:  cleared_transect by as.factor(site)
# Kruskal-Wallis chi-squared = 6.5546, df = 4, p-value = 0.1614

# perform Kruskalmc post-hoc test
kruskalmc(pom ~ as.factor(site), data = meta)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = cleared_transect ~ as.factor(site), data = meta)
# $`as.factor(site)`
# diff         lwr      upr     p adj
# Hout Bay East-Fish Hoek             1.90952381  -2.1563516 5.975399 0.6700437
# Hout Bay West -Fish Hoek            1.96666667  -2.5000570 6.433390 0.7203974
 # Muizenberg-Fish Hoek                5.14166667   0.3516392 9.931694 0.0299052
# Strandfontein beach-Fish Hoek      -2.73333333 -14.0333497 8.566683 0.9578892
# Hout Bay West -Hout Bay East        0.05714286  -4.4729417 4.587227 0.9999996
# Muizenberg-Hout Bay East            3.23214286  -1.6170230 8.081309 0.3340943
# Strandfontein beach-Hout Bay East  -4.64285714 -15.9680687 6.682354 0.7697352
# Muizenberg-Hout Bay West            3.17500000  -2.0148639 8.364864 0.4201891
# Strandfontein beach-Hout Bay West  -4.70000000 -16.1752208 6.775221 0.7703307
# Strandfontein beach-Muizenberg     -7.87500000 -19.4798885 3.729888 0.3165357


## non_cleared transects

# quick visualisation 
ggplot(meta, aes(x = non_cleared_transect)) +
  geom_density(aes(group = site, colour = site, fill = site),alpha = 0.3)


# test for normality and homoscedasticity - month 
meta %>% 
  group_by(month) %>% 
  summarise(meta_dist = as.numeric(shapiro.test(non_cleared_transect)[2]), 
            meta_var = var(non_cleared_transect))

# A tibble: 3 x 3
# month meta_dist meta_var
# <fct>     <dbl>    <dbl>
# 1 Apr       0.340    23.9 
# 2 Feb       0.637    2.33
# 3 Mar       0.312    14.9

# test for normality and homoscedasticity - site
# meta %>% 
#   group_by(site) %>% 
#   summarise(meta_dist = as.numeric(shapiro.test(non_cleared_transect)[2]), 
#             meta_var = var(non_cleared_transect))
# ERROR


sum_non_cleared_trans <-  meta %>% 
  group_by(site) %>% 
  summarise(mn_non = mean(non_cleared_transect), 
            med_non = median(non_cleared_transect),
            min_non = min(non_cleared_transect),
            max_non = max(non_cleared_transect), 
            sd_non = sd(non_cleared_transect))

shapiro.test(meta$non_cleared_transect)

# Shapiro-Wilk normality test
# data:  meta$non_cleared_transect
# W = 0.98281, p-value = 0.6987
# normal 

# transect lengths for each site, non_cleared
ggplot(data = meta, aes(x = site, y = non_cleared_transect)) +
  geom_boxplot(aes(colour = site), notch = TRUE)+
  geom_jitter(alpha = 0.3, color = "black") +
  geom_point(data = sum_cleared_trans, size = 6, shape = 18,
             aes(y = mn_cleared), colour = "goldenrod") +
  labs(title = "non_cleared",
       x = "Site",
       y = "transect lengths (m)") +
  theme_minimal()  


summary(aov(non_cleared_transect ~ as.factor(site), data = meta))
# Df Sum Sq Mean Sq F value Pr(>F)  
# as.factor(site)  4  184.3   46.09   2.537 0.0537 .
# Residuals       43  781.1   18.17                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# site maps  -------------------------------------------------------------------
library(ggmap)


sites <- read.csv("sites.csv", sep=";")

cape_point1 <-  get_map(location = c(lon = 18.6, lat = -34.2),
                        zoom = 10, maptype = 'satellite')

ggmap(cape_point1)

cp1 <- ggmap(cape_point1) +
  geom_point(data = sites, aes(x = long , y = lat ), 
             colour = "red", size =  2.5) +
  #coord_equal(xlim = c(18.2, 19.0), ylim = c(-34.5, -33.8), expand = FALSE) +
  labs(y = "Latitude(°S)", x = "Longitude(°E)", title = "Site Map") 

cp1

cp2 <- cp1 +
  geom_text(data = sites,
            aes(long , lat , label = site), 
            hjust = 0, vjust = 0.7, 
            size = 4, colour = "white") +
  annotate("text", label = "Hout Bay West", 
           x = 18.25, y = -34.11, 
           size = 4, colour = "white") +
  theme_bw()+
  coord_cartesian()

cp2
  


library(jpeg)
strandfontein <- readJPEG("/Users/JESSE_/Desktop/honours/R assignment final/R-assignment/maps/strandfontein.jpg", native = TRUE)
MUIZENBERG <- readJPEG("/Users/JESSE_/Desktop/honours/R assignment final/R-assignment/maps/MUIZENBERG.jpg", native = TRUE)
FISH_HOEK <- readJPEG("/Users/JESSE_/Desktop/honours/R assignment final/R-assignment/maps/FISH HOEK.jpg", native = TRUE)
HBE <- readJPEG("/Users/JESSE_/Desktop/honours/R assignment final/R-assignment/maps/HBE.jpg", native = TRUE)
HBW <- readJPEG("/Users/JESSE_/Desktop/honours/R assignment final/R-assignment/maps/HBW.jpg", native = TRUE)
  
  
library(ggplot2)

final <- cp2 + 
  annotation_custom(grob = ggplotGrob(strandfontein),
                    xmin = 18.8, xmax = 19,
                    ymin = -34.4, ymax = -34.6) +
  annotation_custom(grob = ggplotGrob(MUIZENBERG),
                    xmin = 18.6, xmax = 18.8,
                    ymin = -34.4, ymax = -34.6) +
  annotation_custom(grob = ggplotGrob(FISH_HOEK),
                    xmin = 18.4, xmax = 18.6,
                    ymin = -34.4, ymax = -34.6) +
  annotation_custom(grob = ggplotGrob(HBE),
                    xmin = 18.2, xmax = 18.4,
                    ymin = -34.4, ymax = -34.6) +
  annotation_custom(grob = ggplotGrob(HBW),
                    xmin = 18.2, xmax = 18.4,
                    ymin = -34.2, ymax = -34.4)
final

ggsave(final, filename = "final_sitemap.png")
