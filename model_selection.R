# This accompanies a paper on Common Shelduck abundance in Borgarfjordur in 
# western Iceland. It should be read in conjunction with scripts on 
# data_exploration and model_validation. This corresponds to Table A1 in the
# paper. 

# Niall Tierney and Sunny Townsend
# 15 May 2020

library(dplyr)
library(mgcv)#for gam

#Import data ----
shelducks = read.csv("data/shelduck_counts.csv") %>%
  as_tibble()

#changing the subsite column in shelducks to be a factor, not an integer 
shelducks$tide = as.factor(shelducks$tide)
shelducks$subsite=as.factor(shelducks$subsite)

#Model selection ----

m1 = gam(count ~ s(week)+ tide * subsite + offset(area),
         family = ziP,
         data = shelducks)  

m2 = gam(count ~ s(week)+ tide + subsite + offset(area),
         family = ziP,
         data = shelducks)  

m3 = gam(count ~ s(week)+ tide,
         family = ziP,
         data = shelducks)

m4 = gam(count ~ s(week) + subsite + offset(area),
         family = ziP,
         data = shelducks)

m5 = gam(count ~ subsite + offset(area),
         family = ziP,
         data = shelducks)  

m6 = gam(count ~ s(week),
         family = ziP,
         data = shelducks)  

m7 = gam(count ~ tide,
         family = ziP,
         data = shelducks)

m8 = gam(count ~ 1,
         family = ziP,
         data = shelducks)

m9 = gam(count ~ s(week) +
            s(week, by = as.numeric(subsite %in% c(2, 3))) +
            subsite * tide +
            offset(area),
          family = ziP,
          data = shelducks)

m10 = gam(count ~ s(week) +
           s(week, by = as.numeric(subsite %in% c(2, 4))) +
           subsite * tide +
           offset(area),
         family = ziP,
         data = shelducks)

m11 = gam(count ~ s(week) +
            s(week, by = as.numeric(subsite %in% c(2, 3, 4))) +
            subsite * tide +
            offset(area),
          family = ziP,
          data = shelducks)


summary(m1)
AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)





