# This accompanies a paper on Common Shelduck abundance in Borgarfjordur in 
# western Iceland. It should be read in conjunction with scripts on 
# and model_selection and model_validation. 


# Niall Tierney and Sunny Townsend
# 15 May 2020

library(dplyr)

## import data
shelducks = read.csv("data/shelduck_counts.csv") %>%
  as_tibble()

## data exploration ----

hist(shelducks$count, breaks=100)

plot(shelducks$week, shelducks$count, xlab="Week", ylab="Number of birds")
plot(shelducks$tide, shelducks$count, xlab="Tidal state", 
ylab="Number of birds")
shelducks$subsite = as.factor(shelducks$subsite)
plot(shelducks$subsite, shelducks$count, xlab="Subsite", 
     ylab="Number of birds")

# Exploring zero-inflation and overdispersion
par(mfrow = c(1, 2))
# counts with zeros
hist(shelducks$count, breaks = 1000, main = "Histogram of counts", 
     xlab = "Observed count")
# counts without zeros
pos_counts = filter(shelducks, count > 0)
hist(pos_counts$count, breaks = 1000, main = "Histogram of counts > 0", 
     xlab = "Observed non-zero count")




