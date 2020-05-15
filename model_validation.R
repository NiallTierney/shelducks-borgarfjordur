#intro...

library(dplyr)
library(mgcv)#for gam

#Import data ----
shelducks = read.csv("data/shelduck_counts.csv") %>%
  as_tibble()

#changing the subsite column in shelducks to be a factor, not an integer 
shelducks$subsite=as.factor(shelducks$subsite)


#Best Model based on model_selection.R

best = gam(count ~ s(week) +
           s(week, by = as.numeric(subsite %in% c(2, 3))) +
           subsite * tide +
           offset(area),
         family = ziP,
         data = shelducks)

summary(best)


# fitted values for each
shelducks$fitted = predict(best, type = "response")

## Plot the observed points and fitted line for each subsite and tide 
# variable by week

# function to plot observed and fitted on same graph
plot_model_fit <- function(tide_level, subsite_level){
  
  data_to_plot = filter(shelducks, tide==tide_level, subsite==subsite_level)
  with(data_to_plot, plot(week, count))
  with(data_to_plot, lines(week, fitted, col="red"))
}

par (mfrow=c(6,2), mar=c(2,2,1,1))
plot_model_fit(tide_level="low", subsite_level = 1)
plot_model_fit(tide_level="rising", subsite_level = 1)
plot_model_fit(tide_level="low", subsite_level = 2)
plot_model_fit(tide_level="rising", subsite_level = 2)
plot_model_fit(tide_level="low", subsite_level = 3)
plot_model_fit(tide_level="rising", subsite_level = 3)
plot_model_fit(tide_level="low", subsite_level = 4)
plot_model_fit(tide_level="rising", subsite_level = 4)
plot_model_fit(tide_level="low", subsite_level = 5)
plot_model_fit(tide_level="rising", subsite_level = 5)
plot_model_fit(tide_level="low", subsite_level = 6)
plot_model_fit(tide_level="rising", subsite_level = 6)


# Validation plots
devres = resid(best, type = "deviance")
fitted = predict(best, type = "response")
par (mfrow=c(2,2)) 
plot(fitted, devres)
plot(shelducks$week, devres)
plot(shelducks$tide, devres)
plot(shelducks$subsite, devres)

