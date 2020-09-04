# This accompanies a paper on Common Shelduck abundance in Borgarfjordur in 
# western Iceland. It should be read in conjunction with scripts on 
# data_exploration and model_selection. This corresponds to Figure A1 in the
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


#Best Model based on model_selection.R

best = gam(count ~ s(week) +
             subsite * tide +
             offset(area),
           family = ziP,
           data = shelducks)

summary(best)


# fitted values and residuals
shelducks$fitted = predict(best, type = "response")
shelducks$devres = resid(best, type = "deviance")


# Validation plots ----

# residual plots

png("FigA1.png", height = 6, width = 6, units="in", res=300)
par (mfrow=c(2,2), mar=c(4,4,1,1), xpd=T) 
plot(shelducks$fitted, shelducks$devres,
     xlab = "fitted values",
     ylab="deviance residuals")
text(-170, 30, "a)", cex=1.2)
plot(shelducks$week, shelducks$devres,
     xlab = "week",
     ylab="deviance residuals")
text(2, 30, "b)", cex=1.2)
plot(shelducks$tide, shelducks$devres,
     xlab = "tidal state",
     ylab="deviance residuals")
text(0, 30, "c)", cex=1.2)
plot(shelducks$subsite, shelducks$devres,
     xlab = "subarea",
     ylab="deviance residuals")
text(-1, 30, "d)", cex=1.2)
dev.off()


# observed vs fitted

# function to plot observed and fitted on same graph
plot_model_fit <- function(tide_level, subsite_level){
  
  data_to_plot = filter(shelducks, tide==tide_level, subsite==subsite_level)
  with(data_to_plot, plot(week, count))
  with(data_to_plot, lines(week, fitted, col="red"))
}


png("FigA2.png", height = 6, width = 6, units="in", res=300)
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
dev.off()

