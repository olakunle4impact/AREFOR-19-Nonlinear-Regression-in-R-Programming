setwd('D:/personal/R_tutorials/Regression/data')

figure_data <- read.table('figure_data.csv', sep=",", header=T)

View(figure_data)
#Checking linearity by plotting the data

#Simple plot


plot.with.errorbars <- function(x, y, err, ylim=NULL, ...) {
  if (is.null(ylim))
    ylim <- c(min(y-err), max(y+err))
  plot(x, y, ylim=ylim, pch=19, ...)
  arrows(x, y-err, x, y+err, length=0.05, angle=90, code=3)
}

plot.with.errorbars(figure_data$radius, figure_data$area, figure_data$error)
# plot(area ~ radius, data= figure_data)
# arrows(figure_data$radius, figure_data$radius-figure_data$error, 
#        figure_data$radius, figure_data$radius+figure_data$error, 
#        length=0.05, angle=90, code=3)

abline(lm(area ~ radius, figure_data))

#Using ggplot
library(ggplot2) 

ggplot(data=figure_data, aes(x=radius, y=area))+
  geom_point()+
  geom_errorbar(aes(x=radius, ymin=area-error, ymax=area+error, width=0.25))+
  theme_classic()+
  theme(panel.border = element_rect(colour="black", size=2, fill = NA))+
  geom_smooth(method="lm", se=F)

#The linear trendline added is not passing through all the error bars (not even close to most of them).
#For sure the relationship is not linear. Can we linearize it?

#Let's have a close look at different basic functions to see if there is any relationship that relates to our observations
#Check externally

#It looks like our best bet is an exponential model type.
#Let's perform some non-least square (nls) regression
m<-nls(area~a*radius^n, data=figure_data, trace=T,
       start=c(a=1, n=2))
#get some estimation of goodness of fit
cor(figure_data$area,predict(m))

#plot
#plot(figure_data$radius, figure_data$area)
P <- predict(m)
plot.with.errorbars(figure_data$radius, figure_data$area, figure_data$error)
reorder <- order(figure_data$radius)
lines(figure_data$radius[reorder], P[reorder], col=2)
