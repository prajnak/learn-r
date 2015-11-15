## Regression and correlation

set.seed(995)
xvar <- 1:20 + rnorm(20, sd=3)
yvar <- 1:20/4 + rnorm(20, sd=2)
zvar <- -2*xvar + xvar*yvar/5 + 3 + rnorm(20,sd=4)

# make data frame from vars created above
dat <- data.frame(x=xvar, y=yvar, z=zvar)

## correlation coefficients
cor(dat$x, dat$z)

## Exploratory Data Analysis
## doing a course in swirl, learn r in r
library(swirl)
ppm <- pollution$pm25
hist(ppm,col="green")
rug(ppm)
low
high
hist(ppm, col="green", breaks = 100)
rug(ppm)
hist(ppm, col='green')
abline(v=median(ppm), lwd=4, col="magenta")
#abt 50% of counties still have their pollution level greater than the dept.'s standard

##
reg <- table(pollution$region)
barplot(reg, col='wheat', main="Number of Counties in Each Region")
##boxplot that uses a formula to divide the data based on region and since the pollution dataset has two regions, we end up with two boxplots
boxplot(pm25 ~ region, data=pollution, col='red')

## plotting multiple histograms
par(mfrow = c(2,1), mar=c(4,4,2,1))
east <- subset(pollution, region == "east")
hist(east$pm25, col='green')
hist(subset(pollution, region=='west')$pm25, col='green')

## scatterplots using base.plot
with(pollution, plot(latitude, pm25))
abline(h=12, lwd=2, lty=2)
plot(pollution$latitude, ppm, col=pollution$region)
par(mfrow = c(1,2), mar=c(5,4,2,1))
west <- subset(pollution, region=='west')
plot(west$latitude, west$pm25, main='West')
plot(east$latitude, east$pm25, main='East')

#### Graphics devices in R
?Devices
with(faithful, plot(eruptions, waiting))
title(main='Old Faithful Geyser data')
dev.cur()
pdf(file='myplot.pdf')
dev.off()
dev.copy(png, file='geyserplot.png')
dev.off()

#### Plotting Systems in R
### Base plotting
head(cars)
with(cars, plot(speed, dist))
text(mean(cars$speed), max(cars$dist), "SWIRL rules!")
### Lattice System
head(state)
table(state$region)
xyplot(Life.Exp ~ Income | region, data=state, layout=c(4,1))
xyplot(Life.Exp ~ Income | region, data=state, layout=c(2,2))

### ggplot2
head(mpg)
dim(mpg)
table(mpg$model)
qplot(displ, hwy, data=mpg)

#### The Base Plotting System
# Base plotting system in R is made up of two packages, The graphics package which contains plotting functions for the "base" system. --> functions include plot, hist, boxplot, barplot, etc. The other one is grDevices which contains code to integrate with multiple graphics devices
head(airquality)
range(airquality$Ozone, na.rm = TRUE)
hist(airquality$Ozone)
table(airquality$Month)
boxplot(Ozone~Month, airquality)
boxplot(Ozone~Month, airquality, xlab="Month", ylab="Ozone (ppb)", col.axis="blue", col.lab="red")
title(main="Ozone and Wind in New York City")
with(airquality, plot(Wind, Ozone))
title(main="Ozone and Wind in New York City")
length(par()) # use ?par to get documentation and options for each configurable parameter
names(par()) # returns all configurable parameters for the base plotting system
par()$pin
par()$fg
par("pch") # pch -> plot character. default is open circle -- can be a single character or an integer code for one of a set of graphics symbols
par("lty") #shows current linetype
## par() is used to specify global graphics paramters that affect all plots in an R session. Use dev.of() or plot.new to reset to defaults. May also be overridden qhen specified as arguments to specific plotting functions. las -> orientation of axis labels on plot, bg -> background color, mar -> margin size and oma -> outer margin siE, mfrow and mfcol -> number of plots per row, column

## mfrow and mfcol deal with multiple plots. mfrow fills rows first and mfcol fills columns first
## title() can add annotations incluiding x, y-axis labels, title, subtitle and outer margin.
## mtext() adds arbitrary text to either inner or outer margins of the plot
## axis() adds axis labels and ticks.
## legend() adds a legend

plot(airquality$Wind, airquality$Ozone, type = "n" ) # type argument sets the plot up but doesn't plot the points yet
title(main="Wind and Ozone in NYC")
may <- subset(airquality,Month == 5 )
points(may$Wind, may$Ozone, col="blue", pch=17) #add points to plot created before
notmay <- subset(airquality, Month != 5)
points(notmay$Wind, notmay$Ozone, col="red", pch=8) # pch 8 looks like snowflakes
legend("topright",pch=c(17,8), col=c("blue", "red"), legend=c("May", "Other Months")) # add legend
abline(v=median(airquality$Wind), lty=2, lwd=2)
par(mfrow=c(1,2))
plot(airquality$Wind, airquality$Ozone, main="Ozone and Wind")
plot(airquality$Ozone, airquality$Solar.R, main="Ozone and Solar Radiation")

## more complicated with 3 plots
par(mfrow=c(1,3), mar=c(4,4,2,1), oma=c(0,0,2,0)) # margins are specified as 4-long vectors of integers. Each number tells how many lines of text to leave at each side. NUmbers assigned clockwise starting at bottom.
plot(airquality$Wind, airquality$Ozone, main="Ozone and Wind")
plot(airquality$Solar.R, airquality$Ozone, main="Ozone and Solar Radiation")
plot(airquality$Temp, airquality$Ozone, main="Ozone and Temperature")
## now add a title for the 1x3 plot. Have to use mtext() cmmands
mtext("Ozone and Weather in New York City", outer = TRUE)









