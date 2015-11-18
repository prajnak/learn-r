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

#### Lattice PLotting system
## the 'lattice' package is an implementation of the Trellis graphics system for R. It focuses on multivariate data. Its implemented using two packages. first -> lattice -- contains code for producing trellis graphics and includes functions such as xyplot, bowplot(box and whiskers) and levelplot.

## the second package is 'grid' -> contains low-level functions used by the 'lattice' package. end users rarely need to call functions in the grid package. All plotting in lattice is done in a single function call unlike 'base' plotting which uses multiple calls
## xyplot --> scatterplot, bwplot -> box and whiskers or boxplot, histograms for histograms. others not covered in this lesson include stripplot, dotplot, splom, levelplot

## Lattice functions take a formula for their first argument of the form y ~ x -> indicates that y depends on x, so in a scatterplot, y goes on y-axis and x goes on x-axis. Example call: xyplot(y ~x | f * g, data). f, g are conditioning ariables that help lattice plot multivariate data
head(airquality)
xyplot(Ozone~Wind, data=airquality)
## lattice shares some graphical params such as pch, col with the base package.
xyplot(Ozone~Wind, data=airquality, col="red", pch=8, main="Big Apple Data")

## Generating multi panel plots using lattice
xyplot(Ozone~Wind | as.factor(Month), data=airquality, layout=c(5,1))
xyplot(Ozone~Wind | Month, data=airquality, layout=c(5,1)) # using as.factor tells lattice to use the value of the Month field to label each subplot

# Calls to the lattice package don't plot data directly to the graphics device. they return an object of class trellis. The lattice print methods do the actual plotting to device portion. they return plot objects that can be stored(better tos ave code and dta rather than plot objects). On cmd line trellis objects are auto-printed so it appears the call to a lattice function is plotting the data
p <- xyplot(Ozone~Wind, data=airquality)
print(p)
names(p) #prints the named properties of the trellis object like 'formula', 'legend', etc. theres about 45 of them. a lot of them maybe NULL in value
mynames[myfull] #show non-NULL properties of p, about 29 values.
p[["formula"]]
p[["x.limits"]]

## lattice has panel functions that control what happens inside each panel of a plot.panel funs receive x,y coords of data points in their panel along with optional arguments.
table(f)
xyplot(y~x|f, layout=c(2,1))
head(v1)

str(diamonds)
table(diamonds$color)
table(diamonds$color, diamonds$cut)
xyplot(price~carat|color*cut, data=diamonds, pch=20, xlab=myxlab, ylab=myylab,
       main=mymain)

#### Working with colors
library(jpeg)
library(RColorBrewer)
library(datasets)
## this lesson supplements the lessons on plotting with base and lattice. anything that atkes the argument 'col'
## two color palettes available in grDevices package. heat.colors(red->yellow->white) and topo.colors(blue -> brown)

##colors() shows available colors
sample(colors(), 10)

##two more functions in grDevices --> colorRamp and colorRampPalette give more options
##they take color names as arguments and use them as "palettes", that is, these argument colors are blended in different proportions to form new colors

## colorRamp -> takes a palette of colors and returns a functions that takes values between 0 and 1 as arguments. 0, 1 are extremes, values in between are blends of extremes
pal <- colorRamp(c("red", "blue"))
pal(0.5) # returns a 1by3 array of 8 bits ea
pal(seq(0,1,len=6))

p1 <- colorRampPalette(c("red", "blue")) # colorramppalette returns a vector of hex values.argument passed in is number of colors to return
p1(2)
p1(6)
0xcc()
p2  <- colorRampPalette(c("red", "yellow"))
p2(2)
p2(10)
showMe(p1(20))
showMe(p2(20))
showMe(p2(2))
?rgb
p3 <- colorRampPalette(c('red', 'blue'), alpha=0.5)
p3(5)

plot(x,y,pch=19, col=rgb(0,.5,.5, .3))

## RColorBrewer package --> 3 types of color palettes -> sequential, divergent and qualitative. 

cols <- brewer.pal(3, "BuGn")
showMe(cols)
pal <- colorRampPalette(cols)
showMe(pal(20))

## use colors generated from pal(20) to display topographic information in Auckland's Maunga Whau volcano.
image(volcano, col=pal(20))
image(volcano, col=p1(20))

#### GGPlot2 Part 1
## combines the best of lattice and base plotting systems. allows multipanel plots, and post facto annotation calls for adding texts, labels, titles etc. Uses the low-level 'grid' package to draw graphics. ggplot2 plots are composed of aesthetics (attribs such as size, shape and color) and geoms(geometric objects) such as points, lines and bars

## 2 major plotting functions: qplot -> quick plot works like base package plot. looks for data in a data frame or parent environment. the other one is ggplot2 which offers more flexibility and customization
str(mpg)
qplot(displ, hwy, data=mpg)
qplot(displ, hwy, data=mpg, color=drv) ## color by wheel drive type. front wheel drive has higest mileage and rear wheel lowest as they also tend to have heavier engine displacements 
qplot(displ, hwy, data=mpg, color=drv, geom=c("point", "smooth")) ## add 95% confidence interval gray areas surrounding each trend line
qplot(y=hwy, data=mpg, color=drv)
myhigh

##box whisker plots
qplot(drv, hwy, data=mpg, geom="boxplot") #boxplot of mileage value (hwy) for each kind of wheel drive
qplot(drv, hwy, data=mpg, geom="boxplot", color=manufacturer) #boxplots of hwy with a boxplot for each manufacturer

##histograms
qplot(hwy, data=mpg, fill=drv)
## use facets/panels instead of colors to distinguish between drive factors. the facets argument is shorthand for number of rows(to the left of the ~) and num of columns(to the right of the ~). The . indicates a single row and drv implies 3 since there are 3 distinct drive factors
qplot(displ, hwy, data=mpg, facets=. ~ drv)

qplot(hwy, data=mpg, facets= drv ~ ., binwidth=2) #num rows = drv, num cols = 1 for the facet grid

#### GGPlot2 part2 - using ggplot instead of qplot
## Basic Components of a ggplot plot:
# DATA FRAME
# AESTHETIC MAPPINGS
# GEOMS
# FACETS
# STATS -> statistical transformations applied by ggplot2 to the data. quantiles, smoothing, binning
# SCALES
# COORDINATE SYSTEM
qplot(displ, hwy, data=mpg, geom=c("point", "smooth"), facets=.~drv, method="loess")
g <- ggplot(mpg, aes(displ, hwy))
summary(g)
g+geom_point() # add a layer to plot
g+geom_point()+geom_smooth() # grey shadow around the blue line is the confidence band.
g+geom_point()+geom_smooth(method="lm")
g+geom_point()+geom_smooth(method="lm")+facet_grid(.~drv)
g+geom_point()+geom_smooth(method="lm")+facet_grid(.~drv)+ggtitle("Swirl Rules!")

## Each geom function can be modified with their options. theme() can be used to modify aspects of entire plot. theme_gray() is default theme, theme_bw() is plainer color scheme
g+geom_point(color="pink", size=4, alpha=1/2)
g+geom_point(size=4, alpha=1/2, aes(color=drv))

g+geom_point(aes(color=drv))+ labs(title="Swirl Rules!") + labs(x="Displacement", y="Hwy Mileage")

g+geom_point(aes(color=drv), size=2, alpha=1/2)+geom_smooth(size=4, linetype=3, method="lm", se=FALSE) # se=FALSE turns off confidence intervals or std error bands

g+geom_point(aes(color=drv))+theme_bw(base_family = "Times")

plot(myx, myy, type="l", ylim=c(-3,3)) #line plot of some test data with preset y range shown
##do the same thing with ggplot
g <- ggplot(testdat, aes(x=myx, y=myy))
g+geom_line()
g+geom_line()+ylim(-3,3) # creates a break in line plot at the excluded outlier.

## add a call to coord_cartesian with ylim=c(-3,3) to prevent broken lines
g+geom_line()+coord_cartesian(ylim = c(-3,3)) # this call just zooms in on the graph between y=-3 to 3

g <- ggplot(mpg, aes(x=displ, y=hwy, color=factor(year)))
g+geom_point()
g+geom_point()+facet_grid(drv~cyl, margins = TRUE)
g+geom_point()+facet_grid(drv~cyl, margins = TRUE)+geom_smooth(method="lm", se=FALSE, size=2, color="black")
g+geom_point()+facet_grid(drv~cyl, margins = TRUE)+geom_smooth(method="lm", se=FALSE, size=2, color="black")+labs(x="Displacement", y="Highway Mileage", title="Swirl Rules!")

#### GGplot extras
qplot(price, data=diamonds)
range(diamonds$price)
qplot(price, data=diamonds, binwidth=18497/30)
qplot(price, data=diamonds, binwidth=18497/30, fill=cut)

## plot histogram asa density function
qplot(price, data = diamonds, geom="density")
qplot(price, data = diamonds, geom="density", color=cut)

##scatterplots
qplot(carat, price, data=diamonds)
qplot(carat, price, data=diamonds, shape=cut)
qplot(carat, price, data=diamonds, color=cut)

qplot(carat, price, data=diamonds, color=cut,geom=c("point", "smooth"), method="lm")
qplot(carat, price, data=diamonds, color=cut,geom=c("point", "smooth"), method="lm", facets=.~cut)

## back to ggplot
g <- ggplot(diamonds, aes(depth, price))
g+geom_point(alpha=1/3) # create scatterplot of relationship b/n depth and price
cutpoints <- quantile(diamonds$carat, seq(0,1,length=4), na.rm = TRUE)
diamonds$car2 <- cut(diamonds$carat, cutpoints)
g+geom_point(alpha=1/3)+facet_grid(cut ~ car2)
g+geom_point(alpha=1/3)+facet_grid(cut ~ car2)+geom_smooth(method="lm", size=3, color="pink")

ggplot(diamonds, aes(carat, price))+geom_boxplot()+facet_grid(.~cut)

#### Hierarchical Clustering -- useful in early stages of analysis. helps find patterns or relationships betweendifferent factos or variables. Creates a hierarchy of clusters. Clustering organizes data points that are close into groups. How do we define closeness? how to group? how to interpret grouping?
## Hierarchical clustering is an agglomerative or bottom up approach. Each observation starts in its own cluster, and pairs of clusters are merged as one moves up the hierarchy. We'll find the closest two points and put them together in one cluster, then find the next closest pair in the updated picture, and so forth. Repeat this process until we reach a reasonable stopping place. One can stop clustering eeither when the clusters are too far apart to be merged (distance criterion) or when there is a sufficiently small number of clusters (number criterion)

## How do we define closeness? Distance/similarity are the metrics usually used to denote closeness. Euclidean distance and correlation similarity are continuous measures of closeness while Manhattan distance is a binary measure. Its important to use a measure of distance that fits the data / problem being investigated

## Manhatan distance or city block distance -> sum of absolute values of distances between each coordinate so the distance between points (x1, y1) and (x2, y2) is |x1-x2|+|y1=y2|. can be generalized for more than 2 dimensions like euclidean distance

## lets use the random data in the dataFrame object and create a dendrogram. A dendogram is an abstract picture or graph which shows how the 12 points in the dataFrame dataset cluster together. Clusters that are close are connected with a line. we;kk use Euclidean distance as a metric for closeness

dist(dataFrame) #calculate euclidean dist. returns a lower triangular matrix. distxy
hc <- hclust(distxy)
plot(hc)
plot(as.dendrogram(hc))
abline(h=1.5, col='blue')
abline(h=.4, col='red')
abline(h=.05, col='green')
