library('ggplot2')

head(diamonds)
head(mtcars)
#qplot(clarity, data = diamonds, fill=cut, geom="bar")
qplot(wt, mpg, data=mtcars)

install.packages('devtools')
require(devtools)
install_github('rCharts', 'ramnathv')

## checking out the metricgraphics r library
devtools::install <- github("hrbrmstr/metricsgraphics")
library(RColorBrewer)
library(metricsgraphics)

tmp <- data.frame(year=seq(1790, 1970, 10), uspop=as.numeric(uspop))
chrt <- tmp %>%
    mjs_plot(x=year, y=uspop, width = 600) %>%
    mjs_line() %>%
    mjs_add_marker(1850, "Something Wonderful") %>%
    mjs_add_baseline(150, "Something Awful")
install.packages('shiny')
chrtstr <- metricsgraphicsOutput(chrt)
chrtstr %>% typeof
paste(chrtstr, collapse="")

install.packages('swirl')
