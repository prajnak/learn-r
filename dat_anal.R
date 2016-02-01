## t-tests

## one sample t-test using mtcars
## need to use a t-test instead of a z-test when we don't have access to population variance and must estimate it from data
mtcars
## want to find theres a significant differnce avg fuel efficiency for cars in mtcars, built in 1973-74 and avg fuel efficiency for all motor vehicles in 1974

## null hypothesis --> avg fuel efficency in dataset not significantly different from 12.0 mpg
## alt. hypothesis --> avg fuel efficiency in cars is siginficantly different from 12.0 mpg
## need to compute t-statistic and compare to an appropriate t ciritical value to accept or reject null hypothesis
allMPG  = mtcars$mpg

## to compute a t-statistic --> sample mean, assumed population mean, variance and sample size
sampleMeanMPG <- mean(allMPG)
## assumed pop mean = 12.0 mpg given in report of mtcars
## sample variance
sampVarMPG <- var(allMPG)
## sample size
n <- length(allMPG)
## compute t-statistic
tStat <- (sampleMeanMPG - 12.0)/sqrt(sampVarMPG/n)
## compare to t critical value
## dof
myDF <- n - 1

## Remember that a p-value gives us the probability of getting a t-statistic as
## or more extreme than the one we actually got from the data. Therefore, since
## we have a positive t-statistic here, we want to first find the area under the
## t-distribution curve to the right of the value of our t-statistic. Then,
## since the t-distribution is symmetrical about 0, we can multiply this value
## by 2 to get the p-value for a two-sided test

## probability of getting a t-statistic greater than or equal to the one we got.
## multiple by two for a two sided test and get p-value
pVal1 <- pt(tStat, df = myDF, lower.tail = F)
pVal <- pVal1 * 2
## [1] 1.46295e-08
## safely reject null hypothesis because p value is well below 0.05,
## most commonly used significance level). So, there is a significant difference
## between fuel eff. of cars included in outr dataset and population avg fuel efficiency

## Most likely explanation --> population mean of 12.0 comes from both large and small
## vehicles, while mtcars dataset only has mainly small vehicles
## ower vehicle weight typically means higher fuel efficiency

## STandard R function for computing t-test p-values
## t.test
t.test(allMPG, mu=12.0, alternative = "two.sided")


### Two sample t-test
## Want to determine if there is a signiificant difference between avg fuel
## efficiency for cars with automatic and manual transmissions

## H0 --> avg fuel efficiency of auto/manual cars is same
## H1 --> avg fuel efficieny of auto and manual cars is difference with a statitically siginificant
## difference value

## compute t-statistic and compare to t-critical value to accept or reject H0
auto <- mtcars[mtcars$am==0, ]
man <- mtcars[mtcars$am==1, ]
autoMPG = auto$mpg
manMPG = man$mpg

## two sample t-test needs 6 imp values --> difference in sample means, assumed differences in
## population means, sample variance for each group, sample size for each group
sampleMeanAutoMPG <- mean(autoMPG)
sampMeanManMPG <- mean(manMPG)
diffSampMean <- sampMeanManMPG - sampMeanAutoMPG

## perform t-test under assumption that H0 is true, i.e., diff in pop. means is 0.
diffPopMean <- 0
sampvarAutoMPG = var(autoMPG)
sampVarManMPG = var(manMPG)
nAuto = length(autoMPG)
nMan = length(manMPG)

## t-static for two-sample
numer <- diffSampMean - diffPopMean
denom <- sqrt(sampvarAutoMPG/nAuto + sampVarManMPG/nMan)
tStat = numer/denom

## computing dof
myDF <- (sampVarManMPG/nMan + sampvarAutoMPG/nAuto)^2 / ((sampVarManMPG/nMan)^2/(nMan - 1) + (sampvarAutoMPG/nAuto)^2/(nAuto - 1))

## compute area under tcurve
pVal1 <- pt(tStat, df=myDF, lower.tail = F)
pVal = pVal1 * 2
## [1] 0.001373638
## p-value well below 0.05. safely reject H0 and accept H1. there is a statisticaly siginificant
## diffference in mpg between automatic and manual transmissions

## do the same thing using t.test
t.test(autoMPG, manMPG, mu=0, alternative = "two.sided" )


#### Errors Power and Sample Size

