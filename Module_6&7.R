## Module 6
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
library(curl)
load("countrydata_2016")
read.csv("/Users/izzynovick/Desktop/BI588_Fall_2021/Module_practice/countrydata_2016")
getwd()
#oh I get why because this isn't where this is located in my computer got it got it

f <- curl("countrydata_2016")
f
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)
## wasn't able to read the saved csv file, could only get it from internet
library(curl)
f <- curl("https://raw.githubusercontent.com/fuzzyatelin/fuzzyatelin.github.io/master/AN588_Fall21/Country-Data-2016.csv")
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)
## I think it works but it gives me a warning message. nvm it works

summary(d)
names(d)

##challenge 1
summary(countrydata_2016)
##Median population size :4.912e+06
##Median area: 69700

#creating the density column in the d dataframe
d$density <- d$population/d$area
d <- d[order(-d$density),]
d[1:10,]

d <- d[order(d$area),]

d[1:10,]
new <- d[grep("^[A-F]", d$country),]
summary(new)
mean(new$population, na.rm = TRUE)
mean(new$area, na.rm = TRUE)
par(mfrow = c(2, 3))
boxplot(d$population)
boxplot(log(d$population))
boxplot(d$area)
boxplot(log(d$area))
barplot(d$population)
barplot(d$area)
par(mfrow=c(1,2))
attach(d)
hist(log(population), freq=FALSE, col = "red", main = "Plot 1", xlab = "log(population size)", ylab = "density", ylim = c(0,0.2))
hist(log(area), freq=FALSE, col = "red", main = "Plot 2", xlab = "log(area)", ylab = "density", ylim = c(0,0.2))
par(mfrow=c(1,1))
hist(log(population), freq=FALSE, col = "white", main = "My Plot with Mean and Density", xlab = "log(population size)", ylab = "density", ylim = c(0,0.2))
abline(v=mean(log(population),na.rm=TRUE), col= "blue")
lines(density(log(population), na.rm =TRUE), col = "green")
detach(d)
sort(table(d$govt_form),decreasing=TRUE)

f <- curl("https://raw.githubusercontent.com/fuzzyatelin/fuzzyatelin.github.io/master/AN588_Fall21/KamilarAndCooperData.csv")
d <- read.csv(f, header = TRUE, stringsAsFactors = FALSE)
attach(d)
head(d)
summary(d)
boxplot(log(Body_mass_female_mean)~Family,d)
detach(d)
library(ggplot2)
p <- ggplot(data=d, aes(x=Family, y=log(Body_mass_female_mean)))
p <- p + geom_boxplot()
p <- p + theme(axis.text.x=element_text(angle=90))
p <- p + ylab("log(Female Body Mass)")
p

attach(d)
par(mfrow=c(1,2))
plot(x = Body_mass_female_mean, y = Brain_Size_Female_Mean)
plot(x = log(Body_mass_female_mean), y = log(Brain_Size_Female_Mean))
detach(d)

p <- ggplot(data=d, aes(x=log(Body_mass_female_mean),
                        y=log(Brain_Size_Female_Mean),
                        color = factor(Family)
)) # first, we build a plot object and color points by Family
p <- p + xlab("log(Female Body Mass)") + ylab("log(Female Brain Size)") # then we modify the axis labels
p <- p + geom_point() # then we make a scatterplot
p <- p + theme(legend.position="bottom", legend.title=element_blank()) # then we modify the legend
p # and, finally, we plot the object

p <- p + facet_wrap(~ Family, ncol=4)
p <- p + theme(legend.position="none")
p
p <- p + geom_smooth(method="lm", fullrange=TRUE)
p

p <- ggplot(data=d, aes(x=log(Body_mass_female_mean),
                        y=log(MaxLongevity_m)
))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p

aggregate(d$Body_mass_female_mean~d$Family, FUN = "mean", na.rm = TRUE)
aggregate(x = d["Body_mass_female_mean"], by = d["Family"], FUN = "mean", na.rm = TRUE)
library(dplyr)

s <- filter(d, Family == "Hominidae" & Mass_Dimorphism > 2)
head(s)

s <- arrange(d, Family, Genus, Body_mass_male_mean) # rearranging a data frame...
head(s)
s <- select(d, Family, Genus, Body_mass_male_mean) # selecting specific columns...
head(s)
s <- rename(d,"Female_Mass" = Body_mass_female_mean)
head(s$Female_Mass)
s <- mutate(d, "Binomial" = paste(Genus, Species, sep=" "))
head(s$Binomial)
s <- summarise(d,
               avgF = mean(Body_mass_female_mean, na.rm=TRUE),
               avgM = mean(Body_mass_male_mean, na.rm=TRUE))
s
byFamily <- group_by(d, Family)
byFamily
s <- summarise(byFamily,
               avgF = mean(Body_mass_female_mean, na.rm=TRUE),
               avgM = mean(Body_mass_male_mean, na.rm=TRUE))
s
s <- d %>% 
  mutate(Binomial = paste(Genus, Species, sep=" ")) %>%
  select(Binomial, Family, Body_mass_female_mean, Body_mass_male_mean, Mass_Dimorphism) %>%
  group_by(Family) %>%
  summarise(avgF = mean(Body_mass_female_mean, na.rm=TRUE),
            avgM = mean(Body_mass_male_mean, na.rm=TRUE),
            avgBMD = mean(Mass_Dimorphism, na.rm=TRUE))
s

## Module 7
install.packages("sciplot")
x <- c(1,2,3,4,5,6,7,8,9,10,25,50,100,200,1000)
gm1 <- function(x){prod(x)^(1/length(x))}
gm1(x)
gm2 <- function(x){exp(mean(log(x)))}
gm2(x)

ss1 <- function(x){sum((x-mean(x))^2)}
ss1(x)

ss2 <- function(x){sum(x^2) - length(x)*mean(x)^2}
ss2(x)

ss3 <- function(x){sum(x^2) - (sum(x))^2/length(x)}
ss3(x)

pop_v <- function(x){sum((x-mean(x))^2)/(length(x))}
pop_v(x)

sample_v <- function (x){sum((x-mean(x))^2)/(length(x)-1)}
sample_v(x)

plot(c(0, 50), c(0, 15), type="n", xlab= "Sample size", ylab="Variance")
for (n in seq(5,50,5))
  # samples of 5, 10, 15...
{
  for(i in 1:50)
    # 50 replicates
  {
    x<- rnorm(n=50, mean=10 ,sd=2)
    points(n,var(x))
  }
}
## this didn't immediately work lol

pop_sd <- function(x){sqrt(pop_v(x))}
pop_sd(x)
sample_sd <- function(x){sqrt(sample_v(x))}
sample_sd(x)

SE1 <- function(x){sqrt(sample_v(x)/length(x))}
SE1(x)

SE2 <- function(x){sqrt(var(x)/length(x))}
SE2(x)

library(sciplot)
se(x)

set.seed(1)
x <- rnorm(10000,0,1)
hist(x)

x<-seq(from = -4, to = 4, by = 0.01)
plot(x,dnorm(x), cex=0.4)

plot(x,pnorm(x), cex=0.4)
x<-seq(from = 0, to = 1, by = 0.01)
plot(qnorm(x),x,cex=0.4)

x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
m <- mean(x)
n <- length(x)
v <- var(x)
s <- sd(x)
e <- sqrt(v/n)
upper <- mean(x) + qnorm(0.975, mean=0, sd=1)*se(x)
lower <- mean(x) + qnorm(0.025, mean=0, sd=1)*se(x) # or lower <- mean(x) - qnorm(0.975)*se(x)
ci <- c(lower,upper)
ci

upper <- m + qnorm(0.975, mean=0, sd=1)*e
lower <- m + qnorm(0.025, mean=0, sd=1)*e # or lower <- m - qnorm(0.975)*e
ci <- c(lower,upper)
ci

normalCI = function(x, CIlevel = 0.95) {
  upper = m + qnorm(1 - (1 - CIlevel)/2) * sqrt(var(x)/length(x))
  lower = m + qnorm((1 - CIlevel)/2) * sqrt(var(x)/length(x))
  ci <- c(lower, upper)
  return(ci)
}
normalCI(x, 0.95)

set <- NULL # sets up a dummy variable to hold our 10000 simulations
n <- 50
for (i in 1:10000){
  set[i] <- mean(sample(x, n, replace=TRUE))
}

quantile(set)
quantile(set, c(0.025, 0.975))
