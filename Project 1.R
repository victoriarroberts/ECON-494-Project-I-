#############################
## ECON 494 PROJECT 1 F20 ###
#############################
## Importing Data as CSV file ##
df<-read.csv('/Users/victoriaroberts/Local\ Documents/ECON_494_DATASET.csv')
dataset<- read.csv('/Users/victoriaroberts/Local\ Documents/ECON_494_DATASET.csv')
View(dataset) #open data-summarized-in another tab

##Preliminary Exploratory Analysis##
dim(dataset)    ##Checks dimensions of the dataset as a matrix object
head(dataset)   ##Shows the first six observations
tail(dataset)   ##Shows the last six observations
summary(dataset)
summary(df$Unemployment.rate)

#################
##CLEANING DATA##   
#################
#Delete unnecessary columns and rows#

library(dplyr) #download package to delete columns and rows
dataset2<- dataset  #new dataset to be able to compare when columns/rows are deleted
dataset2[9:11] <- list(NULL)  ##Deleted columns X,X.1,and X.2 that were irrelevant to the dataset
dataset2$Code <- NULL         ##Deleted column 'Code' 
dataset2$Year <- NULL         ##Deleted column 'Year'
View(dataset2)                ##Opened new dataset in Rscript tab 
##Data has 5 less variables

##Delete NA's and rows##
dataset2<- na.omit(dataset2)   ##This function returns a list of rows without NA values
##Easiest way to remove rows and na values at the same time
complete.cases(dataset2)   ##This function returns vector of rows with NA values
View(dataset2)             ##View new dataset without NA values

##Change column/variable names##
dataset2 <-dataset2 %>% 
  rename(
    Inflation.rate = Inflation..percent.change.in.the.Consumer.Price.Index,
    Population = Population.size.in.millions,
    Spending.education= Public.spending.on.education.percent.of.GDP,
    GDP= Nominal.GDP.2017
  )
View(dataset2)   ##Check to see if the variables changed 
####################################
#EXPLORATORY ANALYSIS ON CLEAN DATA#
####################################

head(dataset2)       ##exploratory analysis with new data to see the changes between tidy and untidy datasets
tail(dataset2)
summary(dataset2$Spending.education)

##Exploratory Analysis with Visualizations##

hist(dataset2$Unemployment.rate) #plots a histogram for the 'Unemployment Rate' variable.
hist(dataset2$Spending.education) #plots a histogram for the 'Public spending on education' variable.
hist(dataset2$Inflation.rate)     #plots a histogram for the 'Inflation Rate' variable.

##Check to see if variable is normally distributed
hist(dataset2$Unemployment.rate, prob = TRUE) #generates a histogram for the Unemployment rate variable
hist(dataset2$Inflation.rate, prob = TRUE)
#add calibrated normal density curve to histogram-unemployment rate
curve(dnorm(x, mean = mean(dataset2$Unemployment.rate), sd = sd(dataset2$Unemployment.rate)), col = "darkblue", lwd = 2, add = TRUE)
curve(dnorm(x, mean = mean(dataset2$Inflation.rate), sd = sd(dataset2$Inflation.rate)), col = "darkblue", lwd = 2, add = TRUE)

#generate a nonparametric density estimate of the distribution of the Unemployment variable
plot(density(dataset2$Unemployment.rate)) 

##Test to see if there are relationships between variables##

##Generating a Scatterplot can help to zoom in on particular pair of variables##
plot(dataset2$Inflation.rate~dataset2$Unemployment.rate) #plot(y~x)
###############################
##Install the GGPLOT2 package##   ##To get more accurate visualizations##
###############################
library(ggplot2)
##Create Scatterplot rendering the data using the point geom
ggplot(dataset2, aes(x = Unemployment.rate, y = Inflation.rate)) + 
  geom_point()

ggplot(dataset2, aes(Unemployment.rate)) + geom_histogram()   ##Histogram for unemployment rate
ggplot(dataset2, aes(Inflation.rate)) + geom_histogram()
ggplot(dataset2, aes(Spending.education)) + geom_histogram()

ggplot(dataset2, aes(Spending.education, Unemployment.rate)) + geom_point(color = "blue") #scatterplots with color
ggplot(dataset2, aes(Inflation.rate, Unemployment.rate )) + geom_point(color = "red")
ggplot(dataset2, aes(Population, Unemployment.rate )) + geom_point(color = "Orange")



ggplot(dataset2, aes(Unemployment.rate, Spending.education)) +    ##Scatterplot using smoothing tool
  geom_point() +
  geom_smooth()

write.csv(dataset2, file= "econ.dataset2")




