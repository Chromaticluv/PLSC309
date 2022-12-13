## Set working directory, load in data set ##
setwd("D:/school/PLSC/PLSC309/Lab reports/Lab report 1")
data<-read.csv("Fariss2010.csv")

#Set global objects/values ##
break_values = c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10) #for histogram
data_democracies = subset(data, democracy==1, #subset data for democracies
                   na.rm = TRUE)
data_nondemocracies = subset(data,democracy != 1, #subset data for non-democracies
                             na.rm = TRUE)
colors<-c("Red","Blue") #fore use in scatterplot

############################ RAW STATISTICS #####################################

##Polity score statistics##
summary(data$polity2, na.rm = TRUE)
sd(data$polity2, na.rm = TRUE)

## Respect for Human Rights score Statistics ##
summary(data$respectHR, na.rm = TRUE)
print(sd(data$respectHR, na.rm = TRUE))

## Treaty count stats ##
summary(data$TreatyCount, na.rm = TRUE)

##mean respect for human rights between democracies and non-democracies##
mean(data_democracies$respectHR) #mean of respect for human rights among democracies
mean(data_nondemocracies$respectHR) #mean of respect for human rights among non-democracies
mean(data$respectHR)#mean of respect for human rights in total data
t.test(data_nondemocracies$respectHR,data_democracies$respectHR) #test the difference in means

############################## GRAPHICS #######################################################

##histogram of polity scores ##
hist(data$polity2,
     breaks = break_values, #set bins to each represent a single polity score.
     main = "fig. 1: Polity Score Distribution",
     xlab = "Polity score",
     col = "skyblue")

## Bar plots for human rights treaties signed by democracies/non-democracies ##
barplot(height = data_democracies$TreatyCount, #Creat a bar plot of treaty count by democracies
        horiz = TRUE, #Set Treatycount as X Axis
        main = "fig. 2: Count of HR treaties signed by democracies",
        xlab= "Treaties signed",
        ylab= "Country",
        col = "blue")
barplot(height = data_nondemocracies$TreatyCount,
        main = "fig. 3: Count of HR treaties signed by non-democracies",
        horiz = TRUE, 
        xlab = "Treaties signed",
        ylab= "Country",
        col = "red")

##Scatterplot of treaties and respect for human rights
plot(data$TreatyCount,data$respectHR,
     main = 'fig. 4: Number of treaties signed by a country vs. Respect of Human Rights score',
     xlab = 'Number of treaties signed by country',
     ylab = 'Respect for human rights score',
     col = colors[factor(data$democracy ==1)]) #set non-democracies to red, democracies to blue
abline(lm(data$respectHR ~ data$TreatyCount), col = 'red') #create regression line
