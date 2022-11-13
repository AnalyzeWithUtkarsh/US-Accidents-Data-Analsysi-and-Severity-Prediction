# US Accidents data analysis

# first we will import necessary libraries
library(dplyr)
library(ggplot2)
library(skimr)
library(tidyr)
library("scales")
library(corrplot)

# we will load our data set
US_Accident <- read.csv("D:\\III YEAR ASSIGNMENTS\\Kaggle Datasets\\US Accidents\\US_Accidents_Dec21_updated.csv")

#checking column names in the dataset
colnames(US_Accident)

View(US_Accident)
skim_without_charts(US_Accident)

# some key points about the data set
# - there are 47 columns in the data set
# - 2825342 rows are present in the data sets
# - 33 character columns
# - 14 numeric columns


# counting number of severe accidents using histogram
ggplot(US_Accident, aes(x=Severity, fill=factor(Severity))) + geom_histogram(bins=4)
# From the histogram we got to know that very few accidents were having severity greater than 2 
# most of the accidents were of severity 2
table(US_Accident$Severity)
# we will calculate the percentage of most severe accidents
severe <- round(((155105+131193)/2845342)*100,2)
severe
# from this we can infer that only 10 percent accidents have severity more than 2


# calculating number of distinct cities in the data set
n_distinct(US_Accident$City)



table(US_Accident$Side)
# it shows that accidents occur more on the right hand side as they drive cars on right side of the road unlike India.

# extracting date and time from datetime column
US_Accident$Start_Date <- as.Date(US_Accident$Start_Time)
US_Accident$Start_Time <- format(as.POSIXct(US_Accident$Start_Time), format="%H:%M:%S")
US_Accident$End_Date <- as.Date(US_Accident$End_Time)
US_Accident$End_Time <- format(as.POSIXct(US_Accident$End_Time), format="%H:%M:%S")
View(US_Accident)

# extracting year from date column
US_Accident$Start_Year <- as.numeric(format(US_Accident$Start_Date,'%Y'))

# extracting month from date 
US_Accident$Month <- as.numeric(format(US_Accident$Start_Date, '%m'))


# plotting histogram for number of accidents each year
table(US_Accident$Start_Year)
ggplot(US_Accident, aes(x = Start_Year, fill = factor(Start_Year))) + geom_histogram(bins=6)


# plotting bar graph for number of accidents by month
ggplot(US_Accident, aes(x = Month, fill = factor(Month))) + geom_bar() + scale_x_continuous('Month', limits = c(0,14), expand = c(0,0))
# from this graph we got to know that many accidents occur in the winter season 


#mean(US_Accident$Temperature.F.)
#is.na(US_Accident$Temperature.F.)


top_5_states <- tail(sort(table(US_Accident$State)),5)
top_5_states
barplot(top_5_states,
        main = "Maximum Accidents in States",
        ylab = "States",
        xlab = "Number of Accidents",
        col  = factor(top_5_states),
        horiz = TRUE)

top_5_city <- tail(sort(table(US_Accident$City)),5)
top_5_city

barplot(top_5_city,
        main = "Maximum Accidents by City",
        xlab = "City",
        ylab = "Number of Accidents",
        col = factor(top_5_city),
        )








# EDA for Houston city
Houston <- filter(US_Accident, City == "Houston")
View(Houston)
# total accidents recorded in the city is 39,448
#checking severity count
table(Houston$Severity)
# same case can be seen 


# number of accidents by year
ggplot(Houston, aes(x = Start_Year, fill = factor(Start_Year))) + geom_histogram(bins=6)
#number of accidents by month
ggplot(Houston, aes(x = Month, fill = factor(Month))) + geom_histogram(bins=12)
# same trend can be seen 

#filling NA values with the median value of the dataset.
Houston$Temperature.F.[is.na(Houston$Temperature.F.)] <- median(Houston$Temperature.F., na.rm = T)
mean(Houston$Temperature.F.)
# so mean temperature of the Houston city is 72.58 F


# selecting top 5 most accidents prone streets in Houson city
H5Streets <- tail(sort(table(Houston$Street)),5)
H5Streets
barplot(H5Streets, 
        col = factor(H5Streets), 
        main = "Top 5 Streets in Houston", 
        xlab = "StreetName", 
        ylab = "Number of Accidents")



# selecting top 5 county
H5County <- tail(sort(table(Houston$County)),5)
H5County

# Harris is the most danegerous county 


Hpercentage <- (39248/39448)*100
Hpercentage

# calculating mean values for Houston city

#calculating mean of humidity
is.na(Houston$Humidity...)
Houston$Humidity...[is.na(Houston$Humidity...)] <- median(Houston$Humidity..., na.rm = T)
mean(Houston$Humidity...)
# humidity mean is 67.137

#calculating mean of pressure
Houston$Pressure.in.[is.na(Houston$Pressure.in.)] <- median(Houston$Pressure.in., na.rm = T)
mean(Houston$Pressure.in.)
# pressure mean is 29.97

#calculating mean of visiblity
Houston$Visibility.mi.[is.na(Houston$Visibility.mi.)] <- median(Houston$Visibility.mi., na.rm = T)
mean(Houston$Visibility.mi.)
#9.24 is the mean 


# analyzing the harris county
harris <- filter(Houston, County == "Harris")
View(harris)

mean(harris$Temperature.F.)
mean(harris$Visibility.mi.)
mean(harris$Pressure.in.)
mean(harris$Humidity...)

# combining different columns
q <- data.frame(Houston$Severity,Houston$Temperature.F.,Houston$Humidity...,Houston$Pressure.in.,Houston$Visibility.mi.)
View(Q)
Q <- cor(q)
corrplot(Q, method = "circle")









# EDA for Dallas city
Dallas <- filter(US_Accident, City == "Dallas")
View(Dallas)
# there are total 41,979 accidents recorded in the Dallas city

# checking severity count
table(Dallas$Severity)

# filling NA values in temperature column with median value of temperature column
Dallas$Temperature.F.[is.na(Dallas$Temperature.F.)] <- median(Dallas$Temperature.F., na.rm = T)
mean(Dallas$Temperature.F.)
# mean temperature is 67.68 F


# number of accidents by year
ggplot(Dallas, aes(x = Start_Year, fill = factor(Start_Year))) + geom_histogram(bins=6)
#number of accidents by month
ggplot(Dallas, aes(x = Month, fill = factor(Month))) + geom_histogram(bins=12)
# same trend can be seen 



#selecting top 5 most accidnets prone streets
D5Streets <- tail(sort(table(Dallas$Street)),5)
D5Streets
barplot(D5Streets, 
        col = factor(D5Streets), 
        main = "Top 5 Streets in Dallas", 
        xlab = "StreetName", 
        ylab = "Number of Accidents")



# selecting top 5 county
D5County <- tail(sort(table(Dallas$County)),5)
D5County

# dallas county is having most accidents

Dpercentage <- (41021/41979)*100
Dpercentage


Dallas$Humidity...[is.na(Dallas$Humidity...)] <- median(Dallas$Humidity..., na.rm = T)
Dallas$Pressure.in.[is.na(Dallas$Pressure.in.)] <- median(Dallas$Pressure.in., na.rm = T)
Dallas$Visibility.mi.[is.na(Dallas$Visibility.mi.)] <- median(Dallas$Visibility.mi., na.rm = T)


z <- data.frame(Dallas$Severity,Dallas$Temperature.F.,Dallas$Humidity...,Dallas$Pressure.in.,Dallas$Visibility.mi.)
View(Z)
Z <- cor(z)
corrplot(Z, method = "circle")









# EDA for Orlando city
Orlando <- filter(US_Accident, City == "Orlando")
View(Orlando)
# total accidents recorded in the city is 54,691
#checking severity count
table(Orlando$Severity)
# same case can be seen 


# number of accidents by year
ggplot(Orlando, aes(x = Start_Year, fill = factor(Start_Year))) + geom_histogram(bins=6)
#number of accidents by month
ggplot(Orlando, aes(x = Month, fill = factor(Month))) + geom_histogram(bins=12)
# same trend can be seen 

#filling NA values with the median value of the dataset.
Orlando$Temperature.F.[is.na(Orlando$Temperature.F.)] <- median(Orlando$Temperature.F., na.rm = T)
mean(Orlando$Temperature.F.)
# so mean temperature of the Houston city is 75.84 F


# selecting top 5 most accidents prone streets in Orlando city
O5Streets <- tail(sort(table(Orlando$Street)),5)
O5Streets
barplot(O5Streets, 
        col = factor(O5Streets), 
        main = "Top 5 Streets in Orlando", 
        xlab = "StreetName", 
        ylab = "Number of Accidents")


# selecting top 5 county
O5County <- tail(sort(table(Orlando$County)),5)
O5County

#orange is the dominating counting in the city

Opercentage <- (54654/54691)*100
Opercentage

Orlando$Visibility.mi.[is.na(Orlando$Visibility.mi.)] <- median(Orlando$Visibility.mi., na.rm = T)
Orlando$Humidity...[is.na(Orlando$Humidity...)] <- median(Orlando$Humidity..., na.rm = T)
Orlando$Pressure.in.[is.na(Orlando$Pressure.in.)] <- median(Orlando$Pressure.in., na.rm = T)



d <- data.frame(Orlando$Severity,Orlando$Temperature.F.,Orlando$Humidity...,Orlando$Pressure.in.,Orlando$Visibility.mi.)
View(d)
D <- cor(d)
corrplot(D, method = "circle")














# EDA for Los Angeles city
Los_Angeles <- filter(US_Accident, City == "Los Angeles")
View(Los_Angeles)
# total accidents recorded in the city is 68,976
#checking severity count
table(Orlando$Severity)
# same case can be seen 


# number of accidents by year
ggplot(Los_Angeles, aes(x = Start_Year, fill = factor(Start_Year))) + geom_histogram(bins=6)
#number of accidents by month
ggplot(Los_Angeles, aes(x = Month, fill = factor(Month))) + geom_histogram(bins=12)
# same trend can be seen 

#filling NA values with the median value of the dataset.
Los_Angeles$Temperature.F.[is.na(Los_Angeles$Temperature.F.)] <- median(Los_Angeles$Temperature.F., na.rm = T)
mean(Orlando$Temperature.F.)
# so mean temperature of the Los Angekes city is 75.84 F


# selecting top 5 most accidents prone streets in Los Angeles city
L5Streets <- tail(sort(table(Los_Angeles$Street)),5)
L5Streets
barplot(L5Streets, 
        col = factor(L5Streets), 
        main = "Top 5 Streets in Los Angeles", 
        xlab = "StreetName", 
        ylab = "Number of Accidents")




# selecting top 5 county 
L5County <- tail(sort(table(Los_Angeles$County)),5)
L5County

#there is only 1 county in los angeles


Los_Angeles$Pressure.in.[is.na(Los_Angeles$Pressure.in.)] <- median(Los_Angeles$Pressure.in., na.rm = T)
Los_Angeles$Humidity...[is.na(Los_Angeles$Humidity...)] <- median(Los_Angeles$Humidity..., na.rm = T)
Los_Angeles$Visibility.mi.[is.na(Los_Angeles$Visibility.mi.)] <- median(Los_Angeles$Visibility.mi., na.rm = T)


c <- data.frame(Los_Angeles$Severity,Los_Angeles$Temperature.F.,Los_Angeles$Humidity...,Los_Angeles$Pressure.in.,Los_Angeles$Visibility.mi.)
View(c)
C <- cor(c)
corrplot(C, method = "circle")









# EDA for Miami city
Miami <- filter(US_Accident, City == "Miami")
View(Miami)
# total accidents recorded in the city is 106,966
#checking severity count
table(Miami$Severity)
# same case can be seen 


# number of accidents by year
ggplot(Miami, aes(x = Start_Year, fill = factor(Start_Year))) + geom_histogram(bins=6)
#number of accidents by month
ggplot(Miami, aes(x = Month, fill = factor(Month))) + geom_histogram(bins=12)
# same trend can be seen 

#filling NA values with the median value of the dataset.
Miami$Temperature.F.[is.na(Miami$Temperature.F.)] <- median(Miami$Temperature.F., na.rm = T)
mean(Miami$Temperature.F.)
# so mean temperature of the Miami city is 78.60 F


# selecting top 5 most accidents prone streets in Los Angeles city
M5Streets <- tail(sort(table(Miami$Street)),5)
M5Streets
barplot(M5Streets, 
        col = factor(M5Streets), 
        main = "Top 5 Streets in Miami", 
        xlab = "StreetName", 
        ylab = "Number of Accidents")

#selecting top 5 countys

M5County <- tail(sort(table(Miami$County)),5)
M5County

# all 5 streets are in Miami-Dade county which makes it the most accident prone in the city. 


per <- (106891/106966)*100
per
# we can say that almost 100 percent accidents that occurred in Miami city happened in Malde_Dade county 
# one possible reason can be population malde-dade county may be home to almost every person in the city
# government needs to take preventive measures in the county 
# they can educate people about this statistics and should encourage safe driving by its citizen

Miami$Pressure.in.[is.na(Miami$Pressure.in.)] <- median(Miami$Pressure.in., na.rm = T)
Miami$Visibility.mi.[is.na(Miami$Visibility.mi.)] <- median(Miami$Visibility.mi., na.rm = T)
Miami$Humidity...[is.na(Miami$Humidity...)] <- median(Miami$Humidity..., na.rm = T)


x <- data.frame(Miami$Severity,Miami$Temperature.F.,Miami$Humidity...,Miami$Pressure.in.,Miami$Visibility.mi.)
View(x)
X <- cor(x)
corrplot(X, method = "circle")
















# looking at this trend it appears like 
#when the temperature increases the number of accidents increases 
#in these cities but 
#when we plotted the bar graph 
#we saw a negative trend accidents increases with drop in temperature
ggplot(Houston, aes(x = Airport_Code, fill = factor(Airport_Code))) + geom_bar()
 











  



