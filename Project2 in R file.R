#Q1
#Compare  the number round trips with one way trips

chi <- read.csv('chicago.csv')
library(ggplot2)
# I run this function to know the levels for Start.Station and End.Station
str(chi$Start.Station)
str(chi$End.Station)

#I found out the levels of Start.Station is more than End.Station, so I run the folowing function
chi$Start.Station <- factor(chi$Start.Station, levels=levels(chi$End.Station))


#Then I compared the two coulmns and stored the result in a new coulmn 'trip_type'. If they are equaled then it is round trip
chi$trip_type <- ifelse(chi$Start.Station == chi$End.Station, 'round trip', 'one way') 
#To know the total number of round trips and One way trips
table(chi$trip_type)


#the position
quan <- as.vector(table(chi$trip_type))
pos <- cumsum(quan) - quan/2 #to put the label in the center of the pie, we use this equation
quantity <- data.frame(trip_type = c('round trip', 'one way'), quantity= quan, position = pos)


#Now, let's plot
mycols <- c('red','blue')# creat a variable to use it as colors of slices
pie <- ggplot(subset(chi, !is.na(trip_type)), aes(x= factor(1), fill= trip_type))+ #I deleted the missing values in trip_type 
  geom_bar(width = 1,color = 'white')+ 
  geom_text(data = quantity, aes(x=factor(1), y = position, label= quantity), size= 5, color='white')+ #to add text label and change the color of it
  labs(x='', y= '')+
  ggtitle('Distribution of the Trip Types')+
  scale_fill_manual(values = mycols)+ #this layer to change fill color manually
  theme_void() #to remove axes and background

pie + coord_polar(theta = 'y')

#It is clear from the pie chart, the number of one way trips in Chicago is higher than round trips.




#Q2
#Which month has the highes number of trips?

wash <- read.csv('washington.csv')

#to convert Start.Time from factor to date
class(wash$Start.Time)
wash$Start.Time2 <- as.POSIXlt(wash$Start.Time, format = "%Y-%m-%d %H:%M:%S")
head(wash$Start.Time2)



#now lets store the month of start time into a variable Start.Time3
wash$Start.Time3<-format(wash$Start.Time2, "%B")

#To know the total number of ridership per month
table(wash$Start.Time3)

#I plot that variable
ggplot(wash, aes(x = Start.Time3 )) +
  geom_histogram(stat = 'count', colour = 'white', binwidth = 50) +
  labs(x = "Start Time of trip by Month")+
  labs(y = "Frequency")+
  ggtitle('Distribution of Start Time by Month')

#I found out that there are only 6 months the riderships rented the bikes. Also, the chart shows June as the highest number of trips in Washington.



#Q3
#Which month has the highes number of trips per gender?


#Actually it takes long time to know what's going on in this data
#I found out that there is a problem with the missing values. it used as blank space , not as 'NA'. So, I read the file like this way:
ny <- read.csv('new-york-city.csv', header=T, na.strings = c(""," ","NA") )




#Now I convert Start.Time from factor to date
class(ny$Start.Time)
ny$Start.Time2 <- as.POSIXlt(ny$Start.Time, format = "%Y-%m-%d %H:%M:%S")
head(ny$Start.Time2)



#now lets store the month of start time into a variable Start.Time4
ny$Start.Time4<-format(ny$Start.Time2, "%B")


#Now let's plot 
wt <- ggplot(aes(x = Start.Time4 ), data = subset(ny,!is.na(Gender))) +
  geom_histogram(stat = 'count', colour = 'white', binwidth = 15)+
  labs(x = "Start Time by Month") +
  labs(y = "Frequency")+
  ggtitle('Distribution of Start Time by Month and Sex ')+
  facet_wrap(~Gender, nrow=2)
wt

#to know the total number of ridership per month for each gender
by(ny$Start.Time4, ny$Gender, summary)


#The bar charts show that number of male who rented the bikes is more than the number of female. Also, the charts show June as the highest number of trips in Washington.
