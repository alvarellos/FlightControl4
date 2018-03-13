# Diego Diaz

myDF <- read.csv("C://Users/Diego/Documents/R/2008.csv")

# how the tapply functions works:
# 1. the vector of data we want to apply a function to
# 2. the way to break up the data into pieces
# 3. the function we want to apply to the data

# 4. we can put as the 4th element extra information
#    for instance, very commonly, we use na.rm = TRUE

# Find the average departure delay at each airport.
# Here are the airports with the largest average departure delays
tail(sort(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)))

# Here are the airports with the smallest average departure delays
head(sort(tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)))

# Here are the airports with the largest average arrival delays
tail(sort(tapply(myDF$ArrDelay, myDF$Dest, mean, na.rm=TRUE)))

# Here are the airports with the smallest average arrival delays
head(sort(tapply(myDF$ArrDelay, myDF$Dest, mean, na.rm=TRUE)))

# Which day of the week should we fly, if we want to minimize
# the expected arrival delay of the flight?
plot(tapply(myDF$ArrDelay, myDF$DayOfWeek, mean, na.rm=TRUE))
# Here 1 denotes Monday, 2 denotes Tuesday, ..., 7 denotes Sunday

# Answering the same question, but restricting attention
# to flights that have IND as the destination airport
# We get the average arrival day of the flights,
# for each day of the week, and restricting (only) to IND arrivals.
tapply(myDF$ArrDelay[myDF$Dest == "IND"],
       myDF$DayOfWeek[myDF$Dest == "IND"], mean, na.rm=TRUE)

# Just double-checking that we are working on two vectors
# that have the same lengths.
length(myDF$ArrDelay[myDF$Dest == "IND"])
length(myDF$DayOfWeek[myDF$Dest == "IND"])

# Which airlines have the best average Arrival Delays?
# Which have the worst average Arrival Delays?
tapply(myDF$ArrDelay, myDF$UniqueCarrier, mean, na.rm=TRUE)

# How many flights occur per month?
table(myDF$Month)
tapply(myDF$Month, myDF$Month, length)

# On which day of the year were the expected ArrDelays the worst?
mydates <- paste(myDF$Month, myDF$DayofMonth, myDF$Year, sep="/")

# Just double-checking that our vectors we are working with
# have the same length.
length(myDF$ArrDelay)
length(mydates)

# This is a leap year, so we get 366 values for the result.
length(tapply(myDF$ArrDelay, mydates, mean, na.rm=TRUE))

# Here are all 366 days in 2008,
# sorted according to the expected ArrDelay on that date.
sort(tapply(myDF$ArrDelay, mydates, mean, na.rm=TRUE))


# The expected ArrDelay for each date,
# but only for the flights that are arriving to IND.
sort(tapply(myDF$ArrDelay[myDF$Dest=="IND"],
            mydates[myDF$Dest=="IND"], mean, na.rm=TRUE))

# We can extend that example,
# now specify that the Dest airport is IND
# and that the Origin airport is ORD

# This is a vector that identifies the flights
# which have IND as Dest and have ORD as the Origin
# It is a vector of 7 million TRUE's and FALSE's.
ordtoind <- myDF$Dest=="IND" & myDF$Origin == "ORD"

# We find the expected arrival delay for flights
# from ORD to IND, categorized according to the date.
sort(tapply(myDF$ArrDelay[ordtoind],
            mydates[ordtoind], mean, na.rm=TRUE))


# Quiz 7
# When considering all flights to an airport, take an average of the distances (in miles) of the flights 
# to that airport. Suppose that we do such an analysis of all airports. Which airport has the 
# longest average distance of flights (in miles) arriving to that airport?

tail(sort(tapply(myDF$Distance, myDF$Dest, mean)))

# Quiz 8
# Which airline has the worst average departure delay?
tail(sort(tapply(myDF$DepDelay, myDF$UniqueCarrier, mean, na.rm=TRUE)))

# Quiz 9
# Question 1
# On which day of the year were the average departure delays the worst?
sort(tapply(myDF$DepDelay, mydates, mean, na.rm=TRUE))

# Question 2
# On which day of the year were the average departure times the worst for flights departing from O'Hare (ORD)?
sort(tapply(myDF$DepDelay[myDF$Origin =="ORD"],
            mydates[myDF$Origin =="ORD"], mean, na.rm=TRUE))


# Quiz 10
# Consider flights that departed from ATL and landed at LAX in 2008. 
# For how many days of the year were the average departure delays more than 90 minutes?

ord <- myDF$Dest=="LAX" & myDF$Origin == "ATL"

# Here I can see that only the last one accomplish > 90 minues
sort(tapply(myDF$DepDelay[ord],
            mydates[ord], mean, na.rm=TRUE))

# Here I calculate the total number
sum(tapply(myDF$DepDelay[ord],mydates[ord],mean,na.rm=TRUE) > 90)
