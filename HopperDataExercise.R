# Hopper data exercise
# Joe Silverstein
# 5-15-16

library("boot")
library("lubridate")

df = read.csv("BOS_CUN_trips1M.csv")

# Note that there are some additional variables in the dataset that aren't listed in the data dictionary.
# read.csv didn't add any new variables
length(unique(df$messageid)) # Each trip bundle contains a lot of trips.
table(df$source)
table(df$merchant)
table(df$currency) # no need to convert prices
table(df$origin) # can remove this
table(df$destination) # can remove this
table(df$cabinclass)
table(df$availableseats)
table(df$paxtype) # Don't know what this is, but it doesn't matter becauset it's always "ADT"
class(df$departure)
class(df$departure_odate) # Date of departure as factor
class(df$departure_dow)
table(df$departure_dow) # don't know what this is, so drop.
table(df$includes_sns)
table(df$refundable)

# Since I assume the customer only cares about price, only consider economy-class seats for now.
# I don't know what it means when availableseats==-1, so I'm dropping those obs.
# Only consider non-refundable tickets in this analysis.
dfSmall = subset(df, cabinclass == 'E' & availableseats >= 1 & refundable == "False")

# Create new dataframe without unnecessary variables
# Throughout my analysis, I assume that total includes tax and surcharge.
drops = c("tripindex", "received", "currency", "tax", "surcharge", "source", "origin", 
           "destination", "return", "los2", "returndurationminutes",
           "returnstops", "cabinclass", "paxtype", "refundable", "receiveddate", "departure_dow",
           "return_dow", "return_ddate", "received_odate", "includes_sns")
dfSmall = dfSmall[ , !(names(dfSmall) %in% drops)]

## Calculate difference (in days) between query date and departure date for each observation

# Convert to date class
dfSmall$queryDate = as.Date(as.POSIXct(dfSmall$triptimestamp, origin="1970-01-01"))
dfSmall$departureDate = as.Date(as.POSIXct(dfSmall$departure, origin="1970-01-01")) 

# matches departure_odate, so we're good
class(dfSmall$departureDate)

# Remove other unecessary variables
dfSmall$departure_odate = NULL

# Calculate difference in days
dfSmall$daysInAdvance = dfSmall$departureDate - dfSmall$queryDate

# Calculate difference in epoch-seconds
# This should be used instead of days, since it looks like flights change price during the day (especially on the last day)
dfSmall$secondsInAdvance = dfSmall$departure - dfSmall$triptimestamp

# Drop exact query and departure dates, since they're unnecessary for this analysis
# dfSmall$queryDate = NULL
# dfSmall$departureDate = NULL

summary(dfSmall$queryDate)
# Not a wide enough timespan of queries to be able to determine the best time of the year to travel.
# Instead, focus on how far in advance to book when booking in this time period (September or October).

# Remove duplicate trips queried at the same date across bundles
# Since there is no unique flight identifier in the dataset, will need to construct it from other variables.
# Assume that if two obs share common total, departure, and outbounddurationminutes, then they are the same flight.
# I want to drop all duplicate flights searched on the same day. That is, all obs with the same values of
# all the variables listed below:
dfUnique = unique(dfSmall[c("secondsInAdvance", "majorcarrierid", "departure", "outbounddurationminutes", "availableseats", 
                            "outboundstops", "merchant", "total")])

# sort by the above variables, just to make sure it worked
dfUnique = dfUnique[order(dfUnique$secondsInAdvance, dfUnique$majorcarrierid, dfUnique$departure, dfUnique$outbounddurationminutes, 
                          dfUnique$availableseats, dfUnique$outboundstops, dfUnique$merchant, dfUnique$total), ]

# It looks like different people are getting quoted different amounts for the same flight at the exact same time and merchant of the query.
# I've used all the available information to construct a unique flight identifier, so I'll have to leave this unexplained.
# Maybe merchants quote different prices to different people at the same time based on other information that isn't in this dataset.

# In this exercise, I assume the consumer only cares about the price 
# (and not trip duration, number of stops, date of travel, refundability, etc.). This could be fixed later, 
# but I'm not going to do it here.

# Strategy: First, find the lowest-priced flight in this dataset and figure out how far ahead of the departure it is.
# Then, do the same on a bunch of bootstrap samples and plot the distribution of how far ahead of departure it is to
# make sure this number is stable.

summary(dfUnique$total)
minPriceObs = dfUnique[dfUnique$total == min(dfUnique$total), ]
minPriceObs$departureDate = as.Date(as.POSIXct(minPriceObs$departure, origin="1970-01-01"))
# The cheapest flight is on 11-11-13. Looking online, this was a Monday. If November 11 is on the weekend instead,
# may want to wait until the following Monday to book.

# In this case there is one uniquely-cheapest flight, but there were 3 queries (conducted at close but different times) that returned the price.
# When this is the case, I'll just select one of the observations at random and use its value for secondsInAdvance in this analysis.
optimalSecondsInAdvance = minPriceObs[sample(nrow(minPriceObs), 1), "secondsInAdvance"]

bestTimeToBook = minPriceObs$departure[1] - optimalSecondsInAdvance
bestDateToBook = as.Date(as.POSIXct(bestTimeToBook, origin = "1970-01-01"))
bestDateToBook

# Keep only essential variables to make bootstrapping run faster
dfUniqueSmall = dfUnique[, c("secondsInAdvance", "total")]

## Now do the same in the bootstrap world to obtain a 95% CI

# function to obtain optimalSecondsInAdvance from the data 
howFarInAdvanceToBuy = function(data, indices) {
  d = data[indices, ] # allows boot to select sample 
  bootMinPriceObs = d[d$total == min(d$total), ]
  return(bootMinPriceObs[sample(nrow(bootMinPriceObs), 1), "secondsInAdvance"])
} 
# bootstrapping with 50 replications (don't want to do a lot more because it's slow)
startTime = Sys.time()
results = boot(data = dfUniqueSmall, statistic = howFarInAdvanceToBuy, R = 50)
endTime = Sys.time()
timeTaken = endTime - startTime
timeTaken

# view results
results 
plot(results)

# get 95% confidence interval using normal bootstrap CI
bootstrapCI = boot.ci(results, type="norm") # This could be improved by using studentized bootstrap instead
bootstrapCI

## Conclusion: Based on the data that I have, the optimal time to buy is very stable at the 95% confidence level.
##             If this is a representative sample (there are no structural breaks), then the prediction from this
##             will be very accurate.

# Convert number of seconds to number of hours:
lowerBound = seconds_to_period(bootstrapCI$normal[1, 2]) # lower bound
lowerBound
upperBound = seconds_to_period(bootstrapCI$normal[1, 3]) # upper bound
upperBound

# Expected price:
(expectedPrice = mean(dfUniqueSmall$total))

# Expected savings:
(expectedSavings = mean(dfUniqueSmall$total) - min(dfUniqueSmall$total))

# Min Price
(min(dfUniqueSmall$total))
