#######################################################################################################

################## 0. LOADING DATASETS FROM PREVIOUS SECTION ##############################

viols <- read.csv("./data/clean/viols.csv")
crimes <- read.csv("./data/clean/crimes.csv")
permits <- read.csv("./data/clean/permits.csv")
dcalls <- read.csv("./data/clean/dcalls.csv")

################## 1. BUILDING ID FOR LOCATIONS #####################
# This is the 1st assignment
# We want to build an index for buildings, based on their gps location (lng,lat)
# Let's take epsilon = 0.0001 and from 4 datasets, we construct a building index as follows
# For each (lng1, lat1) and (lng2, lat2) we say they have the same building index 
# if |lng1 - lng2|, |lat1 - lat2| < epsilon
# with epsilon = 0.0001, the Harvisine distance between two buildings are about 10 m, which is 
# acceptable in reality.

# After step 0, each data set has lat and lng fields
lats <- c(viols$lat, crimes$lat, dcalls$lat, permits$lat)
lngs <- c(viols$lng, crimes$lng, dcalls$lng, permits$lng)
bats <- data.frame(lat = round(lats, digits =4),
                   lng = round(lngs, digits =4))
## dim(bats)[[1]] == dim(viols)[[1]] + dim(crimes)[[1]] + dim(dcalls)[[1]] + dim(permits)[[1]]

## Now we have a data set of all locations, now we want to see the distribution of these
## we can use the count methods in plyr package

library(plyr)
bats.freqs <- count(bats)
bats.freqs <- bats.freqs[order(-bats.freqs$freq),]
head(bats.freqs)

## Unique buildings
batsID <- unique(bats)
# we exclude NA buildings
batsID <- batsID[!is.na(batsID$lat),]
head(batsID)
# Attach id variable 
batsID$id <- 1:dim(batsID)[[1]]
head(batsID)
tail(batsID)

######################## 2. ATTACHING IDs from batsID to each building ############################
# create the id columns for each datasets
# Now we attach these id values into the 4 datasets


############# First, create the rounded variables for lat and lng in each data sets
viols$latR <- round(viols$lat, digits=4)
viols$lngR <- round(viols$lng, digits=4)
crimes$latR <- round(crimes$lat, digits=4)
crimes$lngR <- round(crimes$lng, digits=4)
dcalls$latR <- round(dcalls$lat, digits=4)
dcalls$lngR <- round(dcalls$lng, digits=4)
permits$latR <- round(permits$lat, digits=4)
permits$lngR <- round(permits$lng, digits=4)

############## Second, Run function_helpers to have function findIds

viols$id <- as.numeric(apply(matrix(c(viols$latR, viols$lngR), ncol=2), 1, findIds, batsID)) 
crimes$id <- as.numeric(apply(matrix(c(crimes$latR, crimes$lngR), ncol=2), 1, findIds, batsID))
dcalls$id <- as.numeric(apply(matrix(c(dcalls$latR, dcalls$lngR), ncol=2), 1, findIds, batsID))
permits$id <- as.numeric(apply(matrix(c(permits$latR, permits$lngR), ncol=2), 1, findIds, batsID))

###################################### 5. WRITING OUT THE DATASETS TO DISK #######################

write.csv(viols, "./data/clean/viols.csv")
write.csv(crimes, "./data/clean/crimes.csv")
write.csv(dcalls, "./data/clean/dcalls.csv")
write.csv(permits, "./data/clean/permits.csv")
write.csv(batsID,"./data/clean/batsID.csv")

###################################################################################################