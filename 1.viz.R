#####################################################################################
############################# DATA VISUALIZATION ####################################
#####################################################################################

################ 1. Getting the detroit map from googlemap for background ###########
library(ggmap)
api_key <- "AIzaSyDjcZU1-yuIm9qjzJQOHCoguGC0Yf5Sjk8"
detroitmap <- get_googlemap(center = c(lon=-83.119128,lat=42.384713),maptype = "roadmap", 
                            size=c(640,640),zoom = 11)
ggmap(detroitmap)


# Counting the frequencies of locations (lng,lat) from viols dataset
library(plyr) ## to use the count methods to count frequency
viols.latlng <- count(data.frame(lat= viols$lat, lng= viols$lng))
viols.latlng <- viols.latlng[order(-viols.latlng$freq),]
head(viols.latlng)

# Counting the frequencies of locations based of postal addresses (StreetNumber, StreetName)
viols.ploc <- count(data.frame(StreetNumber = viols$ViolationStreetNumber, 
                               StreetName = viols$ViolationStreetName))
viols.ploc <- viols.ploc[order(-viols.ploc$freq),]
head(viols.ploc)
#################### a. Viols Incidents by street ####################
viols.bystreets <- count(data.frame(streetname = viols$ViolationStreetName))
viols.bystreets <- viols.bystreets[order(-viols.bystreets$freq),]
head(viols.bystreets)

#################################  Display    ########################################
viols.plot <- ggmap(detroitmap) + geom_point(aes(x=lng,y=lat,size=freq),data=viols.latlng[2:1000,], 
                                             color = I("red"))
viols.plot

####################################################################################
############################### 3. OUTLIER ANALYSIS ###################################
# This is comments for the data set provided by the program, due to this
# kind of errors, we subsequently used the data set downloaded from the 
# Detroit OpenData portail.
# This section takes some outlier and studies them in detail
# We first take a detail in the location with the most frequent (lng, lat), with 
# 21114 occurences. 
viols.freq <- viols[which(viols$lat == viols.latlng[2,"lat"] &
                          viols$lng == viols.latlng[2, "lng"]),]
head(viols.freq)
# We can see in the ViolationAddress that for this particular (lat,lng) location
# there are several postal addresses - (streetnumber, streetname)
# We do not know why this happens, but since there are about 7% of this particular location
# in the whole observation. Treating this does affect the analysis. What should we trust 
# eventually ? Apparently, we should trust the postal address.

