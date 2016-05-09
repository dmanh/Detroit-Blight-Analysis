######################### Generating random detroit point ################
library(sp)
det_polygon <- read.csv("./data/clean/detroit_multipolygon.csv")
det_sample <- spsample(Polygon(det_polygon), n=2*ns, type = "random")
det_sample <- as.data.frame(det_sample)
head(det_sample)
names(det_sample) <- c("lng","lat")
det_sample$lngR <- round(det_sample$lng, digits=4)
det_sample$latR <- round(det_sample$lat, digits=4)

## Creating id for det_sample points, if they are already in the batsID database
## we keep that id intact, otherwise we augment the id in batsID. 
## The idea is to keep every points involved a unique ID
det_sample$id <- as.numeric(apply(matrix(c(det_sample$latR, det_sample$lngR), ncol=2), 1, findIds, batsID),
                            na.rm = FALSE) 
det_sample[which(is.na(det_sample$id)), "id"] <- (max(batsID$id) +1):(max(batsID$id)+
                                                                     sum(is.na(det_sample$id)))

## we can visualize the data to see they are really inside Detroit city
ggmap(detroitmap) + geom_point(aes(x=lng,y=lat),data=det_sample, 
                               color = I("red"))
## Now we extract dismantled locations 
blighted <- permits[,c("latR","lngR")]
blighted <- unique(blighted)
head(blighted)
blighted$id <- as.numeric(apply(as.matrix(blighted), 1, findIds, batsID))
blighted$blight <- TRUE
## Now we label variable blight to det_sample
det_sample$blight <- FALSE
det_sample[which(det_sample$id %in% blighted$id),"blight"] <- TRUE

## Now we want to construct the dataset for analytics part
## a balanced dataset, we use dplyr::sample_n
library(dplyr)
df_false <- sample_n(det_sample[which(!det_sample$blight),], nrow(blighted))
df_false$lng <- NULL
df_false$lat <- NULL
df_false <- df_false[c("latR","lngR","id","blight")]
head(df_false)
head(blighted)

data <- rbind(df_false,blighted)
data[sample(1:nrow(data),5),]
write.csv(data, "./data/clean/data.csv", row.names = FALSE)
