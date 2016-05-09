#########################################################################################################
# author: DManh Nguyen
# Project: Blights in Detroit
# Part 3: Constructing training datasets or Assigning Blight or Blighted
############################### 0. LOADING DATASETS ##############################################

viols <- read.csv("./data/clean/viols.csv")  # blight violations data
crimes <- read.csv("./data/clean/crimes.csv") # crimes violation in Detroit
permits <- read.csv("./data/clean/permits.csv") # demolition permits in Detroit
dcalls <- read.csv("./data/clean/dcalls.csv") # 311 calls in Detroit
batsID <- read.csv("./data/clean/batsID.csv") # list of all buildings involved in Detroit

############### 1. Assigning blight to buildings with "Dismantle" type of demolition ############

batsID$blight <- FALSE
# We look for the list of all blight "id"s
blight.ids <- permits[which(permits$PERMIT_DESCRIPTION != ""),"id"]
blight.ids <- unique(blight.ids)
batsID[which(batsID$id %in% blight.ids),"blight"] <- TRUE

###################################### 2. WRITING OUT THE DATASETS TO DISK #######################

write.csv(viols, "./data/clean/viols.csv")
write.csv(crimes, "./data/clean/crimes.csv")
write.csv(dcalls, "./data/clean/dcalls.csv")
write.csv(permits, "./data/clean/permits.csv")
write.csv(batsID,"./data/clean/batsID.csv")

########################################################################################################