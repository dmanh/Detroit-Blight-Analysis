#########################################################################################################
# author: DManh Nguyen
# Project: Blights in Detroit
# Part 5: Features Engineering
############################### 0. LOADING DATASETS ##############################################

viols <- read.csv("./data/clean/viols.csv")  # blight violations data
crimes <- read.csv("./data/clean/crimes.csv") # crimes violation in Detroit
permits <- read.csv("./data/clean/permits.csv") # demolition permits in Detroit
dcalls <- read.csv("./data/clean/dcalls.csv") # 311 calls in Detroit
batsID <- read.csv("./data/clean/batsID.csv") # list of all buildings involved in Detroit
dfs <- read.csv("./data/clean/dfs.csv")  # data
training <- read.csv("./data/clean/training.csv")
testing <- read.csv("./data/clean/testing.csv")

############################## 1. nbviols in my neighborhood ###############################
## we take the batsID database and make new variable nextviols 
names(viols.ids) <- c("id","nbviols")
head(viols.ids)
nbviols_df <- plyr::count(viols, vars = c("latR","lngR"))
head(nbviols_df)
neighbor_nbviols(c(24.7207, -81.0189), df=nbviols_df)

data$neighbor_nbviols <- as.numeric(apply(as.matrix(data[,c("latR","lngR")]), 1, 
                                                           neighbor_nbviols, df=nbviols_df), na.rm=FALSE)
#################### tree model
library(tree)
Train <- sample(nrow(data), size = nrow(data)*.8)
data$blighted <- factor(data$blighted)
training <- data[Train,]
testing <- data[-Train,]

blighted.tree <- tree(blighted ~ nbviols + neighbor_nbviols, data = training)
blighted.tree.pred <- predict(blighted.tree, testing, type="class")
mean(blighted.tree.pred != testing$blighted)  # error: 33.74%
summary(blighted.tree)  ## missclassification error rate: 33.84%

########################## 2. CRIME DATA #################################################
buildings.crimes <- c("ARSON", "DAMAGE TO PROPERTY", "ENVIRONMENT","RUNAWAY")
related.crimes <- c("AGGRAVATED ASSAULT", "DRUNKENNESS", "EMBEZZLEMENT", 
                    "HOMICIDE","JUSTIFIABLE HOMICIDE","LARCENY","NEGLIGENT HOMICIDE",
                    "OTHER BURGLARY","OUIL DISPOSE OF VEHICLE TO AVOID FORFEITURE",
                    "STOLEN VEHICLE","VAGRANCY (OTHER)", "ASSAULT","BURGLARY",
                    "DANGEROUS DRUGS", "HEALTH-SAFETY", "IMMIGRATION", "KIDNAPING",
                    "LIQUOR", "WEAPONS OFFENSES", "STOLEN PROPERTY", "ROBBERY"
                    )
df <- plyr::count(crimes[which(crimes$category %in% buildings.crimes),], 
                          vars = c("latR","lngR"))
head(df)
data$bd.crimes <- as.numeric(apply(as.matrix(data[,c("latR","lngR")]), 1, 
                                   neighbors_total, df), na.rm=FALSE)

#####
df <- plyr::count(crimes[which(crimes$category %in% related.crimes),], 
                  vars = c("latR","lngR"))
head(df)
data$other.crimes <- as.numeric(apply(as.matrix(data[,c("latR","lngR")]), 1, 
                                      neighbors_total, df), na.rm=FALSE)

######################### 3. 311 calls data #################################################
exluded.types <- c("Traffic Sign Issue", "Traffic Signal Issue","Street Light Pole Down",
                   "Test (internal use only, public issue)")
df <- plyr::count(dcalls[-which(dcalls$issue_type %in% exluded.types),], vars = c("latR","lngR"))
head(df)
data$nb_311 <-  as.numeric(apply(as.matrix(data[,c("latR","lngR")]), 1, 
                                 neighbors_total, df=df), na.rm=FALSE)

#########################  4. Violation total fee of neighbors #####################################
df <- viols[,c("latR","lngR","totalfee")]
names(df) <- c("latR","lngR","freq")
data$voisin_fee <- as.numeric(apply(as.matrix(data[,c("latR","lngR")]), 1, 
                                    neighbors_total, df=df), na.rm=FALSE)
###################################### 2. WRITING OUT THE DATASETS TO DISK #######################

write.csv(viols, "./data/clean/viols.csv", row.names = FALSE)
write.csv(crimes, "./data/clean/crimes.csv", row.names = FALSE)
write.csv(dcalls, "./data/clean/dcalls.csv", row.names = FALSE)
write.csv(permits, "./data/clean/permits.csv", row.names = FALSE)
write.csv(batsID,"./data/clean/batsID.csv", row.names = FALSE)
write.csv(dfs, "./data/clean/dfs.csv", row.names = FALSE)
write.csv(data, "./data/clean/dat.csv", row.names = FALSE)

###############################################################################################