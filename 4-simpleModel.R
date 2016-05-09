#########################################################################################################
# author: DManh Nguyen
# Project: Blights in Detroit
# Part 4: Training a simple mode
############################### 0. LOADING DATASETS ##############################################

viols <- read.csv("./data/clean/viols.csv")  # blight violations data
crimes <- read.csv("./data/clean/crimes.csv") # crimes violation in Detroit
permits <- read.csv("./data/clean/permits.csv") # demolition permits in Detroit
dcalls <- read.csv("./data/clean/dcalls.csv") # 311 calls in Detroit
batsID <- read.csv("./data/clean/batsID.csv") # list of all buildings involved in Detroit
dfs <- read.csv("./data/clean/dfs.csv")  # data

############################## 1. First feature ################################################
######### 1a. The number of blight incidents

library(plyr)
batsID$nbviols <- 0
viols.ids <- plyr::count(viols$id)
names(viols.ids) <- c("id","freq")
batsID$nbviols[1:(nrow(viols) -1)] <- viols.ids[c(-nrow(viols)), "freq"]

data <- plyr::join(data, viols.ids, by="id", type="left")
names(data) <- c("latR","lngR","id","blighted","nbviols")
data[which(is.na(data$nbviols)), "nbviols"] <- 0
head(data)
write.csv(data, "./data/clean/data.csv", row.names = FALSE)

######### 1b. Creating a working dataset of balanced dataset
# First, we need to create a training dataset. 

library(dplyr)  # we need to use function sample_n
ns <- table(batsID$blight)[[2]]  # number of blighted 
dfs_false <- sample_n(batsID[which(batsID$blight == FALSE),], size = ns)
dfs_true <- sample_n(batsID[which(batsID$blight == TRUE),], size = ns)
dfs <- rbind(dfs_false, dfs_true)
dfs[sample(1:nrow(dfs),5),]
write.csv(dfs, "./data/clean/dfs.csv")  # saving the dataset to harddisk

######### 1c. Training and test set
library(caret)
Train <- sample(nrow(data), size = nrow(data)*.8)
training <- data[Train,]
testing <- data[-Train,]
write.csv(training, "./data/clean/training.csv", row.names = FALSE)
write.csv(testing, "./data/clean/testing.csv", row.names = FALSE)
# we use contrasts(dfs$blight) to know how R codes TRUE and FALSE

######## 1d. Estimating the accuracy by using cross-validation
#Randomly shuffle the data
training <- training[sample(nrow(training)),]
#Create 5 equally size folds
folds <- cut(seq(1,nrow(training)),breaks=5,labels=FALSE)

########################## 2. Estimating CV error #######################################

################ 2a. CV error with glm
#Perform 5 fold cross validation
error <- 1:5
for(i in 1:5){
  #Segement your data by fold using the which() function 
  validIndexes <- which(folds==i,arr.ind=TRUE)
  validData <- training[validIndexes, ]
  trainData <- training[-validIndexes, ]
  #Use test and train data partitions however you desire
  mod_fit <- glm(blighted ~ log(1+nbviols) + log(1+neighbor_nbviols)
                 + log(1 + bd.crimes) + log(1+ other.crimes) + log(1+ nb_311) + 
                   log(1 + voisin_fee), 
                 data=trainData, family = binomial)
  mod_probs <- predict(mod_fit, newdata = validData, type="response")
  mod_preds <- rep(FALSE, length(mod_probs))
  mod_preds[mod_probs > 0.5] <- TRUE
  error[i] <- mean(mod_preds != validData$blight)
}
mean(error) ## 33.84%

############### 2b. CV error with linear discriminant analysis 
library(MASS)  # for lda function 

error <- 1:5
for(i in 1:5){
  #Segement your data by fold using the which() function 
  validIndexes <- which(folds==i,arr.ind=TRUE)
  validData <- training[validIndexes, ]
  trainData <- training[-validIndexes, ]
  #Use test and train data partitions however you desire
  mod_fit <- lda(blighted ~ log(1+nbviols) + log(1+neighbor_nbviols)
                 + log(1 + nb_crimes) + log(1+ nb_311),
                 data=trainData)
  mod_probs <- predict(mod_fit, validData)
  mod_preds <- mod_probs$class
  error[i] <- mean(mod_preds != validData$blight)
}
mean(error)  ## 47.49% error

############### 2c. CV error with quadratic discriminant analysis 
library(MASS)  # for qda function 

error <- 1:5
for(i in 1:5){
  #Segement your data by fold using the which() function 
  validIndexes <- which(folds==i,arr.ind=TRUE)
  validData <- training[validIndexes, ]
  trainData <- training[-validIndexes, ]
  #Use test and train data partitions however you desire
  mod_fit <- qda(blighted ~ log(1+nbviols) + log(1+neighbor_nbviols)
                 + log(1 + bd.crimes) + log(1+ other.crimes) + log(1+ nb_311) + 
                   log(1 + voisin_fee), 
                 data=trainData)
  mod_probs <- predict(mod_fit, validData)
  mod_preds <- mod_probs$class
  error[i] <- mean(mod_preds != validData$blight)
}
mean(error)  ## 40.01% error

############### 2d. CV error with tree 
library(tree)
data$blighted <- factor(data$blighted)
training$blighted <- factor(training$blighted)
testing$blighted <- factor(testing$blighted)
blighted.tree <- tree(blighted ~ nbviols + neighbor_nbviols
                      + nb_crimes + nb_311, data = training)
blighted.tree.pred <- predict(blighted.tree, testing, type="class")
mean(blighted.tree.pred != testing$blighted)  # error: 34%
summary(blighted.tree)  ## missclassification error rate: 33.59%
###### 2d.1. CV error with tree: this does not make much sense and is not popular

############# 2e. Conclusion
# By comparing all 4 methods, we can see that logistic regression and tree function
# pretty well with CV error about 33.6%
# we go on to calculate the test error is about 34%

###################################### 2. WRITING OUT THE DATASETS TO DISK #######################

write.csv(viols, "./data/clean/viols.csv")
write.csv(crimes, "./data/clean/crimes.csv")
write.csv(dcalls, "./data/clean/dcalls.csv")
write.csv(permits, "./data/clean/permits.csv")
write.csv(batsID,"./data/clean/batsID.csv")

########################################################################################################