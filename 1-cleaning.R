# This is the first file to run, which serves as exploration and cleaning data sets
# This includes gps parsing and prepare the data for analytics in latter sections

####################### 0. GLOBAL CONFIGURATIONs ##################################

options(digits = 8)
epsilon <- 0.0001

######################### 1. Violations data sets ###############################

viols <- read.csv("./data/Blight_Violations.csv")
# Run function_helpers.R file first
latlngs <- gpsParsing(viols$ViolationLocation)
viols$lat <- latlngs$lat
viols$lng <- latlngs$lng
viols$FineAmt <- as.numeric(gsub("\\$","", as.character(viols$FineAmt)), na.rm=FALSE)
viols$FineAmt[is.na(viols$FineAmt)] <- 0
viols$AdminFee <- as.numeric(gsub("\\$","", as.character(viols$AdminFee)), na.rm=FALSE)
viols$AdminFee[is.na(viols$AdminFee)] <- 0
viols$StateFee <- as.numeric(gsub("\\$","", as.character(viols$StateFee)), na.rm=FALSE)
viols$StateFee[is.na(viols$StateFee)] <- 0
viols$LateFee <- as.numeric(gsub("\\$","", as.character(viols$LateFee)), na.rm=FALSE)
viols$LateFee[is.na(viols$LateFee)] <- 0
viols$CleanUpCost <- as.numeric(gsub("\\$","", as.character(viols$CleanUpCost)), na.rm=FALSE)
viols$CleanUpCost[is.na(viols$CleanUpCost)] <- 0
viols$LienFilingFee <- as.numeric(gsub("\\$","", as.character(viols$LienFilingFee)), na.rm=FALSE)
viols$LienFilingFee[is.na(viols$LienFilingFee)] <- 0
viols$JudgmentAmt <- as.numeric(gsub("\\$","", as.character(viols$JudgmentAmt)), na.rm=FALSE)
viols$JudgmentAmt[is.na(viols$JudgmentAmt)] <- 0
viols$totalfee <- viols$FineAmt + viols$AdminFee + viols$StateFee + viols$LateFee + viols$CleanUpCost
                     + viols$LienFilingFee + viols$JudgmentAmt

######################### 2. Detroit 311 calls #########################

dcalls <- read.csv("./data/detroit-311.csv")

######################### 3. Detroit Crimes Data #######################

crimes <- read.csv("./data/detroit-crime.csv")
ncrimes <- names(crimes)
ncrimes <- tolower(ncrimes)
ncrimes[15:16] <- c("lng","lat")
names(crimes) <- ncrimes
rm(ncrimes)

######################## 4. Detroit Demolition permits ########################

permits <- read.csv("./data/detroit-demolition-permits.tsv", sep ="\t")
p.latlngs <- gpsParsing(permits$site_location)
permits$lat <- p.latlngs$lat
permits$lng <- p.latlngs$lng
head(permits)

######################## 5. SAVING THE DATASETS ################################

write.csv(viols, "./data/clean/viols.csv", row.names = FALSE)
write.csv(crimes, "./data/clean/crimes.csv", row.names = FALSE)
write.csv(dcalls, "./data/clean/dcalls.csv", row.names = FALSE)
write.csv(permits, "./data/clean/permits.csv", row.names = FALSE)

####################### R. Restore default options ###########################
options(digits = 7)
