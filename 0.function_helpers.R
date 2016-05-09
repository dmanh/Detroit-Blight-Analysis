## Parsing GPS coordinates
## define regular expression pattern
gpsParsing <- function(addr, p="\\(.*\\)")
{
  r <- regexpr(p, addr)
  out <- rep(NA, length(r))
  out[r != -1] <- regmatches(addr, r)
  ## strip the \\( and \\)
  out <- gsub("[()]", "", out)
  lats <- unlist(lapply(out, function(x) as.numeric(strsplit(x, split=",")[[1]][1])))
  lngs <- unlist(lapply(out, function(x) as.numeric(strsplit(x, split=",")[[1]][2])))
  list(lat=lats, lng=lngs)
}

################### FINDING THE BUILDING ID ##############################
findIds <- function(x = c(0.0, 0.0), batsID, bycolumn="id", digits=4) 
{
  # This function take a point (lat, lng) and the aggregate building index batsID
  # as arguments and return the id in the batsID
  # Condition: the point x = (lat, lng) comes from the population of (lat, lng)
  # which is huge, so this is not valid for outsider points (lat, lng)
  # Well for outsiders or NAs, it just returns NA
  x <- round(x, digits=digits)
  return (batsID[which(batsID[,"lat"] == x[1] & 
                         batsID[,"lng"] == x[2]),"id"])
}


###################### RETURN total nbviols of neighbors  #############################
# given x = (lat, lng), return the total nbviols of its neighbors from 
# nbviols data frame df, which has 3 columns latR, lngR and nbviols or freq
neighbors_total <- function(x=c(0.0, 0.0), df)
{
  ## we define neighbor as all locations within about .01 and .01 in 
  ## longitude and lattitude distance, or about 1.3 km in Harversine distance
  epsilon <- .01
  (sum(df[which(abs(df[,"latR"] - x[1]) < epsilon &
                  abs(df[,"lngR"] - x[2]) < epsilon), "freq"]))
}


