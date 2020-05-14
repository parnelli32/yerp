### Spatially Map Congressional Senators and Representatives politcial ideologies, exclusive of each other, 
###   based on members' voting record. 
elite.map <- function(congress, chamber, dimes) {
  
### Set up
  source(file = "ProPublica Functions/Members by Congress Function.R")
  source(file = "ProPublica Functions/Rollcalls by Congress.R")
  require(pscl)
  require(wnominate)
  
### Pull raw votes from congressional chamber by congress, let's just call it PinPoint
  pinpoint <- rc.record(as.character(congress), chamber = chamber)
  for (i in 1:length(pinpoint)) {
    names(pinpoint)[[i]] <- paste0("rc",i)
  }
  
### Generate list of unique members who participated in PinPoint
  members.pinpoint <- member.list(as.character(congress), chamber = chamber) 
  
### Create data frame to enter into the rollcall object and add new columns to hold the new votes
  rollcall.pinpoint <- data.frame(members.pinpoint$last_name, members.pinpoint$first_name, 
                                  members.pinpoint$gender, members.pinpoint$state, 
                                  members.pinpoint$party, members.pinpoint$id)
  for (b in 1:length(names(pinpoint))) {
    rollcall.pinpoint[,b+6] <- NA                          # creates a new empty column
    boomie <- rollcall.pinpoint[[b+6]]                     # create path for the landing array that will hold vote
    route <- pinpoint[[b]]$positions                # create path for raw votes
    boomie <- route$vote_position[match(members.pinpoint$id, route$member_id)]
    boomie[is.na(boomie)] <- 0
    rollcall.pinpoint[,b+6] <- boomie                       # locate and load votes into the data frame
    rm(route,boomie)
  }
  
### Rename column names of data frame and recode data into numeric values
  colnames(rollcall.pinpoint) <- c("last.name", "first.name", "gender", 
                                   "state", "party", "member.id", names(pinpoint))
  for (a in 7:length(rollcall.pinpoint)) {
    rollcall.pinpoint[a] <- ifelse(rollcall.pinpoint[a] == "Not Voting", 9, 
                              ifelse(rollcall.pinpoint[a] == "Yes", 1, 
                              ifelse(rollcall.pinpoint[a] == "No", 5, 0)))
  }
  
### Isolate the votes from the data frame into a data matrix and then create a rollcall object (pscl package)
### using that matrix
  ###library(pscl)
  blah <- data.matrix(rollcall.pinpoint[7:length(rollcall.pinpoint)])
  rc.pinpoint <- rollcall(blah, yea = 1, nay = 5, missing = 9, notInLegis = 0, 
                       legis.names = rollcall.pinpoint$last.name, legis.data = rollcall.pinpoint[3:5])
  
### Adjust polarity length based on number of dimensions (dimes)
  left.right <- rep(1, as.numeric(dimes))
  
### Run the W-Nominate Algorithm. Print the summary of, plot, and return the result
  ###library(wnominate)
  result.pinpoint <- wnominate(rc.pinpoint, polarity = left.right, dims = as.numeric(dimes), verbose = TRUE)
  summary(result.pinpoint)
  plot(result.pinpoint)
  
### Create class "elite.map" in order to structure the wnominate and rollcall objects
  value <- list(wnom = result.pinpoint, pinpoint = rc.pinpoint)
  attr(value, "class") <- "elite.map"
  
  return(value)
}













