### Build a function that pulls the data about members like: 
###   (cont'd): DOB, Gender, Social Media Ids, committee and sub-committee assignments by congress
get.member <- function(member_id) {
  
### Set-up
  require(httr)
  
### Make variables for API Key and GET flexibility
  apipassword <- "yTTZFL3TzrNAOE81ZdDmDihMXnpx5JUvrn2ntbvd"
  base.url <- "https://api.propublica.org/congress/v1/"
  endpoint.recent <- "/votes/recent.json"
  
  raw.member <- GET(url = paste0(base.url, "members/", member_id, ".json"), add_headers('X-API-Key' = apipassword))
  raw.member <- jsonlite::fromJSON(content(raw.member, "text"), simplifyVector = FALSE)
  
  for (i in 1:length(raw.member$results[[1]]$roles)) {
    names(raw.member$results[[1]]$roles)[[i]] <- raw.member$results[[1]]$roles[[i]]$congress
  }
  
### Establish empty arrays for the data frame inputs and assign variables to interesting data points
  
  first <- raw.member$results[[1]]$first_name
  last <- raw.member$results[[1]]$last_name
  gender <- raw.member$results[[1]]$gender
  current.party <- raw.member$results[[1]]$current_party
  govtrack <- raw.member$results[[1]]$govtrack_id
  cspan <- raw.member$results[[1]]$cspan_id
  votesmart <- raw.member$results[[1]]$votesmart_id
  icpsr <- raw.member$results[[1]]$icpsr_id
  recent <- raw.member$results[[1]]$most_recent_vote
  last.update <- raw.member$results[[1]]$last_updated
  congress <- names(raw.member$results[[1]]$roles)
  
  chamber <- c()
  state <- c()
  district <- c()
  leadership <- c()
  seniority <- c()
  dwnom <- c()
  total.votes <- c()
  missed.votes <- c()
  sponsored <- c()
  cosponsored <- c()
  missed.pct <- c()
  with.party <- c()
  against.party <- c()
  
### Load arrays with data from each vote      
  for (i in seq(1, length(congress), by = 1)) {
    
    path <- raw.member$results[[1]]$roles[i][[1]]
    
### Congressional Chamber    
    if (length(path$chamber) == 0) {
      chamber[i] <- "NULL"
    } else {
      chamber[i] <- path$chamber
    }
    
### State    
    if (length(path$state) == 0) {
      state[i] <- "NULL"
    } else {
      state[i] <- path$state
    }
    
### Federal State District Number
    if (length(path$district) == 0) {
      district[i] <- "NULL"
    } else {
      district[i] <- path$district
    }
    
### Seniority Position 
    if (length(path$seniority) == 0) {
      seniority[i] <- "NULL"
    } else {
      seniority[i] <- path$seniority
    }
    
### DW_NOMINATE Score    
    if (length(path$dw_nominate) == 0) {
      dwnom[i] <- "NULL"
    } else {
      dwnom[i] <- path$dw_nominate
    }
    
### Number of Total Votes
    if (length(path$total_votes) == 0) {
      total.votes[i] <- "NULL"
    } else {
      total.votes[i] <- path$total_votes
    }
    
### Number of Missed Votes
    if (length(path$missed_votes) == 0) {
      missed.votes[i] <- "NULL"
    } else {
      missed.votes[i] <- path$missed_votes
    }
    
### Percentage of Votes Missed
    if (length(path$missed_votes_pct) == 0) {
      missed.pct[i] <- "NULL"
    } else {
      missed.pct[i] <- path$missed_votes_pct
    }
    
### Number of Bills Sponsored    
    if (length(path$bills_sponsored) == 0) {
      sponsored[i] <- "NULL"
    } else {
      sponsored[i] <- path$bills_sponsored
    }
    
### Number of Bills Cosponsored       
    if (length(path$bills_cosponsored) == 0) {
      cosponsored[i] <- "NULL"
    } else {
      cosponsored[i] <- path$bills_cosponsored
    }
    
### Percent of Votes Cast With the Party    
    if (length(path$votes_with_party) == 0) {
      with.party[i] <- "NULL"
    } else {
      with.party[i] <- path$votes_with_party
    }
    
### Percent of Votes Cast Against the Party    
    if (length(path$votes_against_party) == 0) {
      against.party[i] <- "NULL"
    } else {
      against.party[i] <- path$votes_against_party
    }
    
### Leadership Role    
    if (length(path$leadership_role) == 0) {
      leadership[i] <- "NULL"
    } else {
      leadership[i] <- path$leadership_role
    }
  }
  
### Print interesting data points  
  print(paste0("Name: ", first," ", last, " (", current.party, ")", sep = ""))
  print(paste("Gender:", gender))
  print(paste("ID Numbers: ICPSR:", icpsr, "GovTrack:", govtrack, "CSPAN:", cspan, "VoteSmart:", votesmart))
  print(paste("Most Recent Vote:", recent))
  print(paste("Last Update:", last.update))
  
### Build list of objects to be returned  
  member.table <- data.frame(congress, chamber, state, district, leadership, dwnom, total.votes, missed.votes, missed.pct, sponsored, cosponsored, against.party, with.party)
  
  member.data <- list(firt_name = first, last_name = last, party = current.party, gender = gender,
                            id.icpsr = icpsr, id.govtrack = govtrack, id.cspan = cspan, 
                            id.votesmart = votesmart, last.vote = recent, last.update = last.update)
  class(member.data) <- "member.data"
  
### Create class "get.rollcall" in order to structure the returned ovject to my liking  
  value <- list(background = member.data, summary = member.table)
  attr(value, "class") <- "get.member"

  return(value)
}
 