### Set-up
library(httr)

### Make variables for API Key and GET flexibility
apipassword <- "yTTZFL3TzrNAOE81ZdDmDihMXnpx5JUvrn2ntbvd"
base.url <- "https://api.propublica.org/congress/v1/"
endpoint.recent <- "/votes/recent.json"

### Build a function that pulls the data about members like: 
###   (cont'd): DOB, Gender, Social Media Ids, committee and sub-committee assignments by congress

get.member <- function(member_id) {
  
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
    
    chamber[i] <- path$chamber
    state[i] <- path$state
    district[i] <- path$district
    seniority[i] <- path$seniority
    dwnom[i] <- path$dw_nominate
    total.votes[i] <- path$total_votes
    missed.votes[i] <- path$missed_votes
    missed.pct[i] <- path$missed_votes_pct
    sponsored[i] <- path$bills_sponsored
    cosponsored[i] <- path$bills_cosponsored
    with.party[i] <- path$votes_with_party
    against.party[i] <- path$votes_against_party
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
  
  member.table <- data.frame(congress, chamber, state, district, leadership, dwnom, total.votes, missed.votes, missed.pct, sponsored, cosponsored, against.party, with.party)
                             ###, state, district, leadership, seniority, dwnom, total.votes, missed.votes, missed_pct, sponsored, cosponsored, against.party, with.party )
  
  return(member.table)
}
 