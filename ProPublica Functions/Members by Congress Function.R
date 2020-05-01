### Build a function that pulls member records by Congress and Chamber
member.list <- function(congress, chamber) {
  
### Set-up
  require(httr)
  
### Make variables for API Key and GET flexibility
  apipassword <- "yTTZFL3TzrNAOE81ZdDmDihMXnpx5JUvrn2ntbvd"
  base.url <- "https://api.propublica.org/congress/v1/"
  endpoint.recent <- "/votes/recent.json"
  
  
  raw.list <- GET(url = paste0(base.url, as.character(congress), "/", chamber, "/members.json"), add_headers('X-API-Key' = apipassword))
  raw.list <- jsonlite::fromJSON(content(raw.list, "text"), simplifyVector = FALSE)
  
  for (i in 1:length(raw.list$results[[1]]$members)) {
    names(raw.list$results[[1]]$members)[[i]] <- raw.list$results[[1]]$members[[i]]$last_name
  }
  
  ###   ID: Returns the member id of the legislator   
  id <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$id) == 0) {
      id[i] <- "NULL"
    } else {
      id[i] <- relevantmember$id
    }
  }
  
  ###   First_Name: Returns the first name of the legislator   
  first_name <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$first_name) == 0) {
      first_name[i] <- "NULL"
    } else {
      first_name[i] <- relevantmember$first_name
    }
  }
  
  ###   Last_Name: Returns the last name of the legislator   
  last_name <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$last_name) == 0) {
      last_name[i] <- "NULL"
    } else {
      last_name[i] <- relevantmember$last_name
    }
  }
  
  ###   Chamber: Returns the chamber of the legislator   
  chamber.name <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    chamber.name[i] <- raw.list$results[[1]]$chamber
    
  }
  
  ###   Party: Returns the party of the legislator   
  party <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$party) == 0) {
      party[i] <- "NULL"
    } else {
      party[i] <- relevantmember$party
    }
  }
  
  ###   Gender: Returns the gender of the legislator   
  gender <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$gender) == 0) {
      gender[i] <- "NULL"
    } else {
      gender[i] <- relevantmember$gender
    }
  }
  
  ###   DW_NOMINATE: Returns the dw_nominate score of the legislator   
  dwnom <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions
    if (length(relevantmember$dw_nominate) == 0) {
      dwnom[i] <- "NULL"
    } else {
      dwnom[i] <- relevantmember$dw_nominate ###### The dwnominate command/package is not allowing me to call this data point
    }
  }
  
  ###   Total_Votes: Returns the total number of votes of the legislator   
  total_votes <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$total_votes) == 0) {
      total_votes[i] <- "NULL"
    } else {
      total_votes[i] <- relevantmember$total_votes
    }
  }
  
  ###   Missed_Votes: Returns the number of missed votes of the legislator   
  missed_votes <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$missed_votes) == 0) {
      missed_votes[i] <- "NULL"
    } else {
      missed_votes[i] <- relevantmember$missed_votes
    }
  } 
  
  ###   Missed_Votes_Pct: Returns the percent of missed votes by the legislator   
  missed_pct <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$missed_votes_pct) == 0) {
      missed_pct[i] <- "NULL"
    } else {
      missed_pct[i] <- relevantmember$missed_votes_pct
    }
  }
  
  ###   Votes_With_Party: Returns the percent of votes that are in favor of party majority the legislator   
  with.party <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$votes_with_party) == 0) {
      with.party[i] <- "NULL"
    } else {
      with.party[i] <- relevantmember$votes_with_party
    }
  }
  
  ###   Votes_Against_Party: Returns the percent of votes that are in favor of party majority the legislator   
  against.party <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$votes_against_party) == 0) {
      against.party[i] <- "NULL"
    } else {
      against.party[i] <- relevantmember$votes_against_party
    }
  }
  
  ###   State: Returns the state of the legislator   
  state <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$state) == 0) {
      state[i] <- "NULL"
    } else {
      state[i] <- relevantmember$state
    }
  }
  
  ###   Next_Eletion: Returns the next election year of the legislator   
  next_election <- c()
  for (i in seq(1, length(raw.list$results[[1]]$members), by = 1)) {
    
    ###       Segented the index call in order to localize each vote   
    relevantmember <- raw.list$results[[1]]$members[i][[1]]                      
    
    ###       Returns Vote Descriptions   
    if (length(relevantmember$next_election) == 0) {
      next_election[i] <- "NULL"
    } else {
      next_election[i] <- relevantmember$next_election
    }
  }
  
  print(paste(length(id), length(chamber.name), length(party), length(state), length(last_name), length(first_name), length(gender), length(next_election), length(dwnom), length(total_votes), length(missed_votes), length(missed_pct), length(with.party), length(against.party)))
  ### Build data frame from the chosen variables and assign it to the global variables   
  member.congress.table <- data.frame(id, chamber.name, party, state, last_name, first_name, gender, next_election, dwnom, total_votes, missed_votes, missed_pct, with.party, against.party)
  
  print(paste0("Chamber: ", chamber, "-", congress, " ; Total Members: ", raw.list$results[[1]]$num_results))
  
  return(member.congress.table)
  
}
