######## Builds function that returns the most recent 20 roll calls voted on in the designated chamber or chambers          

### Sets the default value as "both"
recent.votes <- function (x = getOption(x = "senate", default = "both")) {
  
### Set-up
  require(httr)
  
### Make variables for API Key and GET flexibility
  apipassword <- "yTTZFL3TzrNAOE81ZdDmDihMXnpx5JUvrn2ntbvd"
  base.url <- "https://api.propublica.org/congress/v1/"
  endpoint.recent <- "/votes/recent.json"
  
### Pulls data from the ProPublica API in JSON form and converts it into a readable format in r 
  votes.recent <- GET(url = paste0(base.url, x, endpoint.recent), add_headers('X-API-Key' = apipassword))
  votes.recent <- jsonlite::fromJSON(content(votes.recent, "text"), simplifyVector = FALSE)
  for (i in 1:length(votes.recent$results$votes)) {
    names(votes.recent$results$votes)[[i]] <- paste0("Vote",i)
  }
  vote.names <- names(votes.recent$results$votes)
  
  if (x == "both" | x == "senate" | x == "house") {
      
### Establish empty arrays for the data frame inputs     
    bill_id <- c()
    description <- c()
    chamber <- c()
    result <- c()
    roll_call <-c()
    date <- c()
    yes <- c()
    no <- c()
    not_voting <- c()
      
### Load arrays with data from each vote      
    for (i in seq(1, 20, by = 1)) {
          
      relevantvote <- votes.recent$results$votes[i][[1]]
          
      if (is.null(relevantvote$bill$bill_id)) {
        if (is.null(relevantvote$nomination$nomination_id)) {
          bill_id[i] <- "BLAAHHHHHH"
        } else {
          bill_id[i] <- relevantvote$nomination$nomination_id
        }
      } else {
        bill_id[i] <- relevantvote$bill$bill_id
      }
          
      description[i] <- relevantvote$description
      chamber[i] <- relevantvote$chamber
      result[i] <- relevantvote$result
      roll_call[i] <- relevantvote$roll_call
      date[i] <- relevantvote$date
      yes[i] <- relevantvote$total$yes
      no[i] <- relevantvote$total$no
      not_voting[i] <- relevantvote$total$not_voting
        
    }
      
### Load arrays into a data frame and return that data frame      
    recent.votes.table <- data.frame(chamber,date, bill_id, roll_call, description, result, yes, no, not_voting, row.names = vote.names)
      
    return(recent.votes.table)
    
  } else {
    
    print("You can only use either 'both', 'senate', or 'house'.  'both' is the default.")
  }
}



