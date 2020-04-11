### Set-up
library(httr)

### Make variables for API Key and GET flexibility
apipassword <- "yTTZFL3TzrNAOE81ZdDmDihMXnpx5JUvrn2ntbvd"
base.url <- "https://api.propublica.org/congress/v1/"
endpoint.recent <- "/votes/recent.json"

### Build a function that lists all the votes a member has been involved in and those votes' results
member.record <- function(member_id) {
  
  raw.record <- GET(url = paste0(base.url, "members/", member_id, "/votes.json"), add_headers('X-API-Key' = apipassword))
  raw.record <- jsonlite::fromJSON(content(raw.record, "text"), simplifyVector = FALSE)
  
### Establish empty arrays for the data frame inputs and assign variables to interesting data points
  date <- c()
  chamber <- c()
  bill_id <- c()
  session <- c()
  roll_call <- c()
  description <- c()
  position <- c()
  result <- c()
  total_yes <- c()
  total_no <- c()
  not_voting <- c()
  
### Load arrays with data from each vote
  for (i in seq(1, length(raw.record$results[[1]]$votes), by = 1)) {
    
    path <- raw.record$results[[1]]$votes[i][[1]]
    
    date[i] <- path$date
    chamber[i] <- path$chamber
    bill_id[i] <- path$bill$bill_id
    session[i] <- path$session
    roll_call[i] <- path$roll_call
    description[i] <- path$description
    position[i] <- path$position
    result[i] <- path$result
    total_yes[i] <- path$total$yes
    total_no[i] <- path$total$no
    not_voting[i] <- path$total$not_voting
  }
  
  record.table <- data.frame(date, chamber, bill_id, session, roll_call, description, position, result, total_yes, total_no, not_voting)
  return(record.table)
}
