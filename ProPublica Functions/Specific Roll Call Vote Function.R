### Set-up
library(httr)

### Make variables for API Key and GET flexibility
apipassword <- "yTTZFL3TzrNAOE81ZdDmDihMXnpx5JUvrn2ntbvd"
base.url <- "https://api.propublica.org/congress/v1/"
endpoint.recent <- "/votes/recent.json"

### Build a function that pulls the roll-call reuslts by member for specific votes/bills 

get.rollcall <- function(congress, chamber, session, roll_call) {
  
  raw.vote <- GET(url = paste0(base.url, congress, "/", chamber, "/sessions/", session, "/votes/", roll_call, ".json"), add_headers('X-API-Key' = apipassword))
  raw.vote <- jsonlite::fromJSON(content(raw.vote, "text"), simplifyVector = TRUE)
  raw.vote.table <- raw.vote$results$votes$vote$positions
  
  date <- raw.vote$results$votes$vote$date
  question <- raw.vote$results$votes$vote$question_text
  description <- raw.vote$results$votes$vote$description
  total.yes <- raw.vote$results$votes$vote$total$yes
  total.no <- raw.vote$results$votes$vote$total$no
  total.not_voting <- raw.vote$results$votes$vote$total$not_voting
  rep.yes <- raw.vote$results$votes$vote$republican$yes
  rep.no <- raw.vote$results$votes$vote$republican$no
  rep.not_voting <- raw.vote$results$votes$vote$republican$not_voting
  rep.position <- raw.vote$results$votes$vote$republican$majority_position
  dem.yes <- raw.vote$results$votes$vote$democratic$yes
  dem.no <- raw.vote$results$votes$vote$democratic$no
  dem.not_voting <- raw.vote$results$votes$vote$democratic$not_voting
  dem.position <- raw.vote$results$votes$vote$democratic$majority_position
  result <- raw.vote$results$votes$vote$result
  
  ###   Print additional interesting / pertinent situation  
  print(date)
  print(question)
  print(description)
  print(result)
  print(paste("Dem. Majority Position:", dem.position, ", Yes =", dem.yes, "No =", dem.no, "Not Voting =", dem.not_voting))
  print(paste("Rep. Majority Position:", rep.position, ", Yes =", rep.yes, "No =", rep.no, "Not Voting =", rep.not_voting))
  print(paste("Total Votes: Yes =", total.yes, "No =", total.no, "Not Voting =", total.not_voting))
  
  
  return(raw.vote.table)
}