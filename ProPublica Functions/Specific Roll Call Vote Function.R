### Build a function that pulls the roll-call reuslts by member for specific votes/bills 
get.rollcall <- function(congress, chamber, session, roll_call) {
  
### Set-up
  require(httr)
  
### Make variables for API Key and GET flexibility
  apipassword <- "yTTZFL3TzrNAOE81ZdDmDihMXnpx5JUvrn2ntbvd"
  base.url <- "https://api.propublica.org/congress/v1/"
  endpoint.recent <- "/votes/recent.json"
  
  raw.vote <- GET(url = paste0(base.url, as.character(congress), "/", chamber, "/sessions/", as.character(session), "/votes/", as.character(roll_call), ".json"), add_headers('X-API-Key' = apipassword))
  raw.vote <- jsonlite::fromJSON(content(raw.vote, "text"), simplifyVector = TRUE)
  
  
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
  
### Print additional interesting / pertinent situation
  print(paste0(date, " : ", result, " : ", question))
  
### Build list of objects to be returned  
  raw.vote.table <- raw.vote$results$votes$vote$positions 
  
  ###raw.list <- c(date, question, description, result, total.yes, total.no, total.not_voting, dem.position, dem.yes, dem.no, dem.not_voting, rep.position, rep.yes, rep.no, rep.not_voting)
  raw.list <- list(congress = congress, chamber = chamber, session = session, roll_call = roll_call, date = date, question = question, description = description, result = result, total.yes = total.yes, total.no = total.no, total.not_voting = total.not_voting, dem.position = dem.position, dem.yes = dem.yes, dem.no = dem.no, dem.not_voting = dem.not_voting, rep.position = rep.position, rep.yes = rep.yes, rep.no = rep.no, rep.not_voting = rep.not_voting)
  class(raw.list) <- "raw.list"
  
### Create class "get.rollcall" in order to structure the returned ovject to my liking  
  value <- list(vote.data = raw.list, positions = raw.vote.table)
  attr(value, "class") <- "get.rollcall"
  
  return(value)
}