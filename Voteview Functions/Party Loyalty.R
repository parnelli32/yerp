

party.loyalty <- function(last.name) {
  
  ### Set-up
  require(devtools)
  devtools::install_github("voteview/Rvoteview")
  library(Rvoteview)
  
  library(pacman)
  p_load(tidyverse, kableExtra, scales)
  
  knitr::opts_chunk$set(echo = TRUE)
  library(tidyverse)
  library(kableExtra)
  
  
  ### Generate member data
  boomie <- member_search(last.name)
  
  ### Reading in the data
  rollcalls <- read_csv("https://voteview.com/static/data/out/rollcalls/Sall_rollcalls.csv") %>% filter(congress>60)
  members <- read_csv("https://voteview.com/static/data/out/members/Sall_members.csv") %>% filter(congress>60)
  votes <- read_csv("https://voteview.com/static/data/out/votes/Sall_votes.csv") %>% filter(congress>60)
  
  
  ### Calculate Party Loyalty
  map_vote <- c("y","y","y", "n", "n", "n", "nv", "nv", "nv")
  
  all <- votes %>% 
    left_join(members, by = c("congress", "chamber", "icpsr")) %>%
    left_join(rollcalls, by=c("congress", "chamber", "rollnumber")) %>%
    filter(party_code == boomie$party_code) %>%
    group_by(congress, rollnumber) %>%
    mutate(cast_code = map_vote[as.integer(cast_code)], 
           party_yea = sum(cast_code=='y'), 
           party_no = sum(cast_code=='n'),
           party_cast_code = ifelse(party_yea > party_no, 'y', 'n'),
           with_party = (party_cast_code == cast_code)) %>%
    filter(party_yea != party_no) %>%
    group_by(icpsr) %>%
    summarize(name = bioname[1],
              state_abbrev = state_abbrev[1],
              party_loyalty = sum(with_party)/sum(cast_code != 'nv'),
              votes_cast = sum(cast_code != 'nv'),
              min_congress = min(congress),
              max_congress = max(congress)) %>%
    filter( max_congress >= min(boomie$congress), min_congress <= max(boomie$congress)) %>%
    mutate(rank = rank(-party_loyalty)) %>%
    ungroup() %>%
    select(rank, everything(), -icpsr) %>%
    arrange(desc(party_loyalty))
  
  
  ### Isolate the members data
  pinpoint <- all %>% filter(str_detect(name, last.name))
  
  ### Organize into a table
  all %>%
    knitr::kable(col.names=c("Rank", "Member", "State", "Party loyalty", "Votes Cast", "First", "Last")) %>%
    row_spec(c(pinpoint$rank), 
             bold = T, background = "#f6fcba") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                              font_size = 10, 
                              full_width = FALSE) %>%
    kableExtra::add_header_above(c(" "=5, "Congresses Served"=2))
  
}
