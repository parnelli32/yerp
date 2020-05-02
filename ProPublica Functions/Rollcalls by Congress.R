### Build a function that returns the list of roll call votes by congress sorted by date and descending roll_call
rc.record <- function(congress, chamber) {
  
### Set-up
  require(httr)
  source(file = "ProPublica Functions/Specific Roll Call Vote Function.R")
  
### Determine the max number of roll calls for 2nd session, put them in an array, and reverse order
  n <- 1
  holder.2 <- c() 
  repeat {
    sesh.2 <- get.rollcall(as.character(congress), chamber, session = 2, roll_call = n)
    if (is.null(sesh.2$positions)) {
      break
    } else {
      holder.2[[n]] <- sesh.2
      n <- n + 1
    }
  }
  holder.2 <- rev(holder.2)

### Determine the max number of roll calls for 1st session, put them in an array, and reverse order
  m <- 1
  holder.1 <- c() 
  repeat {
    sesh.1 <- get.rollcall(as.character(congress), chamber, session = 1, roll_call = m)
    if (is.null(sesh.1$positions)) {
      break
    } else {
      holder.1[[m]] <- sesh.1
      m <- m + 1
    }
  }
  holder.1 <- rev(holder.1)
 
### Combine the two roll call lists   
  practice <- append(holder.2, holder.1, after = length(holder.2))
}

