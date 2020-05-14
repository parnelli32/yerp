# yerp

# Install packages
install.packages("httr")
install.packages("wnominate")
install.packages("pscl")

# Pro Publica Raw Data User Functions
source("ProPublica Functions/Members by Congress Function.R")
source("ProPublica Functions/Recent Member Positions Function.R")
source("ProPublica Functions/Recent Votes Function.R")
source("ProPublica Functions/Rollcalls by Congress.R")
source("ProPublica Functions/Specific Member Function.R")
source("ProPublica Functions/Specific Roll Call Vote Function.R")
source("ProPublica Functions/2nd Level Functions/Map Ideological Positions Function.R")

# Get a Specific Roll Call Vote
exam1_vote <- get.rollcall(congress = 116, chamber = "senate", session = 2, roll_call = 80)

# Get detailed data about a Specific Legislator
exam2_member <- get.member("B001277")

# Get 20 most recent votes either in the Senate, House, or combined
exam3_recent <- recent.votes("both")
exam4_recent <- recent.votes("senate")
exam5_recent <- recent.votes("house")

# Get the 20 most recent votes by a Specific Legislator and the results of those votes
exam6_member <- member.record("B001277")

# Get list of legislators by congress and chamber
exam7_congress <- member.list(congress = 116, chamber = "senate")

# Get list of rollcalls by congress
exam8_congress <- rc.record(congress = 116, chamber = "senate")

# Spatially Map Legislator Ideological Positions
exam9_pinpoint <- elite.map(congress = 115, chamber = "senate", dimes = 4)
plot(exam9_pinpoint$wnom)
