# yerp

# Pro Publica Raw Data User Functions

# Get a Specific Roll Call Vote
exam1_vote <- get.rollcall(congress = 116, chamber = "senate", session = 2, roll_call = 80)

# Get detailed data about a Specific Legislator
exam2_member <- get.member("B001277")

# Get 20 most recent votes either in the Senate, House, or combined
exam3_recent <- recent.votes("both")
exam4_recent <- recent.votes("senate")
exam5_recent <- recent.votes("house")

# Get voting record of a Specific Legislator and the results of those votes
exam6_member <- member.record("B001277")

# Get list of legilsators by congress and chamber
exam7_congress <- member.list(congress = 116, chamber = "senate")