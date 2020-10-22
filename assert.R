# assert.R
# runs a sesries of assertion checks to verify the quality of the data.
# must be run with MAIN-arl-votes-2020.R, values defined <- there.


# sum of total votes == given sum of total votes
assert_that(sum(vote.data$`Total Votes`) == totals.tib$total.voted[1])

# sum of early voted == given sum of early votes
assert_that(sum(vote.data$`Early Voted`) == totals.tib$early.voted[1]) 

# sum of total mail == given sum of total mail ballots requested 
assert_that((sum(vote.data$total.mail)+ e.totmail) == totals.tib$total.mail[1]) # 101 undercount

# sum of total counted == given sum of total counted 
assert_that(sum(vote.data$`Mail Ballots Counted`) == totals.tib$counted[1])

# sum of total mail received == given sum of received + given sum of counted 
assert_that( sum(vote.data$`Mail Received`) == (totals.tib$received[1] + totals.tib$counted[1] ) )

# sum of total outstanding mail votes == given sum of "outstanding" (or "mailed" as imported)
assert_that((sum(vote.data$`Mail Outstanding`) + e.outstanding) == totals.tib$mailed[1]) # 101 off undercount

# assert that there are 54 precincts
assert_that(nrow(vote.data) == nrow )

