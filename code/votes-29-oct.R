# ---  --- --- --- --- --- --- --- --- --- --- --- --- #
# votes-29-oct.R
# imports and processes aggregated vote info from Arlington for 29th october
# ---  --- --- --- --- --- --- --- --- --- --- --- --- #


# load + process voting data file ---- 
# page info 
raw.info <- pdf_info(file.path(votes, "10-29-2020.pdf"))

# import both pages
raw1 <- pdf_text(file.path(votes, "10-29-2020.pdf"))

# page 2
raw2 <- raw1[[2]]
pg2.split <- str_split(raw2, "\\n", simplify = TRUE) %>% as.vector()



# make locality totals
totals.tib <- tibble(RawText=pg2.split) %>%
  filter(row_number() == totalsrowv2) %>%
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)", "\\.")) %>%
  separate(TextSquish, into=c('Totals', 'Rest'), sep = ":",
           extra="merge") %>%
  mutate(Rest=str_squish(Rest),
         Rest=str_replace_all(Rest, "\\(X\\)", "NA")) %>%
  separate(Rest,
           into=c('mailed', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('received', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('counted', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('total.mail', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('early.voted', 'total.voted'), 
           sep = "[:space:]",
           extra='merge') %>%
  convert(num(mailed, received, counted, total.mail, early.voted, total.voted))



pg2.tib <- tibble(RawText=pg2.split) %>% 
  filter(row_number() <= max2v2) %>%
  filter(row_number() >= min2) %>%
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)", "\\.")) %>%
  separate(TextSquish, into=c('precinct', 'Rest'), sep = "-",
           extra="merge") %>%
  mutate(Rest=str_squish(Rest),
         Rest=str_replace_all(Rest, "\\(X\\)", "NA")) %>%
  separate(Rest,
           into=c('prec_name', 'Rest'), 
           sep = "(?<=[:alpha:])[:space:](?=[:digit:])",
           extra='merge') %>%
  separate(Rest,
           into=c('mailed', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('received', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('counted', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('total.mail', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('early.voted', 'total.voted'), 
           sep = "[:space:]",
           extra='merge')  


# page 1
raw1 <- raw1[[1]] # take only first page
pg1.split <- str_split(raw1, "\\n", simplify = TRUE) %>% as.vector()

pg1.tib <- tibble(RawText=pg1.split) %>%
  filter(row_number() <= max1v2) %>%
  filter(row_number() >= min1) %>%
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)", "\\.")) %>%
  separate(TextSquish, into=c('precinct', 'Rest'), sep = "-",
           extra="merge") %>%
  mutate(Rest=str_squish(Rest),
         Rest=str_replace_all(Rest, "\\(X\\)", "NA")) %>%
  separate(Rest,
           into=c('prec_name', 'Rest'), 
           sep = "(?<=[:alpha:])[:space:](?=[:digit:])",
           extra='merge') %>%
  separate(Rest,
           into=c('mailed', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('received', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('counted', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('total.mail', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('early.voted', 'total.voted'), 
           sep = "[:space:]",
           extra='merge') 


# append + rename variables
vote29 <- bind_rows(pg1.tib, pg2.tib) %>%
  select(-RawText) %>%
  mutate(
    prec_name = str_to_title(prec_name)
  ) %>%
  convert(num(received, counted)) %>%
  mutate(
    mail.received = received + counted,
    mail.outstanding = mailed
  )







# merge with voter data with sf and registration data  ---- 
# if using precinct gis data, import 
if (s.gis == 1) {
  arl.precinct <- readRDS(file = file.path(root.data, "precinct-gis.Rda"))
}


# convert columns to numeric en masse

## make lists of vars

### voter data 
numvars1 <- c("precinct", "total.voted", "mailed", "counted", "total.mail",
              "mail.outstanding", "mail.received", "early.voted")
fctvars1 <- c('prec_name')

### precinct data 
numvars2 <- c("objectid", "precinct", "senate")
fctvars2 <- c("prec_name", "label")

### registration data 
numvars3 <- c("precinct", "active", "inactive", "all", "military", "overseas", "federal")
fctvars3 <- c("prec_name")


## make changes...
vote29 <- 
  vote29 %>%
  convert(num(numvars1),
          fct(fctvars1))

if (s.gis == 1) {
  arl.precinct <- 
    arl.precinct %>%
    convert(num(numvars2),
            fct(fctvars2))
}



# Merge
vote.data29 <-
  left_join(vote29, vote.register,
            by = "precinct",
            suffix = c('.x', '.y')) %>%
  mutate(
    prec_name = coalesce(prec_name.x, prec_name.y) 
  ) %>%
  select(-prec_name.x, -prec_name.y)

if (s.gis == 1) {
  vote.data29 <- vote.data29 %>%
    left_join(., arl.precinct,
              by = "precinct",
              suffix = c('.x', '.y')
    )%>%
    mutate(
      prec_name = coalesce(prec_name.y, prec_name.x) # take vote.register names as it has house infused
    ) %>%
    select(precinct, prec_name, everything(), 
           -prec_name.x, -prec_name.y) %>%
    st_as_sf()
}








# quick data mutations 
vote.data29 <-
  vote.data29 %>%
  mutate(
    prec.tot.votes  = total.voted,
    prec.outstand.votes = all - prec.tot.votes,
    outstand.votes.pct  = round( 100 * (prec.outstand.votes/sum(prec.outstand.votes)), 1),
    mail.request    = total.mail,
    prec.tot.votes.pct  = round( 100 * (prec.tot.votes/sum(prec.tot.votes)), 1),
    mail.return.pct = round( (mail.received/mail.request), 2),
    early.voted.pct = round( 100 * (early.voted / sum(early.voted)), 1),
    mail.outstanding.pct = round( 100 * (mail.outstanding / sum(mail.outstanding)), 1),
    mail.received.pct = round( 100 * (mail.received / sum(mail.received)), 1),
    mail.early.ratio  = round( (early.voted / mail.received ), 1),
    prec.cur.turnout  = round( 100 * (prec.tot.votes/all), 1),
    prec.act.turnout  = round( 100 * (prec.tot.votes/active), 1),
    prec.mail.turnout = round( 100 *(mail.received/active), 1),
    prec.early.turnout= round( 100 * (early.voted/active), 1),
    military.pct    = round( (military/sum(active)), 3),
    overseas.pct    = round( (overseas/sum(active)), 3),
    federal.pct     = round( (federal/sum(active)), 3),
    mail.turnout.ratio = round( (prec.early.turnout / prec.mail.turnout ), 1),
    pct.active.reg  =  round( 100 * (active/sum(active)), 1),
    `Percent.Mail.Counted`  =  round( 100 * (counted / mail.received), 1),
    date     = lubridate::as_date(ymd("2020-10-29"))
  )

# check math!
assert_that( sum(vote.data29$mail.outstanding.pct) >= 99.5 & sum(vote.data29$mail.outstanding.pct) <= 100.5)
assert_that( sum(vote.data29$early.voted.pct) >= 99.5 & sum(vote.data29$early.voted.pct) <= 100.5)
assert_that( sum(vote.data29$mail.received.pct) >= 99.5 & sum(vote.data29$mail.received.pct) <= 100.5)
assert_that( sum(vote.data29$prec.tot.votes.pct) >= 99.5 & sum(vote.data29$prec.tot.votes.pct) <= 100.5)
assert_that( sum( (vote.data29$prec.early.turnout + vote.data29$prec.mail.turnout) -
                    vote.data29$prec.act.turnout > 0.15) == 0)


# Change Variable Names ----
vote.data29 <- vote.data29 %>%
  rename(
    Precinct = precinct,
    `Precinct.Name` = prec_name,
    `Mail.Outstanding` = mail.outstanding,
    `Mail.Received` = mail.received,
    `Early.Voted` = early.voted,
    `Active.Registered` = active,
    `Inactive.Registered` = inactive,
    `All.Registered` = all,
    Military = military,
    Overseas = overseas,
    Federal = federal,
    # `VA House District` = house,
    # `VA Senate District` = senate,
    # `Polling Place` = polling_pl,
    `Total.Votes` = prec.tot.votes,
    `Outstanding.Votes` = prec.outstand.votes,
    `Percent.Outstanding.Votes` = outstand.votes.pct,
    `Mail.Ballots.Requested` = mail.request,
    `Precinct.Share.of.All.Votes` =  prec.tot.votes.pct,
    `Mail.Ballot.Return.Rate` = mail.return.pct,
    `Precinct.Share.of.Early.Votes` = early.voted.pct,
    `Precinct.Share.of.Oustanding.Mail.Votes` = mail.outstanding.pct,
    `Precinct.Share.of.Mail.Ballots.Received` = mail.received.pct,
    `Early.to.Mail.Ratio` = mail.early.ratio,
    Turnout = prec.cur.turnout,
    `Active.Turnout` = prec.act.turnout,
    `Turnout.by.Mail` = prec.mail.turnout,
    `Turnout.by.Early.Voting` = prec.early.turnout,
    `Early.to.Mail.Turnout.Ratio` =  mail.turnout.ratio,
    `Mail.Ballots.Counted`  = counted
  )


# create a total/Arlington Row 
## first make a 2-row tibble with names of the main vote dataset
arl <- tibble(
  Precinct = 0,
  `Precinct.Name` = "Arlington Totals",
  `Mail.Outstanding` = sum(vote.data29$`Mail.Outstanding`),
  `Mail.Received` = sum(vote.data29$`Mail.Received`),
  `Early.Voted` = sum(vote.data29$`Early.Voted`),
  `Active.Registered` = sum(vote.data29$`Active.Registered`),
  `Inactive.Registered` = sum(vote.data29$`Inactive.Registered`),
  `All.Registered` = sum(vote.data29$`All.Registered`),
  Military = sum(vote.data29$Military),
  Overseas = sum(vote.data29$Overseas),
  Federal = sum(vote.data29$Federal),
  `Total.Votes` = sum(vote.data29$`Total.Votes`),
  `Outstanding.Votes` = sum(vote.data29$`Outstanding.Votes`),
  `Percent.Outstanding.Votes` = sum(vote.data29$`Percent.Outstanding.Votes`),
  `Mail.Ballots.Requested` = sum(vote.data29$`Mail.Ballots.Requested`),
  `Percent.Mail.Counted`  =  round( 100 *(sum(vote.data29$`Mail.Ballots.Counted`) / sum(vote.data29$`Mail.Received`)), 1),
  `Precinct.Share.of.All.Votes` =  100, # must be 100
  `Mail.Ballot.Return.Rate` = round( (sum(vote.data29$`Mail.Received`) /
                                        sum(vote.data29$`Mail.Ballots.Requested`)), 2),
  `Precinct.Share.of.Early.Votes` = 100, # will be 100
  `Precinct.Share.of.Oustanding.Mail.Votes` = 100, # will be 100
  `Precinct.Share.of.Mail.Ballots.Received` = 100, # will be 100
  `Early.to.Mail.Ratio` = round((`Early.Voted` / `Mail.Received` ), 1),
  Turnout = round( 100 * (`Total.Votes`/`All.Registered`), 1),
  `Active.Turnout` = round( 100 * (`Total.Votes`/`Active.Registered`), 1),
  `Turnout.by.Mail` = round( 100 * (`Mail.Received`/`Active.Registered`), 1),
  `Turnout.by.Early.Voting` = round( 100 * (`Early.Voted`/`Active.Registered`), 1),
  `Early.to.Mail.Turnout.Ratio` =  round((`Turnout.by.Early.Voting`/`Turnout.by.Mail`),1),
  `Mail.Ballots.Counted`  = sum(vote.data29$`Mail.Ballots.Counted`),
  date      = lubridate::as_date(ymd("2020-10-29"))
)

## append arl totals to vote.data 
vote.data29 <- bind_rows(vote.data29, arl)






# run check script ----
# sum of Total.Votes == given sum of Total.Votes
assert_that(arl$`Total.Votes`[1] == totals.tib$total.voted[1])

# sum of Early.Voted == given sum of early votes
assert_that(arl$`Early.Voted`[1] == totals.tib$early.voted[1]) 

# sum of total mail == given sum of total Mail.Ballots.Requested 
assert_that(sum(arl$`Mail.Ballots.Requested`[1] + e.totmail29) == totals.tib$total.mail[1]) # 101 undercount

# sum of total counted == given sum of total counted 
assert_that(sum(arl$`Mail.Ballots.Counted`[1]) == totals.tib$counted[1])

# sum of total Mail.Received == given sum of received + given sum of counted 
assert_that( sum(arl$`Mail.Received`[1]) == (totals.tib$received[1] + totals.tib$counted[1] ) )

# sum of total outstanding mail votes == given sum of "outstanding" (or "mailed" as imported)
assert_that((sum(arl$`Mail.Outstanding`[1]) + e.outstanding29) == totals.tib$mailed[1]) # 101 off undercount

# assert that there are 54 precincts
assert_that(nrow(vote.data29) == nrow )



# remove the un-needed objects 
rm(raw.info, raw1, raw2, pg2.split, totals.tib, pg2.tib, pg1.split, pg1.tib, arl)

# save ----
 saveRDS(vote.data29,
         file.path(votes, "29-oct.Rda"))