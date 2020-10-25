# ---  --- --- --- --- --- --- --- --- --- --- --- --- #
# votes-9-oct.R
# imports and processes aggregated vote info from Arlington for 18th october
# ---  --- --- --- --- --- --- --- --- --- --- --- --- #




# load + process latest voting data file ---- 
# page info 
raw.info <- pdf_info(file.path(votes, "arl-votes-10-9.pdf"))

# import both pages
raw1 <- pdf_text(file.path(votes, "arl-votes-10-9.pdf"))

# page 2
raw2 <- raw1[[2]]
pg2.split <- str_split(raw2, "\\n", simplify = TRUE) %>% as.vector()



# make locality totals
totals.tib <- tibble(RawText=pg2.split) %>%
  filter(row_number() == 11) %>%
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
  filter(row_number() <= max2) %>%
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
  filter(row_number() <= max1) %>%
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
vote.9oct <- bind_rows(pg1.tib, pg2.tib) %>%
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
vote.9oct <- 
  vote.9oct %>%
  convert(num(numvars1),
          fct(fctvars1))

if (s.gis == 1) {
  arl.precinct <- 
    arl.precinct %>%
    convert(num(numvars2),
            fct(fctvars2))
}


vote.register <-
  vote.register %>%
  convert(num(numvars3),
          fct(fctvars3))


# Merge
vote.data9 <-
  left_join(vote.9oct, vote.register,
            by = "precinct",
            suffix = c('.x', '.y')) %>%
  mutate(
    prec_name = coalesce(prec_name.x, prec_name.y) 
  ) %>%
  select(-prec_name.x, -prec_name.y)

if (s.gis == 1) {
  vote.data9 <- vote.data9 %>%
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
vote.data9 <-
  vote.data9 %>%
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
    `Percent Mail Counted`  =  round( 100 * (counted / mail.received), 1)
  )

# check math!
assert_that( sum(vote.data9$mail.outstanding.pct) >= 99.5 & sum(vote.data9$mail.outstanding.pct) <= 100.5)
assert_that( sum(vote.data9$early.voted.pct) >= 99.5 & sum(vote.data9$early.voted.pct) <= 100.5)
assert_that( sum(vote.data9$mail.received.pct) >= 99.5 & sum(vote.data9$mail.received.pct) <= 100.5)
assert_that( sum(vote.data9$prec.tot.votes.pct) >= 99.5 & sum(vote.data9$prec.tot.votes.pct) <= 100.5)
assert_that( sum( (vote.data9$prec.early.turnout + vote.data9$prec.mail.turnout) -
                    vote.data9$prec.act.turnout > 0.15) == 0)

# create tibble of voting methods 
vote.methods <- vote.data9 %>%
  # st_drop_geometry() %>%
  select(mail.received, early.voted) %>%
  summarise(
    Mail = sum(mail.received),
    Early= sum(early.voted)
  ) %>%
  gather("Method", "Votes") %>%
  mutate(
    Percent = round( 100 * (Votes / sum(Votes)), 1)
  )


# create voter turnout object 
vote.turnout <- vote.data9 %>%
  # st_drop_geometry() %>%
  select(prec.tot.votes, active, all, mail.received, early.voted) %>%
  summarise(
    `Cumulative Turnout`         = round( 100 * sum(prec.tot.votes) / sum(all), 2),
    `Cumulative Active Turnout` = round( 100 * sum(prec.tot.votes) / sum(active), 2),
    `Cumulative Mail Turnout`    = round( 100 * sum(mail.received) / sum(all), 2),
    `Cumulative Active Mail Turnout` = round( 100 * sum(mail.received) / sum(active), 2),
    `Cumulative Early Turnout`    = round( 100 * sum(early.voted) / sum(all), 2),
    `Cumulative Active Early Turnout` = round( 100 * sum(early.voted) / sum(active), 2),
    tot.votes                     = sum(prec.tot.votes)
  )



# Change Variable Names ----
vote.data9 <- vote.data9 %>%
  rename(
    Precinct = precinct,
    `Precinct Name` = prec_name,
    `Mail Outstanding` = mail.outstanding,
    `Mail Received` = mail.received,
    `Early Voted` = early.voted,
    `Active Registered` = active,
    `Inactive Registered` = inactive,
    `All Registered` = all,
    Military = military,
    Overseas = overseas,
    Federal = federal,
    # `VA House District` = house,
    # `VA Senate District` = senate,
    # `Polling Place` = polling_pl,
    `Total Votes` = prec.tot.votes,
    `Outstanding Votes` = prec.outstand.votes,
    `Percent Outstanding Votes` = outstand.votes.pct,
    `Mail Ballots Requested` = mail.request,
    `Precinct Share of All Votes` =  prec.tot.votes.pct,
    `Mail Ballot Return Rate` = mail.return.pct,
    `Precinct Share of Early Votes` = early.voted.pct,
    `Precinct Share of Oustanding Mail Votes` = mail.outstanding.pct,
    `Precinct Share of Mail Ballots Received` = mail.received.pct,
    `Early to Mail Ratio` = mail.early.ratio,
    Turnout = prec.cur.turnout,
    `Active Turnout` = prec.act.turnout,
    `Turnout by Mail` = prec.mail.turnout,
    `Turnout by Early Voting` = prec.early.turnout,
    `Early to Mail Turnout Ratio` =  mail.turnout.ratio,
    `Mail Ballots Counted`  = counted
  )









# Create Scatterpolar objects ----


# create arlington average 
scatterpolar <- vote.data9 %>%
  # st_drop_geometry() %>%
  select(polarvars) %>%
  as.data.frame() 

scatterpolar$`Precinct Name` <-  as.character(scatterpolar$`Precinct Name`)

# create averages 


# c("Precinct Name", "Active Turnout", 
#   "Mail Ballot Return Rate", "Early to Mail Ratio" , "Mail Ballots Requested",
#   "Outstanding Votes", "Total Votes" )

scatterpolar <- scatterpolar %>%
  rbind( c( "Arlington Average",
            round(mean(scatterpolar$`Active Turnout`), 0),
            round(mean(scatterpolar$`Mail Ballot Return Rate`), 2), 
            round(mean(scatterpolar$`Early to Mail Ratio`), 1),
            round(mean(scatterpolar$`Mail Ballots Requested`), 0), 
            round(mean(scatterpolar$`Outstanding Votes`), 0 ),
            round(mean(scatterpolar$`Total Votes`), 0) )) %>%
  convert(num(c("Active Turnout", "Mail Ballot Return Rate", "Early to Mail Ratio", "Mail Ballots Requested", 
                "Outstanding Votes", "Total Votes"))) 

arlav  <- scatterpolar %>% 
  filter(`Precinct Name` == "Arlington Average") %>% 
  select(-`Precinct Name`) %>%
  as.vector() %>%
  gather()



# create normalized variables 
sp.norm <- scatterpolar %>%
  mutate(across(where(is.numeric),
                ~round(normalize(.x), 2)))






# run check script ----
# sum of total votes == given sum of total votes
assert_that(sum(vote.data9$`Total Votes`) == totals.tib$total.voted[1])

# sum of early voted == given sum of early votes
assert_that(sum(vote.data9$`Early Voted`) == totals.tib$early.voted[1]) 

# sum of total mail == given sum of total mail ballots requested 
assert_that((sum(vote.data9$total.mail)+ e.totmail9) == totals.tib$total.mail[1]) # 101 undercount

# sum of total counted == given sum of total counted 
assert_that(sum(vote.data9$`Mail Ballots Counted`) == totals.tib$counted[1])

# sum of total mail received == given sum of received + given sum of counted 
assert_that( sum(vote.data9$`Mail Received`) == (totals.tib$received[1] + totals.tib$counted[1] ) )

# sum of total outstanding mail votes == given sum of "outstanding" (or "mailed" as imported)
assert_that((sum(vote.data9$`Mail Outstanding`) + e.outstanding9) == totals.tib$mailed[1]) # 101 off undercount

# assert that there are 54 precincts
assert_that(nrow(vote.data9) == nrow )



# remove the un-needed objects 
rm(raw.info, raw1, raw2, pg2.split, totals.tib, pg2.tib, pg1.split, pg1.tib)



# save ----
saveRDS(vote.data9,
        file.path(votes, "9-oct.Rda"))