# ---  --- --- --- --- --- --- --- --- --- --- --- --- #
# import.R
# imports and processes shapefiles
# ---  --- --- --- --- --- --- --- --- --- --- --- --- #


polarvars <- c("Precinct Name", "Active Turnout", 
               "Mail Ballot Return Rate", "Early to Mail Ratio" , "Mail Ballots Requested",
               "Outstanding Votes", "Total Votes" )



          
          # load shapfile data as sf---- 
        
arl.precinct <-
  st_read(
    file.path(shp)
    )

# make varnames all lowercase
names(arl.precinct) <- str_to_lower(names(arl.precinct))

# combine two Precinct boundaries 
  # issue is that in the shapefiles these are listed seperately because they have different 
  # house districts but in the vote data they are counted as one.
jef2 <- arl.precinct %>% filter(prec_name == "Jefferson")
jef1 <- st_combine(jef2) # combined geometry

# replace Jefferson precinct geometry split by two house districts with singular jefferson precinct geometry 
arl.precinct$geometry[arl.precinct$prec_name == "Jefferson" & arl.precinct$house == 47 ] <- jef1
arl.precinct$geometry[arl.precinct$prec_name == "Jefferson" & arl.precinct$house == 49 ] <- jef1

# drop duplicates by generating random number that will be same every time 
set.seed(seed = 47, kind = NULL)
arl.precinct <- arl.precinct %>%
  arrange(objectid) %>%
  mutate(
    rand = runif(length(arl.precinct$objectid)),
    dup  = duplicated(prec_name)
  ) %>%
filter( (dup==FALSE) == TRUE)

# check that there are 54 precincts
assert_that(nrow(arl.precinct) == 54)


# replace house variable with two house precincts 
arl.precinct$house[arl.precinct$prec_name == "Jefferson" ] <- "47/49"



# issue: there are two entries for "jefferson", change the name for different house dist.
# arl.precinct$prec_name[arl.precinct$prec_name == "Jefferson" & arl.precinct$house == 47 ] <- "Jefferson (H47)"
# arl.precinct$prec_name[arl.precinct$prec_name == "Jefferson" & arl.precinct$house == 49 ] <- "Jefferson (H49)"






          # load + process voting registeration file  ---- 

#page info 
reg.raw.info <- pdf_info(register)

# import both pages and split 
reg.raw1 <- pdf_text(register)
reg.raw2 <- reg.raw1[[2]]
reg.raw1 <- reg.raw1[[1]]


# look <- function(rx) str_view_all(reg.pg2.tib$Rest, rx)
# look.res <- look("(?<=[:alpha:])[:space:](?=[:digit:])")

# page 2

reg.pg2.split <- str_split(reg.raw2, "\\n", simplify = TRUE) %>% as.vector()

reg.pg2.tib <- tibble(RawText=reg.pg2.split) %>%
  filter(row_number() < 26) %>%
  filter(row_number() > 4) %>%
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)", "\\.")) %>%
  separate(TextSquish, into=c('precinct.no.long', 'Rest'), sep = "[:space:]",
           extra="merge") %>%
  mutate(Rest=str_squish(Rest),
         Rest=str_replace_all(Rest, "\\(X\\)", "NA")) %>%
  separate(Rest,
           into=c('precinct', 'Rest'), 
           sep = "-",
           extra='merge') %>%
  separate(Rest,
           into=c('prec_name', 'Rest'), 
           sep = "(?<=[:alpha:])[:space:](?=[:digit:])",
           extra='merge') %>%
  separate(Rest,
           into=c('active', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('inactive', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('all', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('military', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('overseas', 'federal'), 
           sep = "[:space:]",
           extra='merge') 

# page 1 

reg.pg1.split <- str_split(reg.raw1, "\\n", simplify = TRUE) %>% as.vector()

reg.pg1.tib <- tibble(RawText=reg.pg1.split) %>% 
  filter(row_number() < 40) %>%
  filter(row_number() > 6) %>%
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)", "\\.")) %>%
  separate(TextSquish, into=c('precinct.no.long', 'Rest'), sep = "[:space:]",
           extra="merge") %>%
  mutate(Rest=str_squish(Rest),
         Rest=str_replace_all(Rest, "\\(X\\)", "NA")) %>%
  separate(Rest,
           into=c('precinct', 'Rest'), 
           sep = "-",
           extra='merge') %>%
  separate(Rest,
           into=c('prec_name', 'Rest'), 
           sep = "(?<=[:alpha:])[:space:](?=[:digit:])",
           extra='merge') %>%
  separate(Rest,
           into=c('active', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('inactive', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('all', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('military', 'Rest'), 
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('overseas', 'federal'), 
           sep = "[:space:]",
           extra='merge') 




# append 
vote.register <- 
  bind_rows(reg.pg1.tib, reg.pg2.tib) %>%
  select(-RawText, -precinct.no.long) %>%
  mutate(
    prec_name = str_to_title(prec_name)
  )

# substitute out comma
vote.register$all <-  gsub(",", "", vote.register$all)
vote.register$active <-  gsub(",", "", vote.register$active)
vote.register$inactive <-  gsub(",", "", vote.register$inactive)
vote.register$military <-  gsub(",", "", vote.register$military)
vote.register$overseas <-  gsub(",", "", vote.register$overseas)
vote.register$federal <-  gsub(",", "", vote.register$federal)










          # load + process latest voting data file ---- 
# page info 
raw.info <- pdf_info(latest)

# import both pages
raw1 <- pdf_text(latest)

# page 2
raw2 <- raw1[[2]]
pg2.split <- str_split(raw2, "\\n", simplify = TRUE) %>% as.vector()



# make locality totals
totals.tib <- tibble(RawText=pg2.split) %>%
  filter(row_number() == totalsrow) %>%
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

arl.precinct <- 
  arl.precinct %>%
  convert(num(numvars2),
         fct(fctvars2))

vote.register <-
  vote.register %>%
  convert(num(numvars3),
          fct(fctvars3))


# Merge
vote.data <-
  left_join(vote.9oct, vote.register,
            by = "precinct",
            suffix = c('.x', '.y')) %>%
  mutate(
    prec_name = coalesce(prec_name.x, prec_name.y) 
    ) %>%
  select(-prec_name.x, -prec_name.y) %>%
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








            # quick data mutations 
vote.data <-
  vote.data %>%
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
assert_that( sum(vote.data$mail.outstanding.pct) >= 99.5 & sum(vote.data$mail.outstanding.pct) <= 100.5)
assert_that( sum(vote.data$early.voted.pct) >= 99.5 & sum(vote.data$early.voted.pct) <= 100.5)
assert_that( sum(vote.data$mail.received.pct) >= 99.5 & sum(vote.data$mail.received.pct) <= 100.5)
assert_that( sum(vote.data$prec.tot.votes.pct) >= 99.5 & sum(vote.data$prec.tot.votes.pct) <= 100.5)
assert_that( sum( (vote.data$prec.early.turnout + vote.data$prec.mail.turnout) -
                    vote.data$prec.act.turnout > 0.15) == 0)

# create tibble of voting methods 
vote.methods <- vote.data %>%
  st_drop_geometry() %>%
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
vote.turnout <- vote.data %>%
  st_drop_geometry() %>%
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
vote.data <- vote.data %>%
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
   `VA House District` = house,
   `VA Senate District` = senate,
   `Polling Place` = polling_pl,
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
scatterpolar <- vote.data %>%
  st_drop_geometry() %>%
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

## merge 
# scatterpolar <- scatterpolar %>%
#   left_join(., sp.norm,
#             by = "Precinct Name", 
#             suffix = c(".raw", ".norm"))






            # run check script ----
source(file.path(root.code, "assert.R"))




            # export and save ----
if (export == 1) {
  # save local copy
save(
  vote.data,
  vote.methods,
  vote.turnout,
  sp.norm,
  file = file.path(root.data, "rdata/arl-vote2020.Rdata")
)
  
  #also save copy to app 
save(
  vote.data,
  vote.methods,
  vote.turnout,
  sp.norm,
  file = file.path(app, "arl-vote2020.Rdata")
)
  
  
}